# Z_UI2_JSON — Deep Performance Refactor Spec

**Purpose:** analyze where V1's intrinsic cost lives and spec candidate refactors *with an up-front analytical win estimate*, so we spend the 137s measurement runs only on candidates that could plausibly clear the ">5% valuable" bar. Any change ships ONLY if measured against the `Z_UI2_JSON_PERF` baseline it beats it without breaking a unit test.

See `kb/perf-optimization-findings.md` for already-rejected experiments (A: inline join — neutral/O(n²); B: double field read — no headroom; C: escape re-scan — risky, low yield).

## Cost model (what actually runs per node)

**Serialize** (`serialize_int` → `dump_int` → `dump_symbols` → `dump_type` macro):
- RTTI/symbol metadata: **cached** per struct type in `mt_struct_cache`. Not per-row. ✓ already optimal.
- Per row: `dump_symbols` loops fields → `dump_type_int` macro (value→string, kernel `escape` for strings) → build `lt_fields` string table → `CONCATENATE LINES … SEPARATED BY ','` → wrap in `{…}`.
- Per table: parent concatenates the per-row `{…}` fragments (same table+join pattern one level up).

**Deserialize** (`deserialize_int` → `restore_type` → `restore`):
- Field metadata: `get_fields` called **once per table** (line ~2208), passed as `field_cache` to each row. Not per-row. ✓
- `fields = field_cache` copy in `restore` is copy-on-write (read-only in the row path) → no real copy. ✓
- Per value: character scan (`eat_name`/`eat_number`/`eat_white` = `find_any_of`/`find_any_not_of`), then MOVE into target. Kernel-level primitives.

**Conclusion:** the metadata layers are already cached. Remaining cost is (a) per-node value conversion (kernel calls — not beatable in ABAP) and (b) string assembly / character scanning. Only (b) has any structural headroom, and experiment A showed the obvious rewrite is neutral.

## Candidate D — remove `INITIAL SIZE 10000` from `dump_symbols` local table  ★ most promising

- **Location:** `z_ui2_json.clas.abap:1002` — `DATA lt_fields TYPE STANDARD TABLE OF string INITIAL SIZE 10000`.
- **Observation:** `dump_symbols` holds ONE struct's fields (typically 5-40), but is called **once per serialized struct** — 20,000 times for SBOOK. `INITIAL SIZE 10000` forces the kernel to reserve an initial block sized for 10000 rows on every call, of which ~15 are used. Classic "large INITIAL SIZE on a hot local table" anti-pattern.
- **Analytical win estimate:** allocation churn only, not algorithmic. Plausibly a few % on wide-table serialize (SBOOK 20k, AllTypes 10k); likely negligible on small/deep. Uncertain — depends on whether the kernel lazily allocates. **Cheap enough to measure directly** (one-word change, zero correctness risk).
- **Change:** drop `INITIAL SIZE 10000` (let it default-grow) or set a small realistic hint (e.g. none).
- **Risk:** none — semantics identical.
- **Status:** TO MEASURE.

## Candidate E — leaf-level single rope buffer (eliminate per-struct joins)

- **Idea:** instead of each `dump_symbols` building a table + joining, and the parent re-concatenating fragments, thread ONE shared `string_table` buffer through the recursion; every leaf appends its `"name":value` piece; a single `CONCATENATE LINES` runs once at the very top.
- **Analytical win estimate:** questionable. This is essentially what the kernel `IF_JSON_WRITER` (V2) does, and V2 serialize is measured **7-35% SLOWER** than V1 (writer call overhead). A pure-ABAP rope avoids the method-call overhead but still trades the current O(fields) per-struct join for append bookkeeping + separator logic. Experiment A (a partial form of this) was neutral. **Estimated ≤ break-even; not worth the large, risky refactor.**
- **Risk:** high — touches the entire serialize recursion + comma/brace placement + pretty-print indent.
- **Status:** REJECTED analytically (would only re-confirm A at larger scope). Revisit only if D + profiling show the join is the true hotspot.

## Candidate F — deserialize: hoist `eat_white` calls

- **Idea:** `restore`/`restore_type` call `eat_white` before/after every token. Some are provably redundant (e.g. right after `eat_char ':'` the scanner is already positioned).
- **Analytical win estimate:** `eat_white` = `find_any_not_of(... off = offset)` — a single kernel scan that returns immediately on non-whitespace (the common case, since compress output has no spaces). Cost ≈ one kernel call per skipped-nothing. Removing a few is < 1-2% and risks breaking whitespace-tolerant parsing (a documented feature — `deserialize_white_space` test). **Low yield, real risk.**
- **Status:** REJECTED analytically.

## Recommendation

Measure **Candidate D only** (cheap, zero-risk, plausibly a few %). Everything else is analytically at or below break-even for large refactor risk. If D lands ≥5% on a wide-table scenario → ship it. If neutral → document and stop; V1 is at the ABAP ceiling for the current design, and further gains require a fundamentally different representation (e.g. writing straight to an xstring buffer) that is out of scope for incremental tuning and would duplicate the V2 kernel-writer effort.
