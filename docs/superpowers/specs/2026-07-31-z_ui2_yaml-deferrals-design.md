# Z_UI2_YAML — Deferral Resolution Spec (Phase A)

**Date:** 2026-07-31
**Status:** Design approved — ready for implementation plan
**Branch:** feat/z_ui2_yaml (continues the v1 work)
**Related:** [2026-07-30-z_ui2_yaml-design.md](2026-07-30-z_ui2_yaml-design.md), [docs/feature-requests-yaml.md](../../../docs/feature-requests-yaml.md)

## Purpose

Resolve the seven documented v1 deferrals: BUILD three, DECLINE four (three as logged decisions + one as a doc-only decision). Keep the class lean before the Phase B optimization pass.

## Build set

### #1 — camelCase/PascalCase inverse on DESERIALIZE

**Problem:** `SERIALIZE(pretty_name = camel_case)` emits `myField`, but DESERIALIZE only does a case-insensitive uppercase match, so `myField` won't map back to component `MY_FIELD`. Round-trip is asymmetric.

**Fix:** In `lcl_typed_mapper=>map` (struct branch), when the existing name_mappings + case-insensitive-uppercase match fails to find a component, apply the **inverse** pretty_name transform to the YAML key and retry:
- `camel_case` / `extended`: `myField` → `MY_FIELD` (insert `_` before each interior uppercase letter, upper-case all).
- `pascal_case`: `MyField` → `MY_FIELD` (same, first letter also boundary).
- `low_case`: already covered by case-insensitive match.
- `none`: no transform (current behavior).

**Perf-neutrality:** guard the inverse transform behind `pretty_name <> pretty_mode-none` at the top of component resolution — the default path adds zero per-field cost. The transform only runs when a direct match already failed AND a non-none pretty_name is active.

**Tests:** camelCase round-trip (`SERIALIZE(camel)` → `DESERIALIZE(camel)` equals original) into a struct with a multi-word component; PascalCase likewise.

### #4 — Multi-document streams: `GENERATE_ALL` + `DESERIALIZE_ALL`

**Problem:** `---`-separated multi-doc YAML (K8s-style) yields only the first document.

**Design:** The scanner already flags `doc_marker` (`...`) and drops `---`. Change: the scanner (or a thin splitter above the parser) must expose document boundaries. Simplest approach that preserves existing behavior — a helper `split_documents( lines ) RETURNING tt_line_blocks` that partitions the `ty_lines` stream at `---` markers into N sub-streams (a `---` starts a new document; `...` ends the current one). Then:

- `CLASS-METHODS generate_all IMPORTING yaml TYPE string RETURNING VALUE(rt_data) TYPE ref_tab` where `ref_tab` = `STANDARD TABLE OF REF TO data` — one entry per document, each built via the existing `lcl_gen_mapper=>generate` on that document's node tree. Untyped, heterogeneous doc shapes OK. Structural errors propagate (consistent with `generate`).
- `METHODS deserialize_all IMPORTING yaml TYPE string CHANGING results TYPE STANDARD TABLE` (+ static wrapper) — `results` is a caller-provided table of a uniform line type; for each document, create a line workarea, map via the existing `lcl_typed_mapper=>map`, APPEND. Assumes all docs share the table's line type (documented). Static wrapper mirrors `deserialize`'s narrow signature (yaml + results + pretty_name + name_mappings).

**Backward compatibility:** existing `DESERIALIZE` / `GENERATE` unchanged — they process the **first** document only (current behavior; the splitter's first block). Both new methods are thin loops over the existing single-doc path — no new parsing logic, only stream partitioning + iteration.

**Scanner impact:** currently `---` lines are dropped. The splitter needs to SEE them. Options: (a) scanner keeps `---` as a flagged boundary line rather than dropping it, and single-doc `parse` ignores a leading boundary; (b) a separate `scan_documents` that returns `tt_line_blocks`. Prefer (a) minimal: add a `doc_start` flag to `ty_line`, keep the line, and have single-doc `parse` skip a leading `doc_start`. `split_documents` partitions on `doc_start`/`doc_marker`.

**`ref_tab` type:** reuse a `TYPES ref_tab TYPE STANDARD TABLE OF REF TO data WITH DEFAULT KEY` on the public class (mirrors Z_UI2_JSON's `ref_tab`).

**Tests:** GENERATE_ALL on a 3-doc stream → 3 refs, spot-check field in doc 2 and 3; DESERIALIZE_ALL of a 2-doc uniform stream into a typed table → 2 rows with correct values; single-doc input through GENERATE_ALL → 1 entry (degenerate case); existing DESERIALIZE on multi-doc input → still returns doc 1 only (backward-compat guard).

### #3 — Block-header anchors on read (`key: &a |`)

**Problem:** The scanner resolves block scalars and sets `blk_value` + `blk_scalar_hd`, bypassing `value_or_block` where anchor registration lives — so an anchor on a block-scalar header line is lost.

**Fix:** In the scanner's block-header handling, detect and strip a leading `&name` from the header remainder (the text between `key:` and the `|`/`>` indicator), and stash the anchor name on the line (new `ty_line` field `blk_anchor TYPE string`, or reuse a general mechanism). In `parse_mapping`, when consuming a `blk_scalar_hd` line, after building the scalar node, if `blk_anchor` is set, register it in the anchor table (same registration path Task 7 uses). Alias resolution is unchanged (aliases to a block-anchored node already work once registered).

**Tests:** `k: &a |` + body, then `k2: *a` → both resolve to the same block-scalar value; the anchored node is registered.

## Decline set (log as decisions, no code)

Each gets a durable entry in CLAUDE.md "API Design Decisions", and feature-requests-yaml.md moves them to a "Declined" section pointing at CLAUDE.md.

- **#2 Anchor emission on write** — Declined. Requires cycle detection + a dedup heuristic to decide what to anchor; a config *producer* emits fully-expanded YAML, which is more readable for humans editing config. Read-side anchor support stays. Reopen only on concrete demand.
- **#5 Tags (`!!str`, `!<uri>`)** — Declined. Rare in hand-written config; the target ABAP type already determines interpretation (RTTI on deserialize, char-check on generate), so a tag would be redundant or conflicting. Current silent-ignore stays.
- **#6 Explicit block-scalar indent indicator (`|2`, `>4`)** — Declined. Auto-detection from the first non-blank body line's indent covers essentially all real content; the explicit form exists for rare ambiguous cases (leading-space content) that config rarely needs. Reopen on demand.
- **#7 Default key case** — Keep `pretty_mode-none` (uppercase passthrough) as the default, for consistency with Z_UI2_JSON's established convention. Changing the default would be a breaking change for existing callers. Resolution is **documentation**: yaml.md gets a prominent "Producing conventional lowercase config keys" note steering users to `pretty_name = pretty_mode-low_case`. No code change.

## Constraints (unchanged from v1)

- SAP_BASIS 7.57+, modern ABAP, self-contained (no Z_UI2_JSON dependency), PRISTINE test output.
- Structural errors → `cx_sy_conversion_no_number`; strict type mismatch → `cx_sy_move_cast_error`.
- New feature work perf-neutral when the relevant option is unused (guard at top-level, not per-node).
- Static API stays narrow; `DESERIALIZE_ALL`/`GENERATE_ALL` are new top-level methods (not switches on existing ones).
- Tests run in-system via `RunAbapUnit` on ER1; commit to git after each green task; mirror ER1 source into `src/`.

## Out of scope

Phase B (full class review + reuse/simplicity/maintainability optimization) is a separate effort after Phase A merges clean.
