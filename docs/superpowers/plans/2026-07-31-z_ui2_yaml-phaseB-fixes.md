# Z_UI2_YAML Phase B — Review Fixes & Optimization Plan

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:subagent-driven-development. Steps use checkbox syntax.

**Goal:** Apply the Phase B findings — fix 5 correctness bugs, consolidate duplication (reuse), and polish maintainability — on the complete Z_UI2_YAML class without changing documented behavior (except the two approved fixes: CRLF normalization, null-keyword recognition).

**Architecture:** Three sequential tasks on branch `feat/z_ui2_yaml` (HEAD `bb411ce`, 74 tests pristine). Task 1 = correctness (TDD, behavior changes where approved). Task 2 = reuse consolidation (behavior-preserving). Task 3 = maintainability polish (behavior-preserving). Findings sourced from `c:/tmp/yaml-phaseB-{correctness,reuse,maintainability}.md`.

**Tech Stack:** ABAP 7.57+, ER1 via sap-adt MCP, ABAP Unit.

## Global Constraints
- SAP_BASIS 7.57+, self-contained (no Z_UI2_JSON), PRISTINE tests, structural errors → `cx_sy_conversion_no_number`, strict → `cx_sy_move_cast_error`, perf-neutral, static API narrow.
- In-system TDD: SaveClassInclude/TestInclude → ActivateObject → RunAbapUnit on ER1. **RunAbapUnit summary line ONLY — never echo full XML (32k output-limit trap).** Commit after each green task; mirror ER1 source into `src/`.
- Behavior-preserving tasks (2, 3): all 74 existing tests must stay green with NO test changes (proves no behavior drift). New tests only where a task adds/fixes behavior.

## Current-state facts
Node: `ty_node_ref = REF TO lcl_node_ref` (node + children). Locals: lcl_scanner/lcl_parser/lcl_typed_mapper/lcl_gen_mapper/lcl_emitter/lcl_tree in locals_imp; types + c_node in locals_def. Public class z_ui2_yaml. 74 tests across 11 ltc_ classes.

---

### Task 1: Correctness fixes (C1–C5)

**Files:** `src/z_ui2_yaml.clas.abap`, `src/z_ui2_yaml.clas.locals_imp.abap`, `src/z_ui2_yaml.clas.testclasses.abap`

Five distinct fixes, each with a RED test first:

- **C1 — static DESERIALIZE must not dump on structural errors.** `deserialize`/`deserialize_all` (clas.abap ~157/213) catch only `cx_sy_move_cast_error`, but scan/parse raise `cx_sy_conversion_no_number`. Per the documented "lenient" contract, static DESERIALIZE must NOT dump. Fix: in static `deserialize`/`deserialize_all`, also `CATCH cx_sy_conversion_error` (parent of the structural subclass) and treat as lenient no-op (leave data as-is). NOTE: keep `deserialize_int` (instance) propagating structural errors — the fix is on the STATIC lenient face only. Decide + document: does lenient static swallow structural silently, or leave data initial? → swallow (return with data untouched), matching "lenient" = best-effort. Test: `deserialize( yaml = |a:\n\tb: 1| CHANGING out )` does NOT dump (out stays initial); a bad-dedent + unterminated-quote likewise.
- **C2 — CRLF normalization.** `scan` splits on `|\n|`, leaving trailing `\r`. Fix: after the split (or in trim_right), strip a single trailing `\r` (0x0D) from each raw line before processing. Handles CRLF + mixed. Test: `scan(|port: 8080\r\nname: web|)` → line 1 content `port: 8080` (no CR), and a typed deser of CRLF input gives port=8080 (not 0).
- **C3 — negative packed decimal trailing sign.** emitter numeric branch: `TYPE p` negative emits `3.14-`. Fix: after converting the numeric elem to string, detect a trailing `-` and move it to the front (`3.14-` → `-3.14`). Apply only to the packed/numeric path; leave i/decfloat (already leading-sign) untouched. Test: struct field `TYPE p DECIMALS 2 = '-3.14'` serializes to `...: -3.14`.
- **C4 — recognize null/Null/NULL.** `resolve_scalar`: currently only `~`/empty → is_null. Add: unquoted value exactly `null`/`Null`/`NULL` → is_null=true, value=``. (Quoted `"null"` stays the string — resolve_scalar handles quotes before this check.) Test: `deserialize(|a: null|)` into a string field → initial/empty; `generate(|a: null|)` → null handling; a quoted `|a: "null"|` stays string `null`.
- **C5 — indent_block hardcoded n=2.** emitter table-row branch (locals_imp ~1329) uses `indent_block( n = 2 )` ignoring `indent_step`. Fix: `n = indent_step`. Test: an instance with `indent = 4` serializing a table-of-struct → continuation lines indented by 4 (assert the emitted string, or round-trip with indent=4).

- [ ] **Step 1:** Write 5 RED tests (add to relevant ltc_ classes or a new ltc_phaseb). Save test include → activate → RunAbapUnit → confirm the new ones RED (summary only).
- [ ] **Step 2:** Implement C1–C5. Save → activate → RunAbapUnit → GREEN, all (74 + new), pristine.
- [ ] **Step 3:** Mirror src → commit `fix(z_ui2_yaml): structural-error leniency, CRLF, packed sign, null keyword, indent_step`.

---

### Task 2: Reuse consolidation (behavior-preserving)

**Files:** `src/z_ui2_yaml.clas.locals_def.abap`, `src/z_ui2_yaml.clas.locals_imp.abap` (NO test changes — all 74+ must stay green unchanged)

Apply reuse findings #1,#2,#3,#4,#6,#7,#8,#9 (skip #5 scan/strip_anchor cross-class merge = MED/optional, skip #10 quote-state-machine = HIGH/declined):
- **#1** Extract `is_bool_type( eld ) RETURNING abap_bool` (the `CP *ABAP_BOOL*...` set) into a shared local (e.g. lcl_tree or a small util); call from lcl_typed_mapper + lcl_emitter. One source of truth.
- **#2** `CONSTANTS c_yaml_name_chars` in locals_def; replace the 3 verbatim 62-char CO literals.
- **#3** Collapse camel/pascal loops in `format_key` into one (initial-cap flag from pretty_name). Delete the `2`-suffixed clone (~18 lines).
- **#4** Extract `is_date_like( value ) RETURNING abap_bool`; call from emitter quote_scalar + gen_mapper detect_scalar_type.
- **#6** Sequence double-generation in gen_mapper: collect child refs in the uniformity pass, reuse in fill (halve RTTI allocs). Behavior identical.
- **#7** `split_documents` always returns ≥1 block; remove the duplicated degenerate branches in generate_all + deserialize_all_int.
- **#8** Inline single-caller `looks_like_number` into quote_scalar; delete the method decl.
- **#9** `trim_right` strlen micro-optimization (compute length once, decrement).

- [ ] **Step 1:** Apply all consolidations. Save def+impl → activate → RunAbapUnit → all 74+ GREEN with NO test changes (this is the behavior-preservation proof), pristine.
- [ ] **Step 2:** Mirror src → commit `refactor(z_ui2_yaml): consolidate bool/date/name-char/case-loop duplication`.

---

### Task 3: Maintainability polish (behavior-preserving)

**Files:** `src/z_ui2_yaml.clas.abap`, `src/z_ui2_yaml.clas.locals_def.abap`, `src/z_ui2_yaml.clas.locals_imp.abap` (NO test behavior changes — 74+ stay green)

- **quote_mode constants:** add `CONSTANTS: BEGIN OF quote_mode, plain VALUE 'P', always_double VALUE 'D', END OF quote_mode.` to public class; replace `'P'`/`'D'` magic chars in constructor + quote_scalar. (Keeps quote_style TYPE c; just names the values.)
- **Rename cryptic vars:** in scan (`rr`→lv_raw_line, `nn`→lv_next_n, `bbrr`→lv_body_raw, `bsl`→lv_stripped_ln, `blk_ind`→blk_scalar_ind to disambiguate from blk_ind_col); in parse_flow (`tsq`→in_sq, `tdq`→in_dq, `tdepth`→depth, `fc`→lv_colon_found). Match the in_sq/in_dq convention used elsewhere.
- **Extract scan() helpers:** pull block-body collection + block-value assembly into 2 private class-methods (`collect_block_body`, `assemble_block_value`) so scan drops from ~283 to ~80 lines. Behavior identical.
- **Dead code / polish:** delete unused `lv_last` (indent_block ~1577); delete empty `PROTECTED SECTION` (clas.abap); `c_max_comp_name_len TYPE i VALUE 30` constant in gen_mapper (replace the 2 magic 30s); move `aname` decl before its IF for clarity; drop redundant `strlen>0` guard after `IS NOT INITIAL`.
- **Comment:** upgrade the anchor-table ponytail comment to note class-data is not thread-safe (concurrent parse() corrupts anchor state).

- [ ] **Step 1:** Apply polish. Save all → activate → RunAbapUnit → all 74+ GREEN, NO test changes, pristine.
- [ ] **Step 2:** Mirror src → commit `refactor(z_ui2_yaml): quote_mode constants, clearer names, split scan(), dead-code`.

---

## Deliberately NOT done (logged, not fixed)
- Reuse #10 (triple quote-state-machine extraction) — HIGH risk, parse_flow too interleaved; not worth it.
- Reuse #5 (scan re-implements strip_anchor cross-class) — MED, optional; skipped to keep the diff safe.
- Correctness #5 (flow-on-own-line) + #6 (strict kind-mismatch) — exotic/low-value for config; note as accepted limitations if worth a doc line.

## Self-Review
- Coverage: 5 correctness (C1–C5) → Task 1; reuse #1-4,6-9 → Task 2; maintainability meaningful+nitpick → Task 3. ✓
- Behavior-preservation guard: Tasks 2 & 3 forbid test changes, so 74 green = no drift. ✓
- Approved behavior changes (CRLF, null) isolated to Task 1 with new tests. ✓
- Perf baseline (Z_UI2_YAML_PERF create) remains a manual step — not in this plan. ✓
