# Z_UI2_YAML Deferral Resolution (Phase A) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Resolve the seven v1 deferrals — build #1 (camelCase inverse deserialize), #3 (block-header anchors), #4 (multi-doc streams); decline #2/#5/#6/#7 as logged decisions.

**Architecture:** Additive changes to the existing `Z_UI2_YAML` class (branch `feat/z_ui2_yaml`, HEAD `eea66a4`, 65 tests pristine). #1 extends `lcl_typed_mapper` key-matching; #3 extends the scanner block-header + parser anchor registration; #4 adds a document splitter + two new public methods over the existing single-doc path. No rewrite of working code except the minimal scanner `---` change #4 needs.

**Tech Stack:** ABAP (SAP_BASIS 7.57+), abapGit format, ABAP Unit, RTTI. Test system: **ER1** via sap-adt MCP.

## Global Constraints

- **Min SAP_BASIS 7.57+.** Modern ABAP.
- **Self-contained** — no Z_UI2_JSON dependency. Structural errors → `cx_sy_conversion_no_number`; strict type mismatch → `cx_sy_move_cast_error`.
- **PRISTINE test output** — no compiler warnings (use `substring( val = off = )` not `x+off`; PCRE or `##REGEX_POSIX` pragma; no redundant CONV; no unused vars).
- **Perf-neutral when option unused** — #1's inverse transform guarded behind `pretty_name <> none`, only after a direct match fails.
- **Static API narrow** — new methods are top-level (`GENERATE_ALL`/`DESERIALIZE_ALL`), not switches on existing ones. Existing `DESERIALIZE`/`GENERATE` behavior unchanged (first doc only).
- **In-system test cycle:** every "run test" = `SaveClassInclude`/`SaveClassTestInclude` → `ActivateObject` → `RunAbapUnit` on ER1. Read ONLY the summary line from RunAbapUnit (never echo the full XML — it is ~25KB and will blow the output-token limit). Commit to git after each green task; mirror ER1 source into `src/`.
- **OUTPUT DISCIPLINE for implementers:** keep streamed responses terse; RunAbapUnit summary lines only; full detail in the report file.

## Current-state facts (authoritative — from v1 build)

- Node contract: `ty_node_ref = REF TO lcl_node_ref`; `lcl_node_ref` has public `node TYPE ty_node` (kind char via `c_node=>scalar/mapping/sequence`, `value`, `is_null`) + `children TYPE ty_children` (direct table; `ty_child` = `key` + `node`).
- `ty_line` fields: `lineno`, `indent`, `content`, `doc_marker` (set on `...`), `blk_scalar_hd`, `blk_value`. `---` lines are currently DROPPED by `lcl_scanner=>scan`.
- Parser: `lcl_parser=>parse( lines ) RETURNING ty_node_ref`; helpers `parse_block`/`parse_mapping`/`parse_sequence`/`parse_seq_item`/`value_or_block`/`split_key_value`/`resolve_scalar`/`parse_flow`/`strip_anchor`/`resolve_alias`. Anchor table is a CLASS-DATA on `lcl_parser`, cleared at `parse()` entry.
- Emitter: `lcl_emitter=>emit`; forward pretty_name transform (MY_FIELD→myField etc.) lives here — reuse its word-boundary logic (inverted) for #1.
- Typed mapper: `lcl_typed_mapper=>map( node, pretty_name, name_mappings, strict CHANGING data )`; struct branch matches components by name_mappings + case-insensitive uppercase. Bool detection by `absolute_name CP` bool-type set.
- Public class `z_ui2_yaml`: static `serialize`/`deserialize`/`generate` + `serialize_int`/`deserialize_int` + CONSTRUCTOR storing `mv_pretty_name`/`mt_name_mappings`/`mv_strict`/`mv_indent`/`mv_quote_style`/`mv_emit_doc_markers`/`mv_flow_threshold`. `pretty_mode` constants: none/low_case/camel_case/pascal_case/extended.
- Tests: `src/z_ui2_yaml.clas.testclasses.abap`, 65 methods across 11 test classes (ltc_smoke, ltc_scanner, ltc_tree, ltc_parser, ltc_scalar, ltc_block_scalar, ltc_anchor, ltc_deser, ltc_gen, ltc_ser, ltc_fixtures).

## File Structure

All changes in existing files:
- `src/z_ui2_yaml.clas.abap` — new public methods (#4), new `ref_tab` type.
- `src/z_ui2_yaml.clas.locals_def.abap` — new `ty_line` field(s) (#3, #4); new local method signatures (#1 inverse helper, #4 splitter).
- `src/z_ui2_yaml.clas.locals_imp.abap` — impls: #1 inverse match, #3 scanner+parser anchor, #4 splitter + generate_all/deserialize_all internals.
- `src/z_ui2_yaml.clas.testclasses.abap` — new tests per task.
- `docs/feature-requests-yaml.md`, `docs/yaml.md`, `CLAUDE.md` — Task 4 (docs/decisions).

---

### Task 1: #1 — camelCase/PascalCase inverse on DESERIALIZE

**Files:**
- Modify: `src/z_ui2_yaml.clas.locals_def.abap` (add private `pretty_inverse` method to `lcl_typed_mapper`)
- Modify: `src/z_ui2_yaml.clas.locals_imp.abap` (`lcl_typed_mapper` struct-branch match + `pretty_inverse`)
- Test: `src/z_ui2_yaml.clas.testclasses.abap` (ltc_deser additions)

**Interfaces:**
- Consumes: `lcl_typed_mapper=>map` existing struct branch; `z_ui2_yaml=>pretty_mode` constants.
- Produces:
  ```abap
  CLASS-METHODS pretty_inverse
    IMPORTING yaml_key TYPE string pretty_name TYPE z_ui2_yaml=>pretty_name_mode
    RETURNING VALUE(abap_name) TYPE string.
  ```
  Returns the ABAP component name for a YAML key under a pretty_name mode: camel/extended `myField`→`MYFIELD`... wait — the ABAP component is `MY_FIELD`. Transform: insert `_` before each interior uppercase letter, then upper-case the whole. `myField`→`MY_FIELD`; pascal `MyField`→`MY_FIELD`; `low_case`/`none`→`TO_UPPER( yaml_key )` (no underscore insertion — none/low have no camel boundaries). Only called after a direct match fails and `pretty_name <> none`.

- [ ] **Step 1: Write the failing tests** (ltc_deser additions)

```abap
METHOD camel_inverse_deser.
  TYPES: BEGIN OF ty, my_field TYPE string, another_one TYPE i, END OF ty.
  DATA out TYPE ty.
  DATA(o) = NEW z_ui2_yaml( pretty_name = z_ui2_yaml=>pretty_mode-camel_case ).
  o->deserialize_int( EXPORTING yaml = |myField: hello\nanotherOne: 5| CHANGING data = out ).
  cl_abap_unit_assert=>assert_equals( act = out-my_field exp = `hello` ).
  cl_abap_unit_assert=>assert_equals( act = out-another_one exp = 5 ).
ENDMETHOD.
METHOD pascal_inverse_deser.
  TYPES: BEGIN OF ty, my_field TYPE string, END OF ty.
  DATA out TYPE ty.
  DATA(o) = NEW z_ui2_yaml( pretty_name = z_ui2_yaml=>pretty_mode-pascal_case ).
  o->deserialize_int( EXPORTING yaml = |MyField: hi| CHANGING data = out ).
  cl_abap_unit_assert=>assert_equals( act = out-my_field exp = `hi` ).
ENDMETHOD.
METHOD camel_round_trip.
  TYPES: BEGIN OF ty, my_field TYPE string, port_num TYPE i, END OF ty.
  DATA(in) = VALUE ty( my_field = `x` port_num = 9 ).
  DATA out TYPE ty.
  DATA(o) = NEW z_ui2_yaml( pretty_name = z_ui2_yaml=>pretty_mode-camel_case ).
  o->deserialize_int( EXPORTING yaml = o->serialize_int( in ) CHANGING data = out ).
  cl_abap_unit_assert=>assert_equals( act = out exp = in ).
ENDMETHOD.
```
Add the method declarations to ltc_deser DEFINITION.

- [ ] **Step 2: Save test include, activate, RunAbapUnit — verify FAIL (RED).** Summary line only. Expected: the 3 new fail (keys don't match camelCase → fields stay initial).

- [ ] **Step 3: Implement `pretty_inverse` + wire into the struct match.** In `lcl_typed_mapper=>map` struct branch: after the existing name_mappings + `TO_UPPER` match yields no component AND `pretty_name <> pretty_mode-none`, compute `pretty_inverse( yaml_key = child-key pretty_name = pretty_name )` and retry the component lookup with that name. `pretty_inverse`: walk chars, before each interior char that IS uppercase (offset>0) append `_`, then `TO_UPPER` the result; for none/low_case just `TO_UPPER`.

- [ ] **Step 4: Save, activate, RunAbapUnit — verify PASS (GREEN), all 68, pristine.** Summary only.

- [ ] **Step 5: Commit**

```bash
cd "c:/Users/D038062/Documents/GitHub/abap-to-json"
git add src/z_ui2_yaml.clas.locals_def.abap src/z_ui2_yaml.clas.locals_imp.abap src/z_ui2_yaml.clas.testclasses.abap
git commit -m "feat(z_ui2_yaml): camelCase/PascalCase inverse key mapping on DESERIALIZE"
```

---

### Task 2: #3 — block-header anchors on read

**Files:**
- Modify: `src/z_ui2_yaml.clas.locals_def.abap` (add `blk_anchor TYPE string` to `ty_line`)
- Modify: `src/z_ui2_yaml.clas.locals_imp.abap` (scanner block-header: strip `&name`; parser: register on blk_scalar_hd consume)
- Test: `src/z_ui2_yaml.clas.testclasses.abap` (ltc_anchor additions)

**Interfaces:**
- Consumes: scanner block-header handling; `lcl_parser` anchor table + registration path (Task 7 v1); `blk_scalar_hd`/`blk_value` mechanism.
- Produces: `ty_line-blk_anchor` carries the anchor name parsed off a block-scalar header line (empty if none). Parser registers it after building the block-scalar node.

- [ ] **Step 1: Write the failing tests** (ltc_anchor additions)

```abap
METHOD block_header_anchor.
  " anchor on a block-scalar header, then alias it
  DATA(r) = lcl_parser=>parse( lcl_scanner=>scan( |a: &blk \|\n  line1\n  line2\nb: *blk| ) ).
  DATA(av) = r->children[ 1 ]-node->node-value.
  cl_abap_unit_assert=>assert_equals( act = av exp = |line1\nline2\n| ).
  " alias b resolves to the same block value
  cl_abap_unit_assert=>assert_equals( act = r->children[ 2 ]-node->node-value exp = |line1\nline2\n| ).
  " shared reference
  cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node exp = r->children[ 2 ]-node ).
ENDMETHOD.
```
Add `block_header_anchor FOR TESTING RAISING cx_sy_conversion_error` to ltc_anchor DEFINITION. NOTE: the `\|` in the template is an escaped pipe; verify the scanner sees `a: &blk |`.

- [ ] **Step 2: Save, activate, RunAbapUnit — verify FAIL (RED).** Summary only. Expected: alias `*blk` → undefined-alias raise (anchor never registered because block header bypassed value_or_block).

- [ ] **Step 3: Implement.**
  - Add `blk_anchor TYPE string` to `ty_line` in locals_def.
  - Scanner: when parsing a block-scalar header line, after isolating the `key:` prefix and before the `|`/`>` indicator, detect a leading `&name` token in the remainder; if present, strip it and set `line-blk_anchor = name`. (Reuse the same anchor-name validation as `strip_anchor`: `&` + `CO 'A-Za-z0-9_-'`.)
  - Parser `parse_mapping`, in the `blk_scalar_hd = abap_true` branch: after `lv_child = lcl_tree=>new_scalar( value = cur-blk_value )`, if `cur-blk_anchor IS NOT INITIAL`, register `cur-blk_anchor → lv_child` in the anchor table (same insert the inline path uses).

- [ ] **Step 4: Save, activate, RunAbapUnit — verify PASS (GREEN), all 69, pristine.** Summary only. Confirm prior block-scalar + anchor tests still green.

- [ ] **Step 5: Commit**

```bash
cd "c:/Users/D038062/Documents/GitHub/abap-to-json"
git add src/z_ui2_yaml.clas.locals_def.abap src/z_ui2_yaml.clas.locals_imp.abap src/z_ui2_yaml.clas.testclasses.abap
git commit -m "feat(z_ui2_yaml): anchors on block-scalar header lines"
```

---

### Task 3: #4 — multi-document streams (GENERATE_ALL + DESERIALIZE_ALL)

**Files:**
- Modify: `src/z_ui2_yaml.clas.abap` (public `generate_all`, `deserialize_all` + `deserialize_all_int`; `ref_tab` type)
- Modify: `src/z_ui2_yaml.clas.locals_def.abap` (`doc_start` flag on `ty_line`; `split_documents` on `lcl_parser` or a helper)
- Modify: `src/z_ui2_yaml.clas.locals_imp.abap` (scanner keep `---` as `doc_start`; single-doc `parse` skips leading `doc_start`; `split_documents`; generate_all/deserialize_all internals)
- Test: `src/z_ui2_yaml.clas.testclasses.abap` (new ltc_multidoc)

**Interfaces:**
- Consumes: `lcl_scanner=>scan`, `lcl_parser=>parse`, `lcl_gen_mapper=>generate`, `lcl_typed_mapper=>map`.
- Produces:
  ```abap
  " public class
  TYPES ref_tab TYPE STANDARD TABLE OF REF TO data WITH DEFAULT KEY.
  CLASS-METHODS generate_all IMPORTING yaml TYPE string RETURNING VALUE(rt_data) TYPE ref_tab.
  CLASS-METHODS deserialize_all IMPORTING yaml TYPE string
                                          pretty_name TYPE pretty_name_mode DEFAULT pretty_mode-none
                                          name_mappings TYPE name_mappings OPTIONAL
                                CHANGING results TYPE STANDARD TABLE.
  METHODS deserialize_all_int IMPORTING yaml TYPE string CHANGING results TYPE STANDARD TABLE
                              RAISING cx_sy_move_cast_error.
  " local
  CLASS-METHODS split_documents IMPORTING lines TYPE ty_lines RETURNING VALUE(rt_blocks) TYPE ... (table of ty_lines).
  ```
  `split_documents` partitions the line stream at `doc_start` (`---`) boundaries into a table of `ty_lines` (one per document); `...` (`doc_marker`) ends the current document. A leading `---` does not create an empty leading doc. `generate_all`: scan → split → for each block, `parse` + `lcl_gen_mapper=>generate`, APPEND ref. `deserialize_all_int`: scan → split → for each block, create a `results` line workarea (via RTTI of the table's line type), `parse` + `lcl_typed_mapper=>map`, APPEND row. Structural errors propagate; static `generate_all`/`deserialize_all` mirror the narrow signature and instantiate+delegate.

- [ ] **Step 1: Write the failing tests** (new ltc_multidoc class)

```abap
CLASS ltc_multidoc DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS generate_all_three FOR TESTING.
    METHODS deserialize_all_typed FOR TESTING.
    METHODS single_doc_degenerate FOR TESTING.
    METHODS legacy_first_doc_only FOR TESTING.
ENDCLASS.
CLASS ltc_multidoc IMPLEMENTATION.
  METHOD generate_all_three.
    DATA(y) = |a: 1\n---\na: 2\n---\na: 3|.
    DATA(rt) = z_ui2_yaml=>generate_all( y ).
    cl_abap_unit_assert=>assert_equals( act = lines( rt ) exp = 3 ).
    FIELD-SYMBOLS <s> TYPE any. ASSIGN rt[ 2 ]->* TO <s>.
    FIELD-SYMBOLS <f> TYPE any. ASSIGN COMPONENT `A` OF STRUCTURE <s> TO <f>.
    cl_abap_unit_assert=>assert_equals( act = <f> exp = 2 ).
  ENDMETHOD.
  METHOD deserialize_all_typed.
    TYPES: BEGIN OF ty, name TYPE string, port TYPE i, END OF ty.
    DATA results TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
    DATA(y) = |name: a\nport: 1\n---\nname: b\nport: 2|.
    z_ui2_yaml=>deserialize_all( EXPORTING yaml = y CHANGING results = results ).
    cl_abap_unit_assert=>assert_equals( act = lines( results ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = results[ 2 ]-name exp = `b` ).
    cl_abap_unit_assert=>assert_equals( act = results[ 2 ]-port exp = 2 ).
  ENDMETHOD.
  METHOD single_doc_degenerate.
    DATA(rt) = z_ui2_yaml=>generate_all( |a: 1| ).
    cl_abap_unit_assert=>assert_equals( act = lines( rt ) exp = 1 ).
  ENDMETHOD.
  METHOD legacy_first_doc_only.
    " existing DESERIALIZE on multi-doc returns doc 1 only (backward compat)
    TYPES: BEGIN OF ty, a TYPE i, END OF ty.
    DATA out TYPE ty.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |a: 1\n---\na: 2| CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out-a exp = 1 ).
  ENDMETHOD.
ENDCLASS.
```

- [ ] **Step 2: Save, activate, RunAbapUnit — verify FAIL (RED).** Summary only. Expected: generate_all/deserialize_all undefined; legacy_first_doc_only may already pass or error depending on current `---` handling — note which.

- [ ] **Step 3: Implement.**
  - `ty_line`: add `doc_start TYPE abap_bool`. Scanner: instead of dropping `---`, emit a line with `doc_start = abap_true` (content empty, indent 0). Keep `...` → `doc_marker` as now.
  - Single-doc `parse`: skip a leading `doc_start` line (so existing single-doc parse of `---\na: 1` still works and existing `DESERIALIZE`/`GENERATE` see the first doc only — process up to the first *subsequent* `doc_start`/`doc_marker` and ignore the rest, preserving current first-doc behavior).
  - `split_documents( lines ) RETURNING table of ty_lines`: iterate; a `doc_start` closes the current block (if non-empty) and starts a new one; `doc_marker` closes the current block; collect non-boundary lines into the current block. Drop empty blocks.
  - Public `generate_all`: scan → split_documents → loop blocks: `lcl_parser=>parse( block )` → `lcl_gen_mapper=>generate` → APPEND to rt_data.
  - `deserialize_all_int`: scan → split → loop: build a line workarea from `results`' RTTI line type (CREATE DATA), `parse` + `map` into it, APPEND. static `deserialize_all` wraps (instantiate with pretty_name/name_mappings, TRY/CATCH cast error like `deserialize`).
  - Add `ref_tab` type to public class.

- [ ] **Step 4: Save, activate, RunAbapUnit — verify PASS (GREEN), all 73, pristine.** Summary only. CRITICAL regression check: all prior parser/scanner/deser/gen tests still green — confirm the scanner `---` change didn't break single-doc parsing (esp. any existing test using `---`).

- [ ] **Step 5: Commit**

```bash
cd "c:/Users/D038062/Documents/GitHub/abap-to-json"
git add src/z_ui2_yaml.clas.abap src/z_ui2_yaml.clas.locals_def.abap src/z_ui2_yaml.clas.locals_imp.abap src/z_ui2_yaml.clas.testclasses.abap
git commit -m "feat(z_ui2_yaml): multi-document streams (GENERATE_ALL + DESERIALIZE_ALL)"
```

---

### Task 4: Decline decisions + docs

**Files:**
- Modify: `CLAUDE.md` (API Design Decisions: log #2/#5/#6 declines + #7 default-case decision)
- Modify: `docs/feature-requests-yaml.md` (move built items out; add Declined section)
- Modify: `docs/yaml.md` (document GENERATE_ALL/DESERIALIZE_ALL; camelCase deserialize now supported; block-header anchors supported; prominent low_case-for-lowercase-keys note; declined items as limitations)

**Interfaces:**
- Consumes: nothing (docs only). No ABAP, no tests.

- [ ] **Step 1: Update CLAUDE.md** — add to "API Design Decisions" (durable, one paragraph each):
  - `(Z_UI2_YAML) Anchor emission on write — declined.` Config producers emit expanded YAML; anchor emission needs cycle detection + dedup heuristic. Read-side anchors supported.
  - `(Z_UI2_YAML) YAML tags — declined.` `!!str`/`!<uri>` rare in config; RTTI target type already determines interpretation. Silent-ignore stays.
  - `(Z_UI2_YAML) Explicit block-scalar indent indicator (|N) — declined.` Auto-detection from first body line covers real content.
  - `(Z_UI2_YAML) Default key case stays pretty_mode-none (uppercase passthrough).` Consistency with Z_UI2_JSON; changing is breaking. Users wanting conventional lowercase config keys pass `pretty_name = pretty_mode-low_case` (documented in yaml.md).

- [ ] **Step 2: Update docs/feature-requests-yaml.md** — remove #1/#3/#4 from deferrals (now built); add a "## Declined (see CLAUDE.md)" section listing #2/#5/#6/#7 with a one-line reason + pointer.

- [ ] **Step 3: Update docs/yaml.md** — add GENERATE_ALL/DESERIALIZE_ALL usage; note camelCase/Pascal deserialize now round-trips; note block-header anchors supported on read; add a prominent "Producing conventional lowercase keys → use pretty_name = pretty_mode-low_case (default none preserves ABAP uppercase)" callout; list declined items (tags/anchor-emission/|N) under Limitations.

- [ ] **Step 4: Commit**

```bash
cd "c:/Users/D038062/Documents/GitHub/abap-to-json"
git add CLAUDE.md docs/feature-requests-yaml.md docs/yaml.md
git commit -m "docs(z_ui2_yaml): log declined deferrals + document new multi-doc/camelCase/anchor features"
```

---

## Self-Review

**Spec coverage:**
- #1 camelCase inverse deserialize → Task 1 ✓
- #3 block-header anchors → Task 2 ✓
- #4 multi-doc GENERATE_ALL + DESERIALIZE_ALL → Task 3 ✓
- #2/#5/#6 declines + #7 default-case decision → Task 4 (CLAUDE.md) ✓
- yaml.md/feature-requests-yaml.md updates → Task 4 ✓
- Perf-neutral (#1 guarded behind pretty_name<>none) ✓; static API narrow (new top-level methods) ✓; backward compat (existing DESERIALIZE/GENERATE first-doc-only) ✓ Task 3 Step 4 regression check.

**Placeholder scan:** none — all steps have concrete code or concrete doc content.

**Type consistency:** `pretty_inverse` (Task 1), `blk_anchor`/`doc_start` ty_line fields (Tasks 2/3), `ref_tab`/`generate_all`/`deserialize_all`/`deserialize_all_int`/`split_documents` (Task 3) named consistently across interface + steps. Node contract (`->node-value`/`->children`) consistent with v1.

**Ordering:** #1, #3, #4 independent enough; #3 and #4 both touch `ty_line` + scanner — sequence them (Task 2 before Task 3) so the second rebases on the first's committed state. Task 4 (docs) last, after all behavior is final.

**Risk note:** Task 3's scanner `---` change is the only modification to working v1 behavior — Step 4's regression check across all prior tests is the guard.
