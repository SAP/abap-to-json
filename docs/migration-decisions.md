# Migration Decisions Log — Z_UI2_JSON → Kernel JSON API

This file records all architectural decisions, trade-offs, and deliberate modifications made during the migration of `Z_UI2_JSON` from manual character-level parsing to `IF_JSON_READER` / `IF_JSON_WRITER`.

---

## Context

- **Source**: `z_ui2_json` VERSION 23 (`src/z_ui2_json.clas.abap`)
- **Target**: `z_ui2_json2` VERSION 1 (`src/z_ui2_json2.clas.abap`) — independent class, coexists with V23
- **Minimum SAP_BASIS**: 7.57 (stable `SJSON` package with `IF_JSON_READER`, `IF_JSON_WRITER`, `CL_JSON_STRING_READER`, `CL_JSON_STRING_WRITER`)
- **Deployment**: abapGit (no CI/CD, tests run in SAP system via SE80/ADT)
- **Coexistence**: Both classes maintained long-term. V23 gets bug fixes only. V1 gets all new development.

---

## Decision 1 — Target SAP_BASIS Version

**Decision**: Minimum SAP_BASIS 7.57.

**Rationale**: The `SJSON` package (containing `IF_JSON_READER`, `IF_JSON_WRITER`, `CL_JSON_STRING_READER`, `CL_JSON_STRING_WRITER`) is stable from 7.57. The previous minimum was 7.31. This is a deliberate break — the new class targets modern systems. Users on older systems keep the old `Z_UI2_JSON` (VERSION 23).

---

## Decision 2 — Raw JSON Passthrough (e_typekind-json)

**Decision**: Use `lcl_util=>read_json_to_string( reader )` for raw JSON passthrough in `RESTORE_TYPE`. This manually walks the tree with depth tracking, writing to a fresh `cl_json_string_writer`.

**Rationale**: `skip_node(writer)` does NOT work correctly when the reader is positioned on an object member mid-document — confirmed by testing. The manual tree-walking workaround is encapsulated in a single utility method. When `skip_node(writer)` is fixed by the IF_JSON_READER author, this method becomes a one-line replacement.

**Impact**: `RESTORE_TYPE` for `e_typekind-json` calls `lcl_util=>read_json_to_string( reader )` for complex values, or `reader->node-value` for primitives.

---

## Decision 3 — FORMAT_OUTPUT Output Difference Accepted

**Decision**: The output of pretty-printed JSON (FORMAT_OUTPUT / `mv_format_output = true`) will differ from the old implementation. No attempt is made to match the old CRLF+2-space indentation exactly.

**Rationale**: `IF_JSON_WRITER` has its own indentation logic controlled by `set_option( option_linebreaks )` and `set_option( option_indent )`. Replicating the exact old format would require post-processing the writer output, adding complexity for no functional benefit. FORMAT_OUTPUT is a display feature, not a contract.

**Impact**: The test `serialize_associative_array` sub-test for FORMAT_OUTPUT has hardcoded CRLF+2-space indent. This test expectation must be updated to match the new writer output.

---

## Decision 4 — Public API Methods Removed

The following public static methods are removed from the new class (were deprecated or subsumed):

| Method | Reason |
|--------|--------|
| `DUMP` | Thin wrapper over `SERIALIZE`; callers use `SERIALIZE` |
| `BOOL_TO_TRIBOOL` | Utility unrelated to JSON; callers can inline |
| `TRIBOOL_TO_BOOL` | Same |
| `UNESCAPE` | No longer needed — `IF_JSON_READER` returns unescaped values natively |
| `GET_INDENT` | Only served pretty-printing; `IF_JSON_WRITER` handles indentation internally |
| `GET_CONVEXIT_FUNC` | Moved to `lcl_util=>get_convexit_func()` (private) |
| `EDM_DATETIME_TO_TS` | Moved to `lcl_util=>read_edm_datetime()` (was already there); public wrapper removed |
| `ESCAPE` | Callers can use `escape()` BIF directly; no longer a meaningful wrapper |

**Rationale**: The new class exposes less surface. Methods that existed only as compatibility shims or internal helpers are privatized. The user confirmed these can be dropped since this is a new class running in parallel with the old one.

---

## Decision 5 — Protected/Private Method Signature Changes

**`RESTORE` / `RESTORE_TYPE`**:
- Old signature: `EXPORTING json TYPE string, length TYPE i, offset TYPE i, type_descr, ...`
- New signature: `EXPORTING reader TYPE REF TO if_json_reader, type_descr, ...` (no json/length/offset)
- **Rationale**: The reader maintains its own position state. Passing offset/length was the manual parsing approach. All callers are internal — no public contract broken.

**`DUMP_TYPE`** (virtual method for subclass override):
- Old signature: `IMPORTING data TYPE data, type_descr TYPE REF TO cl_abap_elemdescr, typekind, convexit RETURNING r_json TYPE json`
- New signature: `IMPORTING data TYPE data, type_descr TYPE REF TO cl_abap_elemdescr, typekind, convexit, writer TYPE REF TO if_json_writer`
- **Rationale**: Writer-based serialization cannot return a string fragment — it must write to the shared writer. Adding `writer` as import parameter enables subclass overrides to write directly.

**`DUMP_INT` / `DUMP_SYMBOLS`**:
- Old: accumulate string fragments in internal table, CONCATENATE at end
- New: accept `writer TYPE REF TO if_json_writer` and write directly
- **Rationale**: Eliminates intermediate string allocation.

---

## Decision 6 — New Private Method GENERATE_INT_R

**Decision**: Introduce `GENERATE_INT_R( reader TYPE REF TO if_json_reader ) RETURNING VALUE(result) TYPE REF TO data` as the reader-based workhorse for `GENERATE_INT`.

**Rationale**: `GENERATE_INT` currently takes `json/length/offset` and builds ABAP data from unknown-type JSON. With the reader API, position is reader-managed. `GENERATE_INT_R` is the clean internal implementation; `GENERATE_INT` becomes a thin adapter that creates a reader and delegates.

---

## Decision 7 — Macro Strategy

**Decision**: Keep the `dump_type_int` macro approach. Rewrite macro body to emit writer calls instead of string concatenation.

**Rationale**: ABAP does not support method inlining. The macro is used for the fast-path (when `mv_extended IS INITIAL`) to avoid virtual method call overhead on every field. The `dump_type` dispatch macro already handles the `mv_extended` runtime check. Macros stay; their bodies change.

**Specific change**: `dump_type_int` parameters `&1`=data, `&2`=typekind, `&3`=result-string, `&4`=convexit will change to `&1`=data, `&2`=typekind, `&3`=writer, `&4`=convexit. The `&3` parameter is now a writer reference, not an output string. All callsites updated accordingly.

**Macros to remove**:
- `escape_json` — no longer needed; writer escapes automatically
- `xstring_to_string_int` / `string_to_xstring_int` — no longer needed for escaping (base64 path uses `cl_http_utility=>encode_base64` directly)

**Macros to keep** (body changes only):
- `dump_type_int` — core serialization, rewritten for writer
- `dump_type` — dispatch macro, keep as-is (logic unchanged)
- `is_compressable` — keep as-is
- `format_name` — keep as-is
- `format_list_output` — remove (no longer needed with writer approach)
- `restore_reference` / `restore_reference_ex` — rewrite for reader
- `throw_error` — keep as-is (exception raise)
- `while_offset_cs` / `while_offset_not_cs` / `eat_white` / `eat_name` / `eat_number` / `eat_bool` / `eat_char` — **remove** (all manual parsing; replaced by reader API)
- `create_regexp` — keep as-is (still used for ISO8601/OData regex)

---

## Decision 8 — escape_json Removal

**Decision**: Remove the `escape_json` macro entirely.

**Rationale**: `IF_JSON_WRITER` handles all string escaping internally. The macro was a compatibility shim around `escape( format = cl_abap_format=>e_json_string )`. With writer calls like `write_string( value )`, no explicit escaping is needed.

---

## Decision 9 — lcl_util Expansion

**Decision**: Move the following from `Z_UI2_JSON` class-data/methods to `lcl_util`:

| Item | Direction |
|------|-----------|
| `so_regex_unescape_spec_char` | Remove (no longer needed) |
| `get_convexit_func` static method | Move to `lcl_util` |
| `so_regex_iso8601` | Already in `lcl_util` — keep |
| `so_regex_edm_date_time` | Already in `lcl_util` — keep |
| RTTI descriptor caches for UTCLONG/ENUM types | Move to `lcl_util` class-data |

**Rationale**: Reduces the public/protected surface of `Z_UI2_JSON`. `lcl_util` is a `FRIENDS z_ui2_json` class, so access is unrestricted internally.

---

## Decision 10 — Dynamic Calls

**Decision**: Replace dynamic calls for UTCLONG and ENUM typekinds with direct static calls where possible.

**Context**: Dynamic calls (`CALL METHOD ... TYPE (typename)`) were used to avoid syntax errors on older BASIS. Since minimum is now 7.57, `utclong` and `enum` types can be handled with direct statements.

**Exception**: `CREATE OBJECT TYPE (mc_me_type)` — this is intentional polymorphism (the class instantiating itself vs. a subclass). Keep as-is.

**Rationale**: Dynamic calls add overhead and obscure intent. Static calls are cleaner and allow syntax checking.

---

## Decision 11 — mv_initial_ts / mv_initial_date / mv_initial_time Quoting

**Decision**: These instance variables store their initial-value strings WITH surrounding double-quotes (e.g., `mv_initial_ts = '"0001-01-01T00:00:00Z"'`). When passing to `writer->write_string()`, the quotes must be stripped.

**Rationale**: The old code emitted these directly into string concatenation. With writer API, `write_string` adds quotes automatically. Stripping: `mid( mv_initial_ts, 2, strlen(mv_initial_ts) - 2 )`.

---

## Decision 12 — symbol-header Storage Format

**Decision**: The `symbol-header` field in `mt_struct_cache` stores the plain ABAP field name (e.g., `FIELD_NAME`), not the JSON-formatted name. Pretty-name formatting is applied at serialization time.

**Rationale**: This was already the case in VERSION 23. The reader approach uses `reader->node-name` which is the JSON key; lookup maps JSON key → ABAP field. Keep consistent.

---

## Decision 13 — Patch Sync (VERSION 22–23 → New Class)

The following fixes from VERSION 22 and 23 (unreleased PL22 in `docs/history.md`) must be ported into the new class:

1. **EDM DateTime rounding fix**: `read_edm_datetime` must TRUNCATE subseconds to milliseconds, not round. `psubsec = pticks MOD 1000` is correct; ensure no rounding arithmetic is introduced.

2. **Packed field length 8 false-positive**: In `get_struct_info` / type classification, a packed field with length 8 (e.g., `p LENGTH 8`) must not be misidentified as a timestamp. The check must be `type_kind EQ typekind_packed AND length EQ 8 AND decimals EQ 14` — if decimals ≠ 14, it is not a timestamp.

3. **Generate struct duplicate name**: In `GENERATE_STRUCT`, when a JSON object has duplicate member names, the second occurrence must not overwrite the first in the generated ABAP structure reference cache. Use `INSERT ... ACCEPTING DUPLICATES` or check before inserting.

---

## Decision 14 — Test Updates Required

Tests that will need expected-output updates due to format changes:

| Test Method | Reason |
|-------------|--------|
| `serialize_associative_array` (FORMAT_OUTPUT sub-test) | New indentation format from `IF_JSON_WRITER` |
| Any test checking exact whitespace in pretty-printed output | Same |

Tests that must continue to pass unchanged:
- All `serialize_*` tests (field order, value format, number serialization, boolean, null)
- All `deserialize_*` tests
- All `roundtrip_*` tests
- `deserialize_malformed` — must not crash, must return gracefully

---

## Decision 15 — VERSION Reset to 1 (originally planned as 24)

**Decision**: Z_UI2_JSON2 VERSION constant set to **1** (not 24).

**Rationale**: Z_UI2_JSON2 is a new independent class, not a patched version of Z_UI2_JSON. VERSION 24 was used internally during migration but reset to 1 before first release. Both classes have separate version tracks going forward.

---

## Open Questions (Resolved)

1. **`write_raw` availability**: `IF_JSON_WRITER` does NOT expose `write_raw`. The `e_typekind-json` serialization path uses `CL_JSON_STRING_READER=>create(val)->skip_node(writer)` — resolved in `dump_type_int` macro.

2. **`symbol-header` format**: The `header` field in symbol cache was changed from `"NAME":` (with embedded quotes and colon) to just the plain formatted name. Cache-building code updated in all three places (`get_symbols_class`, `get_symbols_struct` ×2). `dump_symbols` now calls `writer->open_member(header)`.

3. **`DUMP` static method**: Delegated to `SERIALIZE` static method (which creates an instance and calls `serialize_int`). The old dynamic call to `DUMP_INT` was removed since `dump_int` no longer has a `RETURNING` clause.

4. **`dump_type` dispatch macro extended path**: The `ELSE` branch (when `mv_extended` is set, i.e., virtual method override active) creates an intermediate `CL_JSON_STRING_READER` from the returned string and calls `skip_node(writer)` to pipe the result to the writer.

5. **`serialize_int` with `name` parameter**: The `name` wrapping (emit `"name":value`) is handled by inline string template `|"{ name }":{ lv_value }|` after `dump_int` completes writing to the local writer and `get_json()` is called to retrieve the value.

6. **SJSON author consultation**: Deferred to optimization phase. Not blocking initial migration.

7. **`CL_JSON_XSTRING_READER`**: The `JSONX_CP` parameter was removed from `DESERIALIZE_INT` in z_ui2_json2. Decision to use `CL_JSON_STRING_READER` only (UTF-16 conversion done before calling, as before). `CL_JSON_XSTRING_READER` support can be added post-migration if needed.

8. **`dump_type` method vs `dump_type_int` macro**: `dump_type_int` (macro, fast path) was correctly migrated to writer calls. `dump_type` (virtual protected method, used when `mv_extended` is set for subclass override support) was NOT migrated in the same step. Its body still uses the old string-building + `escape_json` approach. Decision: `dump_type` retains its current string-returning signature (subclass compatibility requires this — see Decision 5). The macro dispatch bridges the gap via intermediate reader. The method body must be modernized to use string templates instead of `CONCATENATE`/`escape_json`, but the return type stays `TYPE JSON`. The `escape_json` macro call in `dump_type` body must be replaced with a direct `escape(...)` BIF call since the macro will be deleted.

---

## Implementation Status (as of 2026-05-09)

### Completed
- Phase 1: Patch sync (EDM datetime truncation, packed field fix, generate_struct duplicate fix)
- Phase 2: Deserialization migrated to `IF_JSON_READER` (`restore`, `restore_type`, `generate_int_r`, `generate_int_ex`)
- Phase 3: Serialization migrated to `IF_JSON_WRITER` (`dump_int`, `dump_symbols`, `serialize_int`, `dump_type_int` macro rewritten)
- `DUMP` static method delegated to `SERIALIZE`
- Dynamic calls for UTCLONG and ENUM replaced with static calls
- `symbol-header` format changed to plain name
- VERSION bumped to 24
- Clean-code pass: dead `read_string` removed, `lv_comp_name` removed, end-of-method comments removed, `CONCATENATE`/`EQ`/`NE` modernized, inline `DATA()` declarations where safe, `CREATE OBJECT` → `NEW` in `lc_json_custom`
- **`throw_error` macro deleted** — inlined as `RAISE EXCEPTION TYPE cx_sy_move_cast_error` at all call sites in `restore_type`
- **`restore_reference_ex` macro deleted** — body inlined at all 3 call sites in `restore_type`
- **`create_regexp` macro deleted** — replaced with `cl_abap_regex=>create_pcre( pattern = ... )` direct calls in `class_constructor` and `lcl_util=>class_constructor`
- **`escape_json` macro deleted** — `dump_type` method now uses `escape( val = ... format = cl_abap_format=>e_json_string )` BIF directly
- **`_escape` method deleted** from `lcl_util`
- **`get_convexit_func` moved to `lcl_util`** — all 9 call sites updated to `lcl_util=>get_convexit_func(...)`
- **`SO_REGEX_*` and `SO_TYPE_*` moved to `lcl_util`** — declared as public class-data; initialization moved to `lcl_util=>class_constructor`; all callers in main class updated with `lcl_util=>` prefix; `so_type_reftab` removed from PRIVATE SECTION of main class
- **`dump_int`, `dump_symbols`, `get_symbols*`, `get_fields`, `generate_int`, `generate_int_r`, `generate_int_ex`, `generate_struct` moved to PRIVATE SECTION**
- **`dump_type_ex` method deleted**
- **`MC_DEFAULT_INDENT`, `MC_NAME_SYMBOLS_MAP`, `mc_cov_error` deleted** (dead code)
- **`ref_tab` type deleted** from PUBLIC SECTION — declared but never used in z_ui2_json2
- **`SV_WHITE_SPACE` deleted** — was only used by `eat_white`/`while_offset_cs` macros which were removed in Phase 2; no callers remain in z_ui2_json2; initialization block removed from `class_constructor`
- **`detect_typekind` moved to `lcl_util`** — instance variables `mv_numc_as_string`, `mv_bool_types`, `mv_bool_3state` passed as explicit parameters; `mc_json_type` accessed as `z_ui2_json2=>mc_json_type` (public class-data); all 5 call sites updated with `lcl_util=>detect_typekind( ... numc_as_string = mv_numc_as_string bool_types = mv_bool_types bool_3state = mv_bool_3state )`
- **`RAW_TO_STRING` deleted** — not used internally; single test usage replaced with `cl_abap_codepage=>convert_from()` direct call
- **`STRING_TO_RAW` deleted** — not used internally or in tests
- **`DUMP` static method deleted** — was a thin alias for `SERIALIZE` with no callers in tests or internal code; callers should use `SERIALIZE` directly
- **`CL_JSON_XSTRING_READER` in `deserialize_int`** — when `jsonx` is provided, `CL_JSON_XSTRING_READER=>create( jsonx )` is used directly instead of converting via `cl_abap_codepage=>convert_from()` + `CL_JSON_STRING_READER`. The reader handles encoding natively. The `jsonx_cp` parameter is retained in the public API for backwards compatibility but is no longer used internally (the xstring reader defaults to UTF-8, which is the JSON standard encoding).
- **Base64 modernized** — `SSFC_BASE64_ENCODE` / `SSFC_BASE64_DECODE` (RFC-style FMs) replaced with `cl_http_utility=>encode_x_base64()` / `cl_http_utility=>decode_x_base64()` in `xstring_to_string` / `string_to_xstring`. Both are available from SAP_BASIS 7.0+. The `DECODE_X_BASE64` returns initial on invalid input; the existing fallback `IF out IS INITIAL. out = in.` is preserved.
- **`XSTRING_TO_STRING` and `STRING_TO_XSTRING` deleted from public API** — inlined as `cl_http_utility=>encode_x_base64()` / `cl_http_utility=>decode_x_base64()` at all 3 call sites (2 in `restore_type`, 1 in `dump_type`). Macros `xstring_to_string_int` and `string_to_xstring_int` also deleted and their logic inlined directly.
- **`JSONX_CP` parameter removed** from `DESERIALIZE` and `DESERIALIZE_INT` — no longer needed since `CL_JSON_XSTRING_READER` handles encoding natively (UTF-8 default, which is the JSON standard).
- **`CREATE OBJECT TYPE (mc_me_type)` replaced with `NEW z_ui2_json2(...)`** in `SERIALIZE` and `DESERIALIZE`. `mc_me_type` is always `\CLASS=Z_UI2_JSON2` — the dynamic type was inherited from `/UI2/CL_JSON` where the pattern served copy-paste portability. In Z_UI2_JSON2 the type is fixed, so `NEW` is exact. `mc_me_type` is retained in the PROTECTED SECTION for the subclass detection check in `constructor` (`rtti->absolute_name <> mc_me_type` → sets `mv_extended`). The one remaining `CREATE OBJECT data TYPE (type_descr->absolute_name)` in `restore` is genuine runtime polymorphism (unknown target type at compile time) and stays.

---

## Decision 16 — Syntax Fixes During SAP System Import (2026-05-10)

The following syntax errors were discovered and fixed during the first import into the SAP system:

### `DATA(var) TYPE ...` invalid in macros
`DATA(var) TYPE string.` is not valid ABAP — inline `DATA()` requires an initializer expression. All 15 occurrences in `dump_type_int` macro replaced with `DATA var TYPE ...`.

### `FINAL` on private methods illegal
`FINAL` on methods in the PRIVATE SECTION is a syntax error (private methods cannot be inherited, so `FINAL` is meaningless and rejected). Removed from all 7 private methods: `GENERATE_INT_R`, `DUMP_SYMBOLS`, `GET_SYMBOLS_STRUCT`, `GET_SYMBOLS_CLASS`, `GET_SYMBOLS`, `GET_FIELDS`, `GENERATE_INT_EX`.

### `e_typekind` visibility — moved to PUBLIC
`e_typekind` was in PROTECTED SECTION. `lcl_util=>detect_typekind` accesses it as `z_ui2_json2=>e_typekind-*` — but protected constants are not accessible via class name from a non-subclass (local helper class). Moved to PUBLIC SECTION and removed duplicate from PROTECTED. This is correct because subclasses and external callers already rely on it being public.

### `mid()` not a valid BIF in this context
`mid( var offset length )` is not a valid ABAP built-in function call syntax in method arguments. Replaced all 5 occurrences with `substring( val = var off = 1 len = strlen(var) - 2 )`.

### String offset `var+1(len)` not allowed on TYPE STRING
`TYPE STRING` fields do not support the `+offset(length)` syntax (only `TYPE C`/fixed-length types do). Replaced with `substring()` BIF. Removed intermediate `TYPE i` length variables that were introduced as an attempted workaround.

### `write_boolean( abap_true/abap_false )` wrong type
`IF_JSON_WRITER->write_boolean` takes `value TYPE string` (expected values: `'true'` / `'false'`), not `TYPE abap_bool`. Replaced `abap_true` → `` `true` `` and `abap_false` → `` `false` ``. Also removed a duplicate `write_boolean` call left by an earlier edit.

### `cl_json_string_writer=>create()` return type is `REF TO if_json_writer`
`CL_JSON_STRING_WRITER=>create()` is declared `RETURNING value(writer) TYPE REF TO if_json_writer` — it returns the interface, not the concrete class. Assigning to `REF TO cl_json_string_writer` therefore fails. Fixed in both `dump_type` and `serialize_int`:
- Both `lo_writer` variables declared as `REF TO if_json_writer`
- `get_json()` called via downcast: `CAST cl_json_string_writer( lo_writer )->get_json()`

### `lt_symbols` forward reference in `get_fields`
`FIELD-SYMBOLS: <sym> LIKE LINE OF lt_symbols` was declared before the inline `DATA(lt_symbols) = get_symbols(...)`. Inline `DATA()` is not visible before its statement. Fixed by declaring `lt_symbols TYPE t_t_symbol` explicitly before `FIELD-SYMBOLS`, and assigning via normal assignment.

### `GENERATE_INT` moved to PROTECTED
The test class (`abap_unit_testclass INHERITING FROM Z_UI2_JSON2`) calls `generate_int` directly. With `GENERATE_INT` in PRIVATE SECTION this is a syntax error. Moved to PROTECTED SECTION — it was already conceptually an internal method, and subclass test access is the intended use.

### `detect_typekind` IF/ELSEIF structure in `lcl_util`
The `ELSEIF` branches after the packed-field domain check were incorrectly indented in a prior edit (appeared to be after `ENDIF`). Verified correct: the `ELSEIF rv_type = typekind_num ...` chain is correctly part of the outer `IF rv_type = typekind_packed ... ELSEIF ...` block. No code change needed — structure was already correct.

---

## Implementation Status (as of 2026-05-10)

### All items completed including syntax fixes from first SAP import.

### Runtime fixes applied before first test run

**Missing `next_node()` before `skip_node()` in two macro locations:**

The kernel reader (`CL_JSON_STRING_READER`) starts positioned *before* the first node after `create()`. `skip_node()` requires the reader to already be positioned *at* a node. Two places in the macros created a fresh reader and called `skip_node()` without advancing first:

1. `dump_type` macro (extended/inherited path): `cl_json_string_reader=>create( dump_type_ext_json )` → added `dump_type_ext_rdr->next_node( )` before `skip_node( &4 )`.
2. `dump_type_int` macro (`e_typekind-json` branch): `cl_json_string_reader=>create( dump_type_int_jr )` → added `dump_type_int_rdr->next_node( )` before `skip_node( &3 )`.

Note: `restore_type` and `generate_int_r` do NOT need `next_node()` before their first reader access because they receive the reader from a caller that already advanced it.

**`LENGTH` parameter removed from `GENERATE_INT`:**

The `value(LENGTH) TYPE i OPTIONAL` parameter was a leftover from the old offset-based `z_ui2_json` API. The `z_ui2_json2` implementation never uses it — the body always creates a reader from the full `json` string. Removed from the PROTECTED method declaration.

### Still Outstanding

- **`e_typekind` constants: NOT moved to `lcl_util`** — `e_typekind` is referenced in macros (`dump_type_int`, `dump_type` — 15 occurrences) and in `lcl_util=>detect_typekind` (7 occurrences). Macros expand inline inside the class body and cannot use a `lcl_util=>` qualifier; moving would force every macro reference to become `z_ui2_json2=>e_typekind-*`, which is more verbose with no benefit. `lcl_util=>detect_typekind` already accesses it as `z_ui2_json2=>e_typekind-*` via the FRIENDS relationship. Additionally, `e_typekind` is part of the public API (subclasses use it). No action taken. — the previous parallel implementation of ~140 lines was a maintenance hazard: any fix to `dump_type_int` had to be mirrored manually in `dump_type`. The method now creates a local `CL_JSON_STRING_WRITER`, calls `dump_type_int` (same macro used by the non-inherited path), and returns `writer->get_json()`. This guarantees identical behaviour whether the class is inherited or not. The dispatch macro's `write_null` fallback (when `dump_type` returned initial) is now unreachable — `get_json()` always returns at least `"null"` — but is kept as a harmless safety net.

---

## Decision 10 — VERSION Reset to 1

**Decision**: Z_UI2_JSON2 VERSION constant set to 1 (not 24).

**Rationale**: Z_UI2_JSON2 is a new class with an independent lifecycle. VERSION 24 was an internal development number during migration. Both classes have separate version tracks going forward.

---

## Decision 11 — GEN_OPTIMIZE Removed (Always Optimized)

**Decision**: Removed `GEN_OPTIMIZE` parameter from GENERATE, DESERIALIZE, and constructor. Optimized generation is always active.

**Rationale**: The non-optimized path (REF TO data wrappers) produced awkward data structures. The optimized path (typed tables, dereferenced values) is what consumers want. Since V24 is a new class, incompatible changes are acceptable.

---

## Decision 12 — Trailing Commas Not Supported

**Decision**: Input JSON must be valid per RFC 8259. Trailing commas (`,}` / `,]`) cause parse errors.

**Rationale**: V23's lenient manual parser silently ignored them. The kernel reader is strict. Rather than adding a preprocessing workaround, we document this as a requirement and request tolerant mode from the IF_JSON_READER author.

---

## Decision 13 — generate_int_ex Removed

**Decision**: Method `generate_int_ex` deleted entirely. Callers use `generate_int_r` directly.

**Rationale**: `generate_int_ex` existed to temporarily set `mv_assoc_arrays = true` during generation. In V24, the generation path (generate_int_r) creates its own structures and never routes through restore_type's assoc_array handling for objects. The override was unnecessary.

---

## Decision 14 — Performance: Writer API Constrains String Operations

**Decision**: Use string templates (`|{ }|`) or `&&` concatenation for formatting timestamp/date/time values passed to the writer. Fixed-length `TYPE c` variables for intermediate timestamp storage.

**Rationale**: Since `IF_JSON_WRITER` methods require `TYPE string` parameters, the choice between CONCATENATE and string templates is less impactful than originally thought — the writer method call itself dominates. String templates are more readable for fixed-format output. Fixed-length `TYPE c` is used for intermediate timestamp storage where offset/length access is needed.

---

## Decision 15 — skip_node(writer) Workaround

**Decision**: JSON passthrough uses manual tree-walking (`lcl_util=>read_json_to_string`) instead of `skip_node(writer)`.

**Rationale**: `skip_node(writer)` does not correctly pipe a value subtree when the reader is positioned on an object member mid-document. This is a confirmed IF_JSON_READER defect. The workaround is encapsulated in a single utility method for easy replacement when the reader is fixed.

---

## Decision 16 — Cyclic References as null

**Decision**: Cyclic data/object references serialize as `null` instead of `{}`.

**Rationale**: `null` is the correct JSON representation for "cannot be serialized." An empty object `{}` is misleading — it suggests an empty structure, not an unresolvable reference.

---

## Decision 17 — DUMP_TYPE Writes Directly to Writer

**Decision**: `DUMP_TYPE` method signature changed to accept `WRITER` and `NAME` parameters. It writes directly to the writer instead of returning a JSON string. `TYPEKIND` is mandatory.

**Rationale**: The original design returned a JSON string from `DUMP_TYPE`, which the dispatch macro then parsed via an intermediate reader and piped to the real writer — creating 2 extra objects per field for extended classes. The new signature eliminates this serialize-then-reparse round-trip entirely. Subclasses now call `writer->write_string()`/`write_number()` directly.

---

## Decision 18 — Struct Cache Without Level Key

**Decision**: Removed `level` from the `mt_struct_cache` hash table key. The cache is keyed by `type_descr + include_aliases` only.

**Rationale**: In V23, `level` was needed because `dump_symbols` embedded indentation strings into the result — the level determined whitespace. In V1, `IF_JSON_WRITER` handles indentation natively via `set_option(option_indent)`. The cached symbols, type metadata, and field references are level-independent. Removing `level` from the key eliminates duplicate cache entries for the same struct type at different nesting depths. For recursive structures, each call receives a value-copy of the symbol table with re-bound data references.

---

## Decision 19 — TRY/CATCH Restructured for Deserialization

**Decision**: Extracted `restore_type_int` as a private method without TRY. `restore_type` is a thin wrapper that adds TRY/CATCH. Recursive calls use `restore_type_int` directly.

**Rationale**: SAT profiling showed 7.37M TRY block entries consuming 14M µs. The TRY was entered on every recursive `restore_type` call, but exceptions are rare (only type mismatches in non-strict mode). Moving TRY to entry points only (855K calls from `restore`'s field loop + top-level callers) reduced TRY overhead by 68%. Leaf-level MOVE exceptions (`data = sdummy`) are caught locally with targeted TRY blocks.

---

## Decision 20 — detect_typekind Generalized

**Decision**: `lcl_util=>detect_typekind` now accepts `cl_abap_typedescr` (base class) instead of `cl_abap_elemdescr`. `CONVEXIT` parameter is optional.

**Rationale**: Eliminates caller-side type check and cast. The method handles non-elementary types by returning `type_kind` directly, and only performs element-specific detection (timestamp domains, bool types, JSON type) when `kind = kind_elem`.

---

## Decision 21 — Initial Value Defaults Without Quotes

**Decision**: `INITIAL_TS`, `INITIAL_DATE`, `INITIAL_TIME` constructor defaults changed from `""` (with surrounding quotes) to `` `` (empty string). The writer adds quotes automatically.

**Rationale**: In V23, initial values were stored with surrounding quotes because they were concatenated directly into the JSON string. In V1, values are passed to `write_string()` which handles quoting. Storing quotes in the value itself would produce double-quoting. The empty default means "serialize as empty string" — the writer outputs `""`.

---

## Decision 22 — Performance Trade-off: Serialization vs Deserialization

**Decision**: Accept serialization overhead from `IF_JSON_WRITER` method calls in exchange for 33-48% deserialization improvement and architectural cleanliness.

**Rationale**: Profiling confirmed the initial serialization gap was from method call overhead (writer->write_string/write_number per field) vs V23's direct string concatenation, plus unnecessary `open_member`/`close_member` pairs. The deserialization gains (kernel reader, TRY restructure, cache improvements) more than compensate in typical round-trip scenarios.

---

## Decision 23 — Eliminate open_member/close_member Calls

**Decision**: All `open_member`/`close_member` calls removed from the serialization path. Member names are passed directly to `open_object( name = ... )`, `open_array( name = ... )`, and all `write_*( name = ... )` methods. `DUMP_INT` and `DUMP_SYMBOLS` each gained a `NAME TYPE STRING OPTIONAL` parameter.

**Rationale**: Confirmed by IF_JSON_WRITER author (Stefan): *"Instead of calling OPEN_MEMBER, you can simply provide the member name to the attribute name of OPEN_OBJECT, OPEN_ARRAY and WRITE_* methods."* This eliminates two extra method calls per complex field, closing the remaining serialization performance gap vs V23.

**Implementation note**: For the `assoc_arrays_opt = true` case (single-value table entries), the table key name is passed via `name` to `dump_symbols`, which uses it as `lv_name` for the single `dump_type` call when `opt_array = abap_true`. Regular array elements pass `name` as empty, which the writer interprets as no member name (unnamed array element).
