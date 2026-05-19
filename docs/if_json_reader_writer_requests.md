# IF_JSON_READER / IF_JSON_WRITER — Enhancement Requests & Bug Reports

Prepared for discussion with the SJSON package author.
Context: migration of `/UI2/CL_JSON` (SAP's most-used JSON serializer, 10k+ consumers) to kernel JSON APIs.

---

## Bug 1: `skip_node( writer )` does not work on member positions

**Severity**: High — requires 70-line workaround in production code.

**Description**: When the reader is positioned on an object member (not at root), `skip_node( writer )` does not correctly pipe only the value subtree to the writer. The result is either empty or includes the member name.

**Reproduction**:

```abap
" Setup: JSON with nested object as a member value
DATA(json) = `{"outer":{"inner":{"key":"value"}}}`.
DATA(reader) = cl_json_string_reader=>create( json ).

" Navigate to the "inner" member value
reader->next_node( ). " open_object (outer)
reader->next_node( ). " member "outer" = open_object

" Now reader is on "outer" member, node-type = open_object, node-name = "outer"
" Navigate inside "outer" to "inner"
reader->next_node( ). " member "inner" = open_object

" NOW: reader is positioned on member "inner" with node-type = open_object
" We want to capture {"key":"value"} as raw JSON string

DATA(writer) = cl_json_string_writer=>create( ).
reader->skip_node( writer ).
DATA(result) = CAST cl_json_string_writer( writer )->get_json( ).

" EXPECTED: result = '{"key":"value"}'
" ACTUAL: result is empty or malformed
cl_abap_unit_assert=>assert_equals( exp = '{"key":"value"}' act = result ).
```

**Expected behavior**: `skip_node( writer )` should write ONLY the value subtree (the object `{"key":"value"}`) to the writer, regardless of whether the reader is at root or mid-document on a member.

**Workaround**: Manual tree-walking with depth tracking (~70 lines). See `lcl_util=>read_json_to_string` in Z_UI2_JSON2.

---

## Bug 2: NBSP (U+00A0) not treated as whitespace

**Severity**: Medium — causes parse failures on real-world JSON from web sources.

**Description**: The reader throws a parse error when non-breaking space (U+00A0, UTF-8: 0xC2 0xA0) appears between JSON tokens. Many web APIs and copy-paste scenarios introduce NBSP.

**Reproduction**:

```abap
" JSON with NBSP (U+00A0) between tokens
DATA(nbsp) = cl_abap_conv_in_ce=>uccp( '00A0' ).
DATA(json) = `{` && nbsp && `"key":"value"}`.

DATA(reader) = cl_json_string_reader=>create( json ).
TRY.
    reader->next_node( ).
    " EXPECTED: succeeds, delivers open_object
    " ACTUAL: throws parse error "name expected at ..."
  CATCH cx_root INTO DATA(ex).
    cl_abap_unit_assert=>fail( ex->get_text( ) ).
ENDTRY.
```

**Expected behavior**: U+00A0 (and ideally U+FEFF BOM, U+3000 ideographic space) should be treated as whitespace between tokens, matching RFC 7159 Section 2 intent and real-world parser behavior (JavaScript, Python, Go all accept NBSP).

---

## Enhancement 1: Tolerant/lenient mode for trailing commas

**Priority**: Medium — affects backward compatibility with V23 consumers.

**Description**: Many JavaScript-generated JSON files contain trailing commas (`{"a":1,}` or `[1,2,]`). V23's manual parser handled these silently. The kernel reader rejects them per strict RFC 8259.

**Request**: Add a factory parameter or option to enable tolerant parsing:

```abap
" Option A: factory parameter
DATA(reader) = cl_json_string_reader=>create( json = json tolerant = abap_true ).

" Option B: set_option
DATA(reader) = cl_json_string_reader=>create( json ).
reader->set_option( option = if_json_reader=>option_tolerant ).
```

**Reproduction of the issue**:

```abap
DATA(json) = `{"items":[1,2,3,],"trailing":true,}`.
DATA(reader) = cl_json_string_reader=>create( json ).
TRY.
    reader->next_node( ). " open_object - OK
    reader->next_node( ). " "items" open_array - OK
    reader->next_node( ). " 1 - OK
    reader->next_node( ). " 2 - OK
    reader->next_node( ). " 3 - OK
    reader->next_node( ). " THROWS: "value expected at ']'"
  CATCH cx_root INTO DATA(ex).
    " This is the problem — trailing comma before ] causes failure
    cl_abap_unit_assert=>fail( ex->get_text( ) ).
ENDTRY.
```

---

## Enhancement 2: `get_offset()` method on IF_JSON_READER

**Priority**: Medium — would eliminate the skip_node workaround entirely.

**Description**: Expose the current character position in the source string. This enables extracting raw JSON substrings without a writer.

**Request**:

```abap
METHODS get_offset
  RETURNING VALUE(offset) TYPE i.
```

**Use case**:

```abap
DATA(json) = `{"data":{"nested":"value"},"other":1}`.
DATA(reader) = cl_json_string_reader=>create( json ).

reader->next_node( ). " open_object
reader->next_node( ). " member "data" = open_object

" Capture raw JSON for the "data" subtree
DATA(start) = reader->get_offset( ). " position of '{'
reader->skip_node( ).                 " advance past the subtree
DATA(end) = reader->get_offset( ).    " position after '}'

DATA(raw_json) = json+start(end - start). " = '{"nested":"value"}'
```

This would be the simplest and fastest solution for the raw JSON passthrough use case.

---

## Enhancement 3: Proper parse error exception class

**Priority**: Low — quality of life improvement.

**Description**: The reader currently throws generic exceptions (appears as `cx_dynamic_check` subclass). The exception text contains position info as a string (`'value expected at '...'`) but there's no structured access to error position or context.

**Request**: A dedicated exception class:

```abap
CLASS cx_json_parse_error DEFINITION INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.
    DATA offset TYPE i READ-ONLY.        " character position in source
    DATA context TYPE string READ-ONLY.  " snippet around error position
    DATA expected TYPE string READ-ONLY. " what was expected
ENDCLASS.
```

**Benefit**: Consumers can programmatically determine WHERE the error occurred and provide better error messages to end users.

---

## Performance Enhancement 4: Eliminate open_member/close_member overhead ✅ CONFIRMED & IMPLEMENTED

**Priority**: High — SAT showed 5.6M µs (3.2% total runtime) for these calls.

**Status**: Confirmed by IF_JSON_WRITER author (Stefan): *"Instead of calling OPEN_MEMBER, you can simply provide the member name to the attribute name of OPEN_OBJECT, OPEN_ARRAY and WRITE_* methods."*

**Implementation**: `DUMP_INT` and `DUMP_SYMBOLS` now accept a `name TYPE STRING OPTIONAL` parameter. Member names are passed directly to `open_object( name = ... )`, `open_array( name = ... )`, and all `write_*( name = ... )` calls. No `open_member`/`close_member` calls remain in the serialization path.

---

## Performance Enhancement 5: `write_string_unescaped` for pre-validated content

**Priority**: Low-Medium — would save ~5% on timestamp-heavy serialization.

**Description**: `write_string` always scans the value for characters that need JSON escaping (`"`, `\`, control chars). For values that are guaranteed safe (timestamps, dates, formatted numbers), this scan is wasted work.

**Request**:

```abap
METHODS write_string_unescaped
  IMPORTING
    name  TYPE string OPTIONAL
    value TYPE string
  RETURNING
    value(writer) TYPE REF TO if_json_writer.
```

**Use case**:

```abap
" Timestamp formatted as "2024-01-15T10:30:00Z" — guaranteed no special chars
DATA(ts_value) = `2024-01-15T10:30:00Z`.
writer->write_string_unescaped( name = `timestamp` value = ts_value ).
" Skips the escaping scan — directly writes "timestamp":"2024-01-15T10:30:00Z"
```

---

## Performance Enhancement 6: Bulk JSON tree serialization / deserialization

**Priority**: High — would eliminate the fundamental per-field kernel crossing overhead that makes serialization 7-35% slower than the pure-ABAP V23 implementation.

**Context**: `Z_UI2_JSON2` uses `IF_JSON_WRITER` for serialization by driving it one field at a time: one `write_string()` / `write_number()` / `open_object()` call per field, per nesting level. All name mapping, type detection, conversion exits, camelCase transformation, and compression logic is handled in ABAP before each call. The kernel crossing per field is the bottleneck — not the logic, not the parsing.

The request is for a **bulk handoff API**: ABAP prepares a complete, pre-resolved value tree (all logic already applied — names mapped, types resolved, values converted) and passes it to the kernel in a single call. The kernel does only what it is uniquely good at: escaping, unescaping, and emitting/parsing RFC-8259-compliant JSON efficiently.

**Two possible implementation shapes** — either would satisfy the use case:

### Option A: New kernel class `CL_JSON_TREE_WRITER` / `CL_JSON_TREE_READER`

A flat node table represents the JSON tree. Each row is one value or structural marker.

**Escaping contract** — asymmetric by design:
- **Deserialize**: kernel always unescapes string values unconditionally. ABAP always receives clean unescaped strings, regardless of node kind. No flag needed.
- **Serialize**: ABAP sets `kind` per node to tell the kernel whether to escape. ABAP knows from the ABAP type whether escaping is needed:
  - `S` — string with arbitrary content: kernel must escape (`"`, `\`, control chars)
  - `R` — raw string: value is pre-validated safe (no JSON special chars possible), kernel writes it inside quotes without escaping. Used for timestamps, dates, times, integers formatted as strings, base64 — any type whose value space is a strict subset of safe ASCII
  - `N` — number literal: written as bare JSON number, no quotes, no escaping
  - `B` — boolean literal: `true` or `false`, no quotes
  - `0` — null literal
  - `{` `}` `[` `]` — structural open/close markers

```abap
TYPES: BEGIN OF ty_json_node,
  name     TYPE string,   " member name (empty for array elements)
  kind     TYPE char1,    " S=string(escape), R=string(raw/no-escape),
                          " N=number, B=boolean, 0=null,
                          " {=open_object, }=close_object,
                          " [=open_array, ]=close_array
  value    TYPE string,   " for S: unescaped — kernel escapes on write, unescapes on read
                          " for R: pre-validated safe — kernel writes as-is, unescapes on read
                          " for N/B: ready-to-emit literal (no quotes)
END OF ty_json_node.

" Serialize: ABAP builds lt_nodes (logic fully applied), kernel emits JSON
DATA(lv_json) = cl_json_tree_writer=>serialize( lt_nodes ).

" Deserialize: kernel parses JSON into lt_nodes (all values unescaped), ABAP assigns fields
DATA(lt_nodes) = cl_json_tree_reader=>parse( lv_json ).
```

**Example** — serializing a structure with a timestamp and a string field:
```abap
" ABAP has already resolved: name mapping, type detection, conversion exits
lt_nodes = VALUE #(
  ( name = ``        kind = `{` value = `` )           " open root object
  ( name = `ts`      kind = `R` value = `2024-01-15T10:30:00Z` )  " timestamp: no escape needed
  ( name = `comment` kind = `S` value = `He said "hi"` )          " string: kernel escapes
  ( name = `count`   kind = `N` value = `42` )                    " number: no quotes
  ( name = ``        kind = `}` value = `` )           " close root object
).
DATA(lv_json) = cl_json_tree_writer=>serialize( lt_nodes ).
" Result: {"ts":"2024-01-15T10:30:00Z","comment":"He said \"hi\"","count":42}
```

For serialization: one call replaces N `write_*()` calls. Kernel processes the entire table in a single internal pass.
For deserialization: kernel parses the whole JSON and returns the flat node table with all values unescaped. ABAP walks the table to assign fields — same logic as today but driven by a table read instead of `next_node()` polling.

### Option B: New `CALL TRANSFORMATION` transformation type

Extend the `CALL TRANSFORMATION` statement with a new built-in transformation (e.g. `raw_json` or `json_tree`) that accepts a pre-resolved node table as source/result:

```abap
" Serialize
CALL TRANSFORMATION json_tree
  SOURCE nodes = lt_nodes
  RESULT XML lv_json.

" Deserialize
CALL TRANSFORMATION json_tree
  SOURCE XML lv_json
  RESULT nodes = lt_nodes.
```

This reuses the existing `CALL TRANSFORMATION` infrastructure (already bulk by design) and fits naturally alongside the `id` transformation.

**Why not just use `CALL TRANSFORMATION id`?**

`CALL TRANSFORMATION id` operates on typed ABAP data directly and applies its own fixed rules for type-to-JSON mapping. `Z_UI2_JSON2` needs to apply its own mapping logic (camelCase names, conversion exits, associative arrays, custom boolean types, compression, etc.) *before* the kernel sees the data. The node table acts as the resolved intermediate representation — a contract between ABAP logic and kernel I/O.

**Expected impact**:

Current serialization is 7-35% slower than the pure-ABAP V23 due to per-field kernel crossings. With bulk handoff:
- Serialization: one kernel call regardless of structure size → should match or exceed V23
- Deserialization: marginal gain (kernel parse is already bulk internally; gain comes from eliminating `next_node()` call overhead on the ABAP side)

The node table construction cost is comparable to the existing ABAP walk — and like the existing `mt_struct_cache`, the structural part (names, kinds, nesting) could be cached separately from values for repeated calls on the same type.

---

## Summary

| # | Type | Priority | Impact |
|---|------|----------|--------|
| Bug 1 | Bug | High | 70-line workaround for JSON passthrough |
| Bug 2 | Bug | Medium | Parse failure on NBSP whitespace |
| Enh 1 | Enhancement | Medium | Backward compat with lenient parsers |
| Enh 2 | Enhancement | Medium | Eliminates Bug 1 workaround entirely |
| Enh 3 | Enhancement | Low | Better error reporting |
| Enh 4 | Performance | **DONE** | Confirmed & implemented — all open_member/close_member calls eliminated |
| Enh 5 | Performance | Low-Medium | ~5% timestamp serialization |
| Enh 6 | Performance | High | Bulk tree handoff — eliminates per-field crossing overhead entirely |
