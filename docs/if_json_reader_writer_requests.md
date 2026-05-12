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

## Performance Enhancement 4: Eliminate open_member/close_member overhead for primitives

**Priority**: High — SAT shows 5.6M µs (3.2% total runtime) for these calls.

**Description**: The current API pattern for writing a named primitive member requires 3 method calls:

```abap
" Current pattern (3 calls per field):
writer->open_member( 'fieldName' ).
writer->write_string( 'value' ).
writer->close_member( ).
```

The `write_string( name = ... value = ... )` combined form EXISTS in the interface but the writer still requires the `open_member`/`close_member` framing for structural correctness. If we use `write_string( name = 'fieldName' value = 'value' )` without `open_member`, it works — but the documentation is unclear about whether this is officially supported.

**SAT evidence** (20k rows × 5 iterations):

```
open_member:  6.39M hits, 3.4M µs net
close_member: 6.39M hits, 2.2M µs net
Total overhead: 5.6M µs that didn't exist in string-concatenation approach
```

**Request**: Confirm/document that `write_string( name = value = )` is the correct single-call pattern for named primitives WITHOUT requiring `open_member`/`close_member`. Or provide an optimized `write_member( name = value = )` that avoids the overhead.

**Reproduction showing both patterns produce identical output**:

```abap
" Pattern A: open_member + write + close_member
DATA(w1) = cl_json_string_writer=>create( ).
w1->open_object( ).
w1->open_member( `key` ).
w1->write_string( `value` ).
w1->close_member( ).
w1->close_object( ).
DATA(json1) = CAST cl_json_string_writer( w1 )->get_json( ).

" Pattern B: write_string with name parameter (no open/close_member)
DATA(w2) = cl_json_string_writer=>create( ).
w2->open_object( ).
w2->write_string( name = `key` value = `value` ).
w2->close_object( ).
DATA(json2) = CAST cl_json_string_writer( w2 )->get_json( ).

" Both should produce: {"key":"value"}
cl_abap_unit_assert=>assert_equals( exp = json1 act = json2 ).
cl_abap_unit_assert=>assert_equals( exp = `{"key":"value"}` act = json2 ).
```

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

## Summary

| # | Type | Priority | Impact |
|---|------|----------|--------|
| Bug 1 | Bug | High | 70-line workaround for JSON passthrough |
| Bug 2 | Bug | Medium | Parse failure on NBSP whitespace |
| Enh 1 | Enhancement | Medium | Backward compat with lenient parsers |
| Enh 2 | Enhancement | Medium | Eliminates Bug 1 workaround entirely |
| Enh 3 | Enhancement | Low | Better error reporting |
| Enh 4 | Performance | High | 5.6M µs overhead per serialization run |
| Enh 5 | Performance | Low-Medium | ~5% timestamp serialization |
