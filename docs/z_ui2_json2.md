# Z_UI2_JSON2 — Kernel API Edition

`Z_UI2_JSON2` is the successor to `Z_UI2_JSON`. It provides the same JSON serialization/deserialization capabilities but replaces the manual character-level parser and string-concatenation serializer with SAP's kernel-native JSON API (`IF_JSON_READER` / `IF_JSON_WRITER`).

Both classes coexist in the repository and have **independent version tracks**:
- `Z_UI2_JSON` (VERSION 23+) — unchanged, continues to work on SAP_BASIS 7.31+, receives only bug fixes
- `Z_UI2_JSON2` (VERSION 1) — requires **SAP_BASIS 7.57** or higher, receives all new development

---

## When to use which class

| Situation | Use |
|-----------|-----|
| SAP_BASIS < 7.57 | `Z_UI2_JSON` |
| SAP_BASIS ≥ 7.57, new development | `Z_UI2_JSON2` |
| Existing code using `Z_UI2_JSON` | No migration required — keep as-is |
| Subclass overriding `DUMP_TYPE` or `RESTORE` | Review signature changes below before migrating |

---

## Incompatible changes (migration checklist)

When migrating from `Z_UI2_JSON` to `Z_UI2_JSON2`, review these breaking changes:

### Strict JSON required

Input JSON must be valid per [RFC 8259](https://tools.ietf.org/html/rfc8259). Trailing commas (`,}` / `,]`) are **no longer tolerated** — unlike `Z_UI2_JSON`'s lenient parser that silently ignored them.

### GEN_OPTIMIZE parameter removed

The `GEN_OPTIMIZE` parameter is removed from `GENERATE`, `DESERIALIZE`, and the constructor. The optimized generation behavior (typed tables, dereferenced struct fields) is **always active**. Generated data structures contain typed values directly — not wrapped in `REF TO data`.

### DUMP static method removed

Use `SERIALIZE` directly — identical behavior.

### Removed utility methods

| Method | Replacement |
|--------|-------------|
| `DUMP` | `SERIALIZE` |
| `RAW_TO_STRING` | `cl_abap_codepage=>convert_from()` |
| `STRING_TO_RAW` | `cl_abap_codepage=>convert_to()` |
| `XSTRING_TO_STRING` | `cl_http_utility=>encode_x_base64()` |
| `STRING_TO_XSTRING` | `cl_http_utility=>decode_x_base64()` |
| `ESCAPE` | `escape( val = ... format = cl_abap_format=>e_json_string )` |
| `UNESCAPE` | Not needed — `IF_JSON_READER` returns unescaped strings |
| `BOOL_TO_TRIBOOL` | Inline the 3-line IF/ELSE |
| `TRIBOOL_TO_BOOL` | Inline the 3-line IF/ELSE |
| `GET_INDENT` | Not needed — `IF_JSON_WRITER` handles indentation |
| `EDM_DATETIME_TO_TS` | Internal only — `lcl_util=>read_edm_datetime()` |

### Removed parameters

| Parameter | Context | Reason |
|-----------|---------|--------|
| `GEN_OPTIMIZE` | `GENERATE`, `DESERIALIZE`, constructor | Always optimized |
| `OPTIMIZE` | `GENERATE` | Same as above |
| `JSONX_CP` | `DESERIALIZE`, `DESERIALIZE_INT` | `CL_JSON_XSTRING_READER` handles encoding natively |

### Pretty-print output format

`FORMAT_OUTPUT = 'X'` produces different whitespace (1-space indent, newline style from `IF_JSON_WRITER`). The JSON content is semantically equivalent.

### Error reporting in strict mode

When a JSON value is syntactically invalid, the kernel reader may throw before delivering the member name. Error paths like `$.struct.field` may report only `$.struct`.

---

## What changed internally

### Serialization

`SERIALIZE` / `SERIALIZE_INT` create a `CL_JSON_STRING_WRITER` internally, drive it through `DUMP_INT`, and return `writer->get_json()`. No intermediate string accumulation per field.

### Deserialization

`DESERIALIZE` / `DESERIALIZE_INT` create a `CL_JSON_STRING_READER` and drive `RESTORE_TYPE` through it. The reader maintains position state; no `OFFSET` parameter is threaded through recursive calls.

`IF_JSON_READER` returns string values already unescaped — no manual `\uXXXX` processing needed.

### Generation

`GENERATE` / `GENERATE_INT` always produce optimized data structures:
- Arrays where all elements share the same type → typed STANDARD TABLE (not table of REF TO data)
- Struct fields → directly typed components (STRING, INT, DATE, etc.)
- Type detection uses character checks (no regex) for date/time/timestamp patterns

---

## Signature changes (subclass authors)

If you have a subclass of `Z_UI2_JSON` and want to migrate it to inherit from `Z_UI2_JSON2`:

**`DUMP_INT`** (protected, final):
- Old: `RETURNING value(R_JSON) TYPE JSON`
- New: `IMPORTING WRITER TYPE REF TO IF_JSON_WRITER` — writes directly, no return value

**`DUMP_SYMBOLS`** (protected, final):
- Old: `RETURNING value(R_JSON) TYPE JSON`
- New: `IMPORTING WRITER TYPE REF TO IF_JSON_WRITER` — writes directly, no return value

**`DUMP_TYPE`** (protected, virtual — override this for custom value serialization):
- Old (Z_UI2_JSON): `RETURNING value(R_JSON) TYPE JSON`
- New: `IMPORTING WRITER TYPE REF TO IF_JSON_WRITER, NAME TYPE STRING optional, TYPEKIND TYPE ABAP_TYPEKIND` — writes directly to the writer. No intermediate serialization/parsing round-trip.

**`RESTORE`** (protected, virtual):
- Old: `IMPORTING JSON TYPE STRING, LENGTH TYPE I, CHANGING OFFSET TYPE I`
- New: `IMPORTING READER TYPE REF TO IF_JSON_READER`

**`RESTORE_TYPE`** (protected, virtual):
- Same direction change as `RESTORE` — reader-based.

**`GENERATE_INT_R`** (private — replaces `GENERATE_INT_EX`):
- Old (`GENERATE_INT_EX`): `IMPORTING JSON TYPE STRING, LENGTH TYPE I, CHANGING OFFSET TYPE I`
- New (`GENERATE_INT_R`): `IMPORTING READER TYPE REF TO IF_JSON_READER`
- Note: `GENERATE_INT_EX` is removed entirely; `GENERATE_INT_R` is the reader-based workhorse

---

## Usage

The public API (`SERIALIZE`, `DESERIALIZE`, `GENERATE`, `CONSTRUCTOR`) is largely identical to `Z_UI2_JSON`. Replace the class name:

```abap
" Before
DATA(lv_json) = z_ui2_json=>serialize( data = ls_data ).
z_ui2_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_data ).

" After
DATA(lv_json) = z_ui2_json2=>serialize( data = ls_data ).
z_ui2_json2=>deserialize( EXPORTING json = lv_json CHANGING data = ls_data ).
```

All constructor parameters, pretty-print modes, `ASSOC_ARRAYS`, `TS_AS_ISO8601`, `HEX_AS_BASE64`, `BOOL_TYPES`, name mappings, and conversion exits work identically.

For instance usage (repeated calls, better performance):

```abap
DATA(lo_json) = NEW z_ui2_json2(
  compress    = abap_true
  pretty_name = z_ui2_json2=>pretty_mode-camel_case ).

DATA(lv_json) = lo_json->serialize_int( data = ls_data ).
lo_json->deserialize_int( EXPORTING json = lv_json CHANGING data = ls_data ).
```

---

## Extending Z_UI2_JSON2

Inheritance works the same as with `Z_UI2_JSON`. Override `DUMP_TYPE` to customize value serialization, or `IS_COMPRESSABLE` to control which fields are omitted when compress is on.

**Important**: `DUMP_TYPE` now receives the writer directly. Write your custom value to the writer using `write_string`, `write_number`, `write_boolean`, or `write_null`. Call `super->dump_type(...)` for fallback handling.

```abap
CLASS lcl_my_json DEFINITION INHERITING FROM z_ui2_json2.
  PROTECTED SECTION.
    METHODS dump_type REDEFINITION.
ENDCLASS.

CLASS lcl_my_json IMPLEMENTATION.
  METHOD dump_type.
    " Custom handling for a specific type
    IF type_descr->absolute_name EQ '\TYPE=MY_CUSTOM_TYPE'.
      writer->write_string( name = name value = |{ data }-custom| ).
      RETURN.
    ENDIF.
    " Fall through to base class
    super->dump_type( data = data type_descr = type_descr convexit = convexit typekind = typekind writer = writer name = name ).
  ENDMETHOD.
ENDCLASS.
```

For full extension examples see [class-extension.md](class-extension.md) — the patterns shown there apply equally to `Z_UI2_JSON2`, with the method signature changes noted above.

---

## Performance comparison (Z_UI2_JSON V23 vs Z_UI2_JSON2 V1)

Measured on SAP_BASIS 7.57, same data sets, averaged over 5 runs (3 for generation).

| Scenario | V23 (µs) | V1 (µs) | Difference |
|----------|----------|---------|------------|
| **Deserialize** SBOOK 20K lines | 3,573K | 2,485K | **+30% faster** |
| **Deserialize** SBOOK camelCase | 2,659K | 1,778K | **+33% faster** |
| **Deserialize** AllTypes 10K | 1,329K | 934K | **+30% faster** |
| **Deserialize** Strings 10K | 465K | 325K | **+30% faster** |
| **Deserialize** Deep struct 1K×10 | 351K | 271K | **+23% faster** |
| **Deserialize** Timestamps 100K | 2,637K | 2,178K | **+17% faster** |
| **Generate** SBOOK 5K lines | 3,367K | 901K | **+73% faster** |
| Serialize Timestamps 100K | 595K | 804K | -35% slower |
| Serialize SBOOK 20K lines | 858K | 954K | -11% slower |
| Serialize SBOOK compressed+camelCase | 711K | 777K | -9% slower |
| Serialize AllTypes 10K | 336K | 383K | -14% slower |
| Serialize Strings 10K | 133K | 133K | ~0% |
| Serialize Deep struct 1K×10 | 97K | 104K | -7% slower |

**Summary**: Deserialization is 17-33% faster. Generation is 73% faster. Serialization is 7-35% slower — inherent `IF_JSON_WRITER` method call overhead per field vs. direct string concatenation. The timestamp case is worst because each timestamp value triggers multiple writer calls with little other work per row.

> Numbers are from a single SAP_BASIS 7.57 system and will vary by kernel patch level, hardware, and data characteristics.

The serialization gap is the inherent cost of `IF_JSON_WRITER` method calls vs. direct string concatenation. In typical round-trip scenarios (serialize + deserialize), V1 is significantly faster overall.

---

## Known limitations

See [history.md](history.md#known-limitations-pending-if_json_readerwriter-enhancements) for the full list. Key items:

- NBSP (U+00A0) as whitespace causes parse errors — kernel reader defect
- `skip_node( writer )` doesn't work correctly mid-document — workaround in `lcl_util=>read_json_to_string`
- No tolerant mode for trailing commas — consumers must supply valid JSON

---

## Version correlation

| Z_UI2_JSON2 | Based on Z_UI2_JSON | Notes |
|-------------|---------------------|-------|
| VERSION 1   | PL22 feature-set    | Initial release, kernel API migration |

Both classes have independent version tracks from this point forward.
