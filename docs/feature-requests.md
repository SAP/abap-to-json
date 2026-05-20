# Feature Requests, Bug Reports & Usage Patterns

Compiled from: repository documentation, FAQ, patch history, GitHub issues
(https://github.com/SAP/abap-to-json/issues), and analysis of real-world `/UI2/CL_JSON`
consumer code across a large SAP landscape (~3,000 usages across ~400 consuming objects).

Scope: `Z_UI2_JSON` / `/UI2/CL_JSON` only. Items specific to the Z_UI2_JSON2 kernel-API
migration are tracked separately.

---

## Priority Summary

| # | Item | Type | Priority |
|---|------|------|----------|
| 1 | `it_no_compress_fields` / `it_always_compress_fields` constructor params | Feature | High |
| 2 | Custom boolean types via static `SERIALIZE`/`DESERIALIZE` (no subclassing) | Feature | High |
| 3 | `DESERIALIZE` returns success/failure flag | Usability | High |
| 4 | `INITIAL_DATE` / `INITIAL_TIME` / `INITIAL_TS` on static `SERIALIZE` | Usability | Medium |
| 5 | `IS_VALID( json )` method | Usability | Medium |
| 6 | Streaming / chunked serialization for huge tables | Feature | Medium |
| 7 | ENUM deserialization throws on BASIS < 7.51 instead of silently ignoring | Bug | Medium |
| 8 | `path` parameter for `DESERIALIZE` — deserialize a JSON subnode directly | Feature | Medium |
| 9 | ASSOC_ARRAYS composite key separator not configurable | Config | Low |
| 10 | Deprecate `pretty_name = abap_true` boolean form | Cleanup | Low |
| 11 | Promote `Z_UI2_DATA_ACCESS` for post-GENERATE use | Docs | Low |

---

## Part 1: Feature Requests

### 1.1 Built-in compression control: `it_no_compress_fields` / `it_always_compress_fields`

**Status**: Not implemented. `IS_COMPRESSABLE` is a virtual method, subclassing required.

**Frequency**: High — analysis of consumer code shows `IS_COMPRESSABLE` is the dominant (and essentially only) extension point that consumers override via subclassing. Two use cases appear consistently:
1. Exclude specific named fields from compression (never compress field X even if empty).
2. Always compress regardless of type/value.

Both cases are straightforward enough to support natively via constructor parameters, eliminating the most common reason to subclass.

**Suggestion**: Add to the constructor and to `SERIALIZE`:
```abap
IMPORTING
  it_no_compress_fields     TYPE string_table OPTIONAL   " field names: never compress
  it_always_compress_fields TYPE string_table OPTIONAL   " field names: always compress
```

---

### 1.2 Custom boolean types via static API

**Status**: Not implemented for static methods. `BOOL_TYPES` is a constructor-only parameter.

**Frequency**: High — one of the most common user questions.

Users have `CHAR 1` boolean types not in the default list (`ABAP_BOOLEAN`, `BOOLEAN`, `XFELD`, etc.) and want `true`/`false` conversion without creating an instance. Currently the only options are: pass `BOOL_TYPES` to the constructor (requires instance API), or subclass and override `MC_BOOL_TYPES` in `class_constructor`.

**Suggestion**: Add `bool_types TYPE string OPTIONAL` to static `SERIALIZE` and `DESERIALIZE`.

---

### 1.3 Streaming / chunked serialization for huge tables

**Status**: Not implemented.

**Frequency**: Medium — dedicated FAQ entry.

Users get `SYSTEM_NO_ROLL`, `STRING_SIZE_TOO_LARGE`, `MEMORY_NO_MORE_PAGING` when serializing tables that produce >1 GB JSON strings. Memory exceptions are not catchable, so size cannot be validated up front.

**Workaround**: Manually split the table into chunks before calling the class.

**Suggestion**: A `serialize_chunk( data start end )` API that serializes a table slice into a partial JSON array fragment, allowing callers to write to a stream or combine chunks themselves.

---

### 1.4 Configurable ASSOC_ARRAYS composite key separator

**Status**: Not implemented. Hardcoded as `-` (`MC_KEY_SEPARATOR` constant).

**Frequency**: Low.

The key separator for composite multi-key tables serialized as associative arrays is always `-`. If any key value contains `-`, the round-trip breaks silently.

**Suggestion**: Add `key_separator TYPE string DEFAULT '-'` to the constructor and static methods.

---

### 1.5 No distinction between JSON `null` and absent field

**Status**: Not implemented. `null` always maps to the ABAP initial value; no way to detect it.

**Frequency**: Medium — relevant for REST APIs that distinguish explicit null from field omission.

No nullable wrapper type or null-indicator field pattern is supported.

---

### 1.6 Currency/quantity field pair formatting (CURR/CUKY)

**Status**: Not planned — maintainer explicitly declines default support (implementation complexity, performance penalty). Documented in FAQ.

**Workaround**: Subclass and override `DUMP_INT`/`RESTORE_TYPE`.

---

### 1.7 Deserialization of `REF TO <interface>` attributes

**Status**: Not implemented — cannot determine concrete class to instantiate. Documented in FAQ.

Classes with `TYPE REF TO <interface>` attributes cannot be deserialized. The deserializer cannot determine which concrete class to instantiate for an interface reference.

---

### 1.8 Field ordering in GENERATE output

**Status**: Not implemented. Current behavior: alphabetical order (for cache normalization). Documented in FAQ.

Users ask for generated ABAP structures to have fields in the same order as the JSON keys.

**Workaround**: Pre-populate `mt_struct_type` cache via subclass constructor.

---

### 1.9 `path` parameter for `DESERIALIZE` / `GENERATE` — deserialize a JSON subnode directly

**Status**: Not implemented.

**Frequency**: Medium — the OData response wrapper pattern is the canonical example. OData v2 wraps all results in a `{"d":{"results":[...]}}` envelope. To deserialize the inner `results` array into a typed ABAP table, callers today must declare the full outer wrapper structure just to give the deserializer a navigation target:

```abap
DATA:
  BEGIN OF ls_odata_response,
    BEGIN OF d,
      results TYPE STANDARD TABLE OF ts_result WITH DEFAULT KEY,
    END OF d,
  END OF ls_odata_response.

/ui2/cl_json=>deserialize( EXPORTING json = lv_json
                                     pretty_name = pretty_mode-camel_case
                           CHANGING  data = ls_odata_response ).
DATA(lt_results) = ls_odata_response-d-results.
```

With a `path` parameter, the caller could deserialize directly into the target table without the wrapper boilerplate:

```abap
DATA lt_results TYPE STANDARD TABLE OF ts_result WITH DEFAULT KEY.

/ui2/cl_json=>deserialize( EXPORTING json = lv_json
                                     path = `d-results`
                                     pretty_name = pretty_mode-camel_case
                           CHANGING  data = lt_results ).
```

**Design considerations**:

- Path syntax: same `-` separator as `Z_UI2_DATA_ACCESS` / `iv_component` for consistency.
- Array access: addressing an element by index (e.g. `d-results[0]`) is desirable but adds complexity. A first version could skip array indexing and only support object member traversal.
- Pretty-name rules must apply to the path segments the same way they apply to field names (e.g. `camelCase` input path `d-results` maps correctly regardless of pretty_mode).
- Applicable to both `DESERIALIZE` and `GENERATE`.

**Workaround**: Declare the full wrapper structure (as shown above), or use `GENERATE` + `Z_UI2_DATA_ACCESS` to navigate to the subnode and then deserialize into the target type from that point.

---

## Part 2: Open Bugs

### 2.1 ENUM deserialization throws on BASIS < 7.51 instead of silently ignoring

**Status**: Open. Serialization works on all BASIS levels. For deserialization, the class tries `CL_ABAP_XSD=>TO_VALUE` dynamically; when this call fails (BASIS < 7.51 where `CL_ABAP_XSD` doesn't exist), it throws instead of silently ignoring the field as documented.

**Source**: `src/z_ui2_json.clas.abap:2350`

Note 2650040 states: "From SAP_BASIS 7.51, below, the enums are **ignored**." The current code throws rather than ignoring — the behavior diverges from the documented contract.

---

## Part 3: Usability Improvements

### 3.1 `INITIAL_DATE` / `INITIAL_TIME` / `INITIAL_TS` on static `SERIALIZE`

**Status**: Not on static `SERIALIZE`. These three parameters are constructor-only.

**Frequency**: Medium — acknowledged in `docs/faq.md`: "If I get multiple requests regarding extending SERIALIZE with these defaults, I will do it."

Teams using static methods everywhere must switch to the instance API just to control initial date/time rendering.

**Suggestion**: Add `initial_ts`, `initial_date`, `initial_time TYPE string OPTIONAL` to static `SERIALIZE`.

---

### 3.2 `DESERIALIZE` returns success/failure flag

**Status**: Not implemented. `DESERIALIZE` always succeeds silently in non-strict mode.

**Frequency**: High — consumer code analysis shows a widespread pattern of checking `IS INITIAL` after DESERIALIZE as a proxy for "did it work?", and wrapping DESERIALIZE in service-layer classes that raise exceptions when the result is empty.

**Suggestion**: Add a `rv_success TYPE abap_bool` returning parameter to `DESERIALIZE` (or a separate `TRY_DESERIALIZE` method) that returns `abap_false` when the JSON did not match the target type, without requiring `STRICT_MODE`.

---

### 3.3 `IS_VALID( json )` method

**Status**: Not implemented.

**Frequency**: Medium — users want to validate JSON before processing without side effects on a CHANGING data target.

**Suggestion**:
```abap
CLASS-METHODS is_valid
  IMPORTING json            TYPE string
  RETURNING VALUE(rv_valid) TYPE abap_bool.
```

---

### 3.4 Static methods cannot be subclassed — verbose wrapper required

**Status**: By design — ABAP static methods cannot be redefined. Documented in `docs/class-extension.md`.

**Frequency**: Very common.

Subclasses must copy the full `SERIALIZE`/`DESERIALIZE` signature into new static wrapper methods (`SERIALIZE_EX`, etc.) and maintain them. This is standard ABAP but creates friction.

**Mitigation**: The class-extension guide documents the recommended pattern. No immediate API change feasible.

---

### 3.5 Class instance deserialization fails silently for mandatory constructor parameters

**Status**: Open — documented in `docs/advanced.md` but not in FAQ.

Deserializing JSON into ABAP class instances where the constructor has mandatory parameters silently produces empty results. No exception is raised.

**Suggestion**: Add to FAQ; consider throwing a specific exception when instantiation fails due to mandatory parameters.

---

### 3.6 Private/protected attribute serialization requires FRIENDS declaration

**Status**: By design — documented in `docs/advanced.md` but not in FAQ.

To serialize private/protected attributes of ABAP objects, the target class must declare the serializer as a FRIEND. Not possible without modifying the target class.

**Suggestion**: Add to FAQ so users discover this constraint before attempting it.

---

### 3.7 Deprecate `pretty_name = abap_true` boolean form

**Status**: Still accepted silently. The boolean `abap_true` form of `pretty_name` (equivalent to `pretty_mode-low_case`) is legacy but still appears in active production code.

**Suggestion**: Add a deprecation note to docs.

---

## Part 4: Observed Usage Patterns in Consumer Code

These patterns were identified through analysis of real-world `/UI2/CL_JSON` consumers.
No internal system names or class names are included.

### 4.1 Pre-processing before DESERIALIZE

**LLM output stripping** — the most common pre-processing pattern found. Any code that sends an LLM prompt asking for JSON output must strip the markdown code fence the LLM wraps around it:
```abap
REPLACE ALL OCCURRENCES OF '```json' IN lv_json WITH ''.
REPLACE ALL OCCURRENCES OF '```'     IN lv_json WITH ''.
CONDENSE lv_json.
/ui2/cl_json=>deserialize( EXPORTING json = lv_json ... CHANGING data = ls_result ).
```
This exact pattern is copy-pasted across many classes. A documented utility (or optional `pre_clean` parameter) would eliminate the boilerplate.

**Whitespace normalization** — stripping CR/LF and newlines before deserialization:
```abap
REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_json WITH space.
REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_json WITH space.
```

**OData envelope rewriting** — examining the top-level key of a JSON response and rewriting the outer array/object shape to match the ABAP target type. Compensates for `DESERIALIZE` requiring an exact shape match.

---

### 4.2 Post-processing after SERIALIZE

**JSON-in-a-field** — storing serialized JSON as a string field in an OData entity, DB table, or RFC parameter. Very common pattern; the serializer output is treated as an opaque string.

**Hybrid JSON assembly** — serializing a sub-structure and embedding it in a larger JSON envelope built with string templates:
```abap
DATA(lv_part) = /ui2/cl_json=>serialize( data = ls_detail compress = abap_false
  pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
rv_json = |\{ "status": "ok", "detail": { lv_part } \}|.
```
Suggests demand for a "serialize with named root key" option or partial-document building.

**LLM prompt construction** — serializing ABAP data as context for an LLM prompt. The combination `compress = abap_true` + `pretty_mode-low_case` appears consistently here (compact token count, lowercase keys that LLMs parse better).

---

### 4.3 Most Common Parameter Combinations

| Combination | Frequency | Context |
|---|---|---|
| `pretty_name = pretty_mode-camel_case` | Very high | REST/OData/BTP payloads |
| `pretty_name = pretty_mode-camel_case` + `name_mappings` | High | camelCase base + specific field overrides (e.g. `@context`) |
| `compress = abap_true` + `pretty_mode-low_case` | Medium | LLM prompt context |
| `pretty_mode-low_case` + `assoc_arrays = abap_true` | Seen | REST APIs returning dicts/maps |
| `pretty_name = abap_true` (deprecated boolean) | Seen | Legacy code still in production |
| No parameters | Common | Simple round-trips with uppercase field names |
| Inline `name_mappings = VALUE #( ... )` | Seen | Small one-off mappings, preferred style |

The `pretty_mode-camel_case` + `name_mappings` combination is the dominant REST API pattern.
Inline `VALUE #(...)` for name_mappings should be documented as the recommended idiom.

---

### 4.4 Error Handling Patterns

**No error handling** — most common. Consumers rely on non-strict mode silently ignoring mismatches. Works in practice but produces silent data loss when JSON doesn't match the ABAP type.

**IS INITIAL after DESERIALIZE** — used as a validity check:
```abap
/ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
IF ls_result IS INITIAL.
  RAISE EXCEPTION TYPE cx_my_error.
ENDIF.
```
This is a workaround for the missing success/failure return (see 3.2).

**Two-attempt pattern** — try primary type, fall back to error structure:
```abap
/ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_response ).
IF ls_response IS INITIAL AND ls_error IS REQUESTED.
  /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_error ).
ENDIF.
```

---

### 4.5 Subclassing Patterns

Analysis confirms `IS_COMPRESSABLE` is the dominant — and essentially the only — extension point overridden in practice. Three subclass patterns were observed:

1. **Exclude specific fields from compression**: overrides `IS_COMPRESSABLE` to suppress compression for a configured list of field names passed to the constructor. One observed implementation had a bug: the field list was declared as class-data (shared across all instances) instead of instance data.

2. **Disable compression entirely**: overrides `IS_COMPRESSABLE` to always return `abap_false`, ignoring type and value.

3. **Configuration-driven wrapper**: a local subclass inside a wrapper class reads name mappings and compression rules from a database table keyed by a process ID, overriding both `IS_COMPRESSABLE` and `DUMP_TYPE`. The public wrapper exposes a typed API while hiding the subclass.

No `PRETTY_NAME` / `PRETTY_NAME_EX` overrides were observed in consumer code. `DUMP_TYPE` overrides appear only in specialized wrappers.

Building `it_no_compress_fields` and `it_always_compress_fields` into the constructor (see 1.1) would eliminate the most common subclassing reason.

---

### 4.6 Dynamic Data Access after GENERATE

The post-GENERATE navigation pattern using `ASSIGN COMPONENT` is widespread in consumer code:
```abap
ASSIGN lr_data->* TO <root>.
ASSIGN COMPONENT 'MY_FIELD' OF STRUCTURE <root> TO <field>.
ASSIGN <field>->* TO <value>.
```
Consumers appear unaware of `Z_UI2_DATA_ACCESS`, which provides a cleaner path-based API for exactly this pattern. Better promotion in the docs would help.

---

## Part 5: GitHub Issues Summary

All issues are currently closed. Key findings:

| Issue | Summary | Status |
|-------|---------|--------|
| #7 | Infinite loop on malformed JSON | Fixed in a subsequent patch level |
| #11 | Option to skip escaping for specific values (e.g. Windows paths) | Closed without documented resolution — no built-in "skip escaping" flag exists |
| #12 | Scientific notation (TYPE F) from dynamic data access | TYPE F always serializes scientific in ABAP; concern is in the data access layer, not the serializer |
| #19 | Apache Parquet support | Out of scope — JSON-only library |
| #20 | OData `/Date(...)` off-by-one-second rounding | Fixed in a subsequent patch level |
