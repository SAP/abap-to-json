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
| 1 | ENUM deserialization throws on BASIS < 7.51 instead of silently ignoring | Bug | High |
| 2 | `it_no_compress_fields` / `it_always_compress_fields` constructor params | Feature | High |
| 3 | `path` parameter for `DESERIALIZE` / `GENERATE` — deserialize a JSON subnode | Feature | Medium |
| 4 | Strict-on-unknown-fields (raise on JSON keys not mapped to a structure component) | Feature | Medium |
| 5 | `IS_VALID( json )` method | Usability | Low |
| 6 | `BOOL_TYPES` on static API | Feature | Low |
| 7 | No distinction between JSON `null` and absent field | Feature | Low |
| 8 | Field ordering in GENERATE output | Feature | Low |
| 9 | `MAX_DEPTH` constructor parameter — bound recursion depth | Feature | Low |
| 10 | Catch-all field for unknown JSON keys (lossless round-trip) | Feature | Low |

---

## Part 1: Open Feature Requests

### 1.1 Built-in compression control: `it_no_compress_fields` / `it_always_compress_fields`

**Status**: Not implemented. `IS_COMPRESSABLE` is a virtual method, subclassing required.

**Frequency**: High — analysis of consumer code shows `IS_COMPRESSABLE` is the dominant (and essentially only) extension point that consumers override via subclassing. Two use cases appear consistently:
1. Exclude specific named fields from compression (never compress field X even if empty).
2. Always compress regardless of type/value.

Both cases are straightforward enough to support natively via constructor parameters, eliminating the most common reason to subclass.

**Suggestion**: Add to `CONSTRUCTOR`:
```abap
IMPORTING
  it_no_compress_fields     TYPE string_table OPTIONAL   " field names: never compress
  it_always_compress_fields TYPE string_table OPTIONAL   " field names: always compress
```
Field names match the raw ABAP names already passed to `IS_COMPRESSABLE` (uppercase, as
declared in the structure).

---

### 1.2 `path` parameter for `DESERIALIZE` / `GENERATE` — deserialize a JSON subnode directly

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
- Path segments are raw JSON attribute names, not ABAP field names. Pretty-name mapping (`pretty_mode`, `name_mappings`) does not apply to path resolution — the path `d-results` refers to the JSON keys `"d"` and `"results"` literally, regardless of the `pretty_name` setting in effect for the rest of the deserialization.
- Applicable to both `DESERIALIZE` and `GENERATE`.

**Workaround**: Declare the full wrapper structure (as shown above), or use `GENERATE` + `Z_UI2_DATA_ACCESS` to navigate to the subnode and then deserialize into the target type from that point.

---

### 1.3 Strict-on-unknown-fields

**Status**: Not implemented. Today, `STRICT_MODE = abap_true` raises `CX_SY_MOVE_CAST_ERROR` only on type mismatches; JSON keys with no matching ABAP component are silently ignored regardless of strict mode.

**Frequency**: Medium — common defensive feature in modern parsers (Go `DisallowUnknownFields`, .NET `UnmappedMemberHandling.Disallow`, Pydantic `extra='forbid'`). Catches contract drift and typos when consuming external APIs with stable schemas.

**Suggestion**: Extend strict mode behavior, or add a sibling constructor parameter (e.g. `disallow_unknown TYPE abap_bool`) so unknown JSON keys raise the same exception with the offending key name in the cast error's `source_typename`.

**Design note**: Constructor-only — the static API stays untouched. Pairs naturally with `STRICT_MODE`; could be folded into it (strict implies disallow-unknown) or kept separate (strict = type-only, disallow-unknown = membership-only) depending on whether existing strict-mode consumers would break under the stricter contract.

---

### 1.4 `IS_VALID( json )` method

**Status**: Not implemented.

**Frequency**: Low — confirmed demand is unclear (no tracked user requests). Could still be a cheap addition: a parse-only call that returns `abap_bool` without a `CHANGING` data target.

**Suggestion**:
```abap
CLASS-METHODS is_valid
  IMPORTING json            TYPE string
  RETURNING VALUE(rv_valid) TYPE abap_bool.
```
No bloat to existing signatures (standalone new method).

---

### 1.5 Custom boolean types via static API

**Status**: Constructor-only. `BOOL_TYPES` is exposed only on `CONSTRUCTOR`.

Users with custom `CHAR 1` boolean types who otherwise use only static methods must switch to the instance API for one parameter.

**Workaround**: Use the instance API:
```abap
DATA(lo_json) = NEW /ui2/cl_json( bool_types = `MY_BOOL,ZBOOL,` && /ui2/cl_json=>mc_bool_types ).
DATA(lv_json) = lo_json->serialize_int( ls_data ).
```

---

### 1.6 No distinction between JSON `null` and absent field

**Status**: Not implemented. `null` always maps to the ABAP initial value; no way to detect it.

**Frequency**: Medium — relevant for REST APIs that distinguish explicit null from field omission.

No nullable wrapper type or null-indicator field pattern is supported. A general fix would require either nullable wrapper types or a parallel "presence map" — both invasive.

---

### 1.7 Field ordering in GENERATE output

**Status**: Not implemented. Current behavior: alphabetical order (for cache normalization). Documented in FAQ.

Users ask for generated ABAP structures to have fields in the same order as the JSON keys.

**Workaround**: Pre-populate `mt_struct_type` cache via subclass constructor.

**Risk**: Changing the order could affect `mt_struct_type` cache hit rate; would need verification.

---

### 1.8 `MAX_DEPTH` constructor parameter

**Status**: Not implemented. Current parser recurses without an upper bound.

**Frequency**: Low — defensive feature. Common in modern parsers (.NET `MaxDepth = 64` default, most languages cap at 32–512).

Protects against deeply-nested JSON used to exhaust stack / cause DoS in shared services.

**Suggestion**: Add `max_depth TYPE i DEFAULT 0` to `CONSTRUCTOR` only (0 = unlimited, preserves current behavior). When exceeded, raise `CX_SY_MOVE_CAST_ERROR` (or a dedicated subclass) noting depth and position.

---

### 1.9 Catch-all field for unknown JSON keys (lossless round-trip)

**Status**: Not implemented. Unknown JSON keys are silently dropped today.

**Frequency**: Low. Niche — relevant for gateway/proxy scenarios where the class needs to read JSON, modify some known fields, and write it back without losing the rest.

**Inspiration**: Pydantic `extra='allow'` + `model_extra`, Jackson `@JsonAnyGetter`/`@JsonAnySetter`.

**Suggestion**: A convention-based marker — designate one structure component (by name, e.g. `_extras`, or by a constructor parameter naming the catch-all field) of type `string` (raw JSON fragment) or `string_table` / hash table to receive unmapped JSON keys, and serialize them back out on `SERIALIZE`. Constructor-only.

**Design note**: One-way friendly — no DOM access required; the caller just sees an extra field in their structure. Should interact predictably with `STRICT_MODE` and the proposed strict-on-unknown-fields (1.3) — likely: catch-all wins, no exception raised when the field is present.

---

## Part 2: Open Bugs

### 2.1 ENUM deserialization throws on BASIS < 7.51 instead of silently ignoring — **agreed to fix**

**Status**: Open, fix agreed.

**Source**: `src/z_ui2_json.clas.abap:2350`

Serialization works on all BASIS levels. For deserialization, the class tries `CL_ABAP_XSD=>TO_VALUE` dynamically; when this call fails (BASIS < 7.51 where `CL_ABAP_XSD` doesn't exist), it currently calls `throw_error`. Note 2650040 states: "From SAP_BASIS 7.51, below, the enums are **ignored**." The current code throws rather than ignoring — the behavior diverges from the documented contract.

**Fix**: One-line — replace `throw_error` in the `CATCH cx_sy_dyn_call_error` block with `eat_name sdummy. RETURN.` (consume the JSON value and continue, matching the documented "silently ignore" contract).

---

## Part 3: Observed Usage Patterns in Consumer Code

These patterns were identified through analysis of real-world `/UI2/CL_JSON` consumers.
No internal system names or class names are included.

### 3.1 Pre-processing before DESERIALIZE

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

**OData envelope rewriting** — examining the top-level key of a JSON response and rewriting the outer array/object shape to match the ABAP target type. Compensates for `DESERIALIZE` requiring an exact shape match. Item 1.2 (`path` parameter) addresses this directly.

---

### 3.2 Post-processing after SERIALIZE

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

### 3.3 Most Common Parameter Combinations

| Combination | Frequency | Context |
|---|---|---|
| `pretty_name = pretty_mode-camel_case` | Very high | REST/OData/BTP payloads |
| `pretty_name = pretty_mode-camel_case` + `name_mappings` | High | camelCase base + specific field overrides (e.g. `@context`) |
| `compress = abap_true` + `pretty_mode-low_case` | Medium | LLM prompt context |
| `pretty_mode-low_case` + `assoc_arrays = abap_true` | Seen | REST APIs returning dicts/maps |
| `pretty_name = abap_true` (legacy boolean) | Seen | Legacy code still in production |
| No parameters | Common | Simple round-trips with uppercase field names |
| Inline `name_mappings = VALUE #( ... )` | Seen | Small one-off mappings, preferred style |

The `pretty_mode-camel_case` + `name_mappings` combination is the dominant REST API pattern.
Inline `VALUE #(...)` for name_mappings should be documented as the recommended idiom.

---

### 3.4 Error Handling Patterns

**No error handling** — most common. Consumers rely on non-strict mode silently ignoring mismatches. Works in practice but produces silent data loss when JSON doesn't match the ABAP type.

**IS INITIAL after DESERIALIZE** — used as a validity check:
```abap
/ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
IF ls_result IS INITIAL.
  RAISE EXCEPTION TYPE cx_my_error.
ENDIF.
```
Anti-pattern. Recommended replacement is `STRICT_MODE = abap_true` + `DESERIALIZE_INT` → `CATCH CX_SY_MOVE_CAST_ERROR`. Documented in FAQ.

**Two-attempt pattern** — try primary type, fall back to error structure:
```abap
/ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_response ).
IF ls_response IS INITIAL AND ls_error IS REQUESTED.
  /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_error ).
ENDIF.
```

---

### 3.5 Subclassing Patterns

Analysis confirms `IS_COMPRESSABLE` is the dominant — and essentially the only — extension point overridden in practice. Three subclass patterns were observed:

1. **Exclude specific fields from compression**: overrides `IS_COMPRESSABLE` to suppress compression for a configured list of field names passed to the constructor. One observed implementation had a bug: the field list was declared as class-data (shared across all instances) instead of instance data.

2. **Disable compression entirely**: overrides `IS_COMPRESSABLE` to always return `abap_false`, ignoring type and value.

3. **Configuration-driven wrapper**: a local subclass inside a wrapper class reads name mappings and compression rules from a database table keyed by a process ID, overriding both `IS_COMPRESSABLE` and `DUMP_TYPE`. The public wrapper exposes a typed API while hiding the subclass.

No `PRETTY_NAME` / `PRETTY_NAME_EX` overrides were observed in consumer code. `DUMP_TYPE` overrides appear only in specialized wrappers.

Building `it_no_compress_fields` and `it_always_compress_fields` into the constructor (see 1.1) would eliminate the most common subclassing reason.
