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
| 1 | ~~ENUM deserialization throws on BASIS < 7.51 instead of silently ignoring~~ (DONE — Z_UI2_JSON) | Bug | High |
| 2 | DECFLOAT16/34 zero serializes as `null` (fixed both editions, /UI2/CL_JSON rollout pending) | Bug | High |
| 3 | ~~`disable_string_type_detect` flag — opt out of date/time autodetect~~ (DONE — both editions) | Feature | High |
| 4 | `it_no_compress_fields` / `it_always_compress_fields` constructor params | Feature | High |
| 5 | ~~`path` parameter for `DESERIALIZE` / `GENERATE` — deserialize a JSON subnode~~ (DONE — both editions, object + array-index) | Feature | Medium |
| 6 | ~~Strict-on-unknown-fields (raise on JSON keys not mapped to a structure component)~~ (DONE — both editions) | Feature | Medium |
| 7 | `IS_VALID( json )` method | Usability | Low |
| 8 | `BOOL_TYPES` on static API | Feature | Low |
| 9 | No distinction between JSON `null` and absent field | Feature | Low |
| 10 | Field ordering in GENERATE output | Feature | Low |
| 11 | `MAX_DEPTH` constructor parameter — bound recursion depth | Feature | Low |
| 12 | Catch-all field for unknown JSON keys (lossless round-trip) | Feature | Low |

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

**Status**: Implemented in `Z_UI2_JSON` and `Z_UI2_JSON2` — `DESERIALIZE`, `DESERIALIZE_INT`, and `GENERATE`. Supports object-member traversal (`d-results`) and array indexing (`d-results[5]`, bare `[1]` on a top-level array). Documented in [`basic.md`](basic.md#path-extracting-a-subnode). Rollout to `/UI2/CL_JSON` pending.

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

#### Implementation evaluation (2026-07-23) — target: next patch level, all classes

**Scope decision**: implement in all three parsing entry points so the API stays uniform across editions: `Z_UI2_JSON`, `/UI2/CL_JSON` (string-offset parser), and `Z_UI2_JSON2` (kernel `IF_JSON_READER`). The API surface (a new optional `PATH` importing parameter) is identical; only the internal navigation differs.

**API shape** (per the narrow-static-API rule this *can* go on the static methods — it is a common one-off need, not an advanced switch, and it does not depend on constructor state):
```abap
class-methods DESERIALIZE
  importing ... !PATH type STRING optional ...
```
Same addition on `GENERATE`. No change to `CONSTRUCTOR` or the `*_INT` instance methods' core loop — path resolution is a pre-positioning step that runs once before the existing `restore_type` / `generate_int` recursion begins.

**Where it hooks in — `Z_UI2_JSON` / `/UI2/CL_JSON` (offset parser):**
- `deserialize_int` (`src/z_ui2_json.clas.abap:674`) positions `offset` at the first structural char via `while_offset_not_cs`, then calls `restore_type`.
- A path pre-step would, before that call, walk the object levels named in `PATH`: at each segment `eat_char '{'` → loop `eat_name` / `eat_white` / `eat_char ':'`, comparing the key to the segment; on match, descend; on miss, skip the value. Skipping an unwanted value already exists — `restore_type` called **without** `data` supplied consumes and discards a value (`src/z_ui2_json.clas.abap:2040`, `:2196`), so the skip logic is reusable, not new code.
- After the final segment is matched and `offset` sits on the subnode's opening char, hand off to the existing `restore_type( ... data = data ... )` unchanged.

**Where it hooks in — `Z_UI2_JSON2` (kernel reader):**
- `deserialize_int` (`src/z_ui2_json2.clas.abap:494`) does `lo_reader->next_node( )` then `restore_type`.
- Path navigation is cleaner here: walk `reader->node-name` at each `open_object` level (mirrors the existing loop at `src/z_ui2_json2.clas.abap:1301`), calling `reader->skip_node( )` for non-matching members (same primitive the WA2 skip_node fix relies on) until the target segment is reached, then hand off to `restore_type`.

**Performance — when `PATH` is NOT supplied (the hot path, must stay neutral):**
- Guard with a single `IF path IS NOT INITIAL.` around the entire pre-step. When empty, the added cost is one `IS INITIAL` test per top-level `deserialize`/`generate` call — not per node, not per field. This is immeasurable against the existing per-call setup (RTTI describe, `while_offset_not_cs` BOM scan).
- **Requirement**: the path-splitting regex/`SPLIT` must run **only inside** the `path IS NOT INITIAL` branch. Do not compile or split at construction time. Confirm the `Z_UI2_JSON_PERF` baseline scenarios are unchanged (target: 0% delta; anything >5% on a previously-neutral scenario blocks the change per CLAUDE.md).

**Performance — when `PATH` IS supplied:**
- Net cost is *sub-linear in the skipped volume vs. the current workaround*: today the wrapper-structure approach parses AND type-converts the outer envelope; path-skip parses the envelope tokens but does **no** RTTI lookup or MOVE for skipped members. So the feature is faster than the workaround it replaces, not just more convenient.
- One-time cost: split `PATH` into segments (bounded, tiny — typically 1–3 segments). Reuse the `Z_UI2_DATA_ACCESS` `so_regex_hier` pattern (`src/z_ui2_data_access.clas.abap:307`) only if array indexing is in scope; for the object-member-only v1, a plain `SPLIT path AT '-'` is cheaper and sufficient.

**Scope for v1 (recommended)**: object-member traversal only (no `[n]` indexing). Covers the OData `d-results` canonical case. Array indexing deferred to a follow-up — it complicates the offset parser's skip logic (must count array elements) with little added demand.

**Open question**: behavior when a path segment is not found. Options: (a) return initial/unchanged `data` silently, (b) raise `CX_SY_MOVE_CAST_ERROR` with the missing segment in `source_typename`, gated on `STRICT_MODE`. Recommend (b)-under-strict / (a)-otherwise, matching the existing strict-mode contract.

---

### 1.3 Strict-on-unknown-fields — **IMPLEMENTED**

**Status**: Implemented in both editions as constructor parameter `DISALLOW_UNKNOWN` (default `abap_false`), a **sub-option of `STRICT_MODE`**: it has effect only when `STRICT_MODE = abap_true` as well. In that combination, a JSON key with no matching ABAP component raises `CX_SY_MOVE_CAST_ERROR` (offending key in `source_typename`) instead of being silently skipped. `strict_mode` alone keeps its existing behavior (type mismatches raise, unknown keys tolerated). Constructor-only; performance-neutral when off. Rollout to `/UI2/CL_JSON` pending.

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

### 1.10 `disable_string_type_detect` — opt out of date/time autodetection in GENERATE / `REF TO DATA` — **IMPLEMENTED**

**Status**: Implemented in `Z_UI2_JSON` and `Z_UI2_JSON2` (constructor-only, default `abap_false` → fully backwards-compatible). When `abap_true`, all JSON quoted strings stay `STRING` in GENERATE / `REF TO DATA` mode — no date/time/timestamp inference. Guarded by a single test in the string branch of `generate_int` (perf-neutral when unused). Rollout to `/UI2/CL_JSON` pending.

**Problem**: When deserializing JSON into `REF TO DATA` (GENERATE mode), the class auto-detects ABAP types from JSON values. A quoted string matching `YYYY-MM-DD` is inferred as type `D` (ABAP date) and the hyphens are stripped (ABAP date is `YYYYMMDD`). False-positives for IDs, version strings, and codes.

**Requested**: An optional constructor parameter (e.g. `disable_string_type_detect`, default `abap_false`) that, when `abap_true`, keeps all JSON quoted strings as `STRING` — disabling date/time/timestamp inference entirely.

**Reported twice**: Tiwari (Jul 7, `"0133-01-01"` treated as date after PL21), Chizhenko (Jul 21, `"2026-07-21"`). Same root cause as the PL19 date-detection change (Note 3414589). KB: `kb/incoming/2026-07-21-chizhenko-deserialize-hyphen-date-autodetect.md`, `kb/incoming/2026-07-07-tiwari-date-detection-regression.md`.

**Follow-up doc task**: add a FAQ entry with the `YYYY-MM-DD` → date false-positive example and the new flag as the fix.

---

## Part 2: Open Bugs

### 2.1 ENUM deserialization throws on BASIS < 7.51 instead of silently ignoring — **FIXED**

**Status**: Fixed in `Z_UI2_JSON` (`Z_UI2_JSON2` requires SAP_BASIS 7.57 where `CL_ABAP_XSD` always exists, so the gap cannot occur there). Rollout to `/UI2/CL_JSON` pending.

**Source**: `src/z_ui2_json.clas.abap` (enum branch of `restore_type`)

Serialization works on all BASIS levels. For deserialization, the class tries `CL_ABAP_XSD=>TO_VALUE` dynamically; when this call fails (BASIS < 7.51 where `CL_ABAP_XSD` doesn't exist), it previously called `throw_error`. Note 2650040 states: "From SAP_BASIS 7.51, below, the enums are **ignored**." The `CATCH cx_sy_dyn_call_error` block now `RETURN`s (the JSON value is already consumed by `eat_name` above), matching the documented "silently ignore" contract. Cannot be unit-tested on ER1 (7.57) — the ignore path only triggers below 7.51.

---

### 2.2 DECFLOAT16 / DECFLOAT34 zero serializes as `null` — **fixed in both editions, /UI2/CL_JSON rollout pending**

**Status**: Fix implemented + committed in `Z_UI2_JSON` (commits `f51a219`, `cf240e5`) and ported to `Z_UI2_JSON2` (decfloat typekind aliases + numeric serialization branch; 61/61 tests pass). PENDING rollout: copy to `/UI2/CL_JSON` on U1Y (client 010), then correction note. Target SAP_BASIS 921/816/824 (confirmed by Yakovlev 2026-07-10).

**Source**: `src/z_ui2_json.clas.macros.abap` (dump type CASE branches).

A `DECFLOAT16`/`DECFLOAT34` field with value `0` serialized as JSON `null` instead of `0`, because decfloat kinds were not in the explicit numeric CASE branches. Fix adds `WHEN e_typekind-decfloat16 OR e_typekind-decfloat34` to the numeric path and uses local `e_typekind` constants so it compiles on older releases without the kernel type constants.

Reported by Yakovlev, Mikhail (2026-07-10). KB: `kb/incoming/2026-07-10-yakovlev-decfloat-null-serialization.md`, `kb/incoming/2026-07-10-decfloat-null-bug.md`.

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
