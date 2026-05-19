# Feature Requests, Bug Reports & Usage Patterns

Compiled from: repository documentation, FAQ, history, migration decisions, GitHub issues (https://github.com/SAP/abap-to-json/issues), and SAP system usage analysis (3,053 references across 385 objects in ER1).
Prepared for the June 2026 meeting with the ABAP kernel team (Stefan et al.).

---

## Priority Summary

| # | Item | Type | Priority | Source |
|---|------|------|----------|--------|
| 1 | Full ANY JSON in `CALL TRANSFORMATION` (kernel) | Feature | Highest | SAP Influence, docs/faq.md |
| 2 | `IS_COMPRESSABLE` → built-in constructor params | Usability | High | ER1 analysis: #1 subclassing reason |
| 3 | `it_no_compress_fields` / `it_always_compress_fields` | Feature | High | ER1: all subclasses override IS_COMPRESSABLE |
| 4 | Custom boolean type via static API (no subclassing) | Feature | High | FAQ: top question |
| 5 | LLM noise stripping utility (`strip_markdown_fences`) | Usability | High | ER1: copy-paste boilerplate in many classes |
| 6 | Streaming/chunked serialization for huge data | Feature | High | FAQ: memory dumps |
| 7 | `DESERIALIZE` success/failure return (no IS-INITIAL workaround) | Usability | High | ER1: IS-INITIAL pattern widespread |
| 8 | `INITIAL_DATE`/`INITIAL_TIME`/`INITIAL_TS` on static `SERIALIZE` | Usability | Medium | FAQ: maintainer invite |
| 9 | `IS_VALID( json )` check method | Usability | Medium | Inferred from usage patterns |
| 10 | Tolerant mode for trailing commas (also: kernel request) | Compat | Medium | PL14 history, Z_UI2_JSON2 regression |
| 11 | No null vs absent field distinction | Feature | Medium | REST API needs |
| 12 | ENUM deserialization (all BASIS levels) | Bug/Feature | Medium | Source comments |
| 13 | ASSOC_ARRAYS composite key separator | Config | Low | basic.md |
| 14 | CURR/CUKY pair formatting | Feature | Low | FAQ: declined |
| 15 | `Z_UI2_DATA_ACCESS` — promote/document for post-GENERATE use | Docs | Low | ER1: consumers unaware |
| 16 | Deprecate `pretty_name = abap_true` boolean form | Cleanup | Low | ER1: still in use |

---

---

## Part 1: Missing Features (Not Yet Implemented)

### 1.1 Full ANY JSON support in CALL TRANSFORMATION (kernel ask — highest priority)

**Frequency**: Industry-wide — dedicated SAP Influence campaign exists.

Users want `CALL TRANSFORMATION id` to handle arbitrary (non-ABAP-normalized) JSON with name mapping, booleans as `true`/`false`, ISO 8601 timestamps, camelCase keys, etc. — at kernel speed.
Currently `CALL TRANSFORMATION id` only handles SAP's own normalized JSON format.

The maintainer actively linked a customer influence campaign: https://influence.sap.com/sap/ino/#idea/367772

This is effectively the same goal as Enhancement 6 in `docs/if_json_reader_writer_requests.md` (bulk tree handoff), approached from a different angle.

---

### 1.2 Custom boolean type auto-detection without subclassing

**Frequency**: High — one of the most common user questions.

Users have `CHAR 1` boolean types not in the hardcoded list (`ABAP_BOOLEAN`, `BOOLEAN`, `XFELD`, etc.) and want automatic `true`/`false` conversion without subclassing.

**Workaround**: Pass custom `BOOL_TYPES` string to constructor, or subclass and override `mc_bool_types`. Both require dropping static methods or subclassing.

**Suggestion**: Expose `BOOL_TYPES` as a parameter on static `SERIALIZE`/`DESERIALIZE`.

---

### 1.3 Streaming / chunked serialization for huge tables

**Frequency**: High — dedicated FAQ entry, multiple user reports.

Users get `SYSTEM_NO_ROLL`, `STRING_SIZE_TOO_LARGE`, `MEMORY_NO_MORE_PAGING` when serializing tables producing >1GB JSON strings. Memory exceptions are not catchable, so pre-validation is impossible.

**Workaround**: Manually split table into chunks before calling the class. No helper infrastructure provided.

**Suggestion**: A `serialize_chunk( data start end )` API or callback-based serialization to avoid materializing the full string.

---

### 1.4 INITIAL_DATE / INITIAL_TIME / INITIAL_TS on static SERIALIZE

**Frequency**: Medium — acknowledged in FAQ as "tell me if multiple people ask".

The `INITIAL_DATE`, `INITIAL_TIME`, `INITIAL_TS` parameters controlling JSON output for initial ABAP date/time values are only available on the instance constructor, not on `SERIALIZE`. Teams using static methods everywhere must switch to instance methods just for this.

**Current FAQ note**: "If I get multiple requests regarding extending SERIALIZE with these defaults, I will do it."

**Suggestion**: Add these three parameters to the static `SERIALIZE` method signature.

---

### 1.5 Tolerant/lenient parsing for trailing commas

**Frequency**: Medium-high — V23 had silent tolerance (added in PL14 after user demand); Z_UI2_JSON2 re-exposes as strict RFC 8259.

JSON with trailing commas (`{"a":1,}`, `[1,2,]`) is common from JavaScript sources and copy-paste. V23 fixed this in PL14; Z_UI2_JSON2 breaks it again due to the kernel reader.

Also filed as Enhancement 1 in `docs/if_json_reader_writer_requests.md` against `IF_JSON_READER`.

---

### 1.6 Configurable ASSOC_ARRAYS composite key separator

**Frequency**: Low.

The key separator for composite multi-key tables serialized as associative arrays is hardcoded as `-` (`MC_KEY_SEPARATOR`). If any key value contains `-`, the round-trip breaks silently.

**Suggestion**: Add `key_separator TYPE string OPTIONAL` to the constructor and static methods.

---

### 1.7 No distinction between JSON `null` and absent field

**Frequency**: Medium-high — important for REST APIs that distinguish null vs absent.

JSON `null` deserializes to the ABAP initial value with no way to distinguish "explicitly null" vs "field was absent". No nullable wrapper type or null-indicator field pattern supported.

---

### 1.8 Currency/quantity field pair formatting (CURR/CUKY)

**Frequency**: Medium — recurring question, maintainer explicitly declines default support.

Users ask to display `CURR` fields rounded/formatted relative to the associated `CUKY` currency key in JSON output. Maintainer cites implementation complexity and performance penalty.

**Workaround**: Subclass and override `DUMP_INT`/`RESTORE_TYPE` — non-trivial.

---

### 1.9 Deserialization of REF TO interface attributes

**Frequency**: Medium-high — explicit FAQ entry.

Classes with `TYPE REF TO <interface>` attributes cannot be deserialized because the deserializer cannot determine which concrete class to instantiate.

---

### 1.10 Field ordering in GENERATE output

**Frequency**: Medium — recurring question.

Users ask for generated ABAP structures to have fields in the same order as in the JSON. Current behavior uses alphabetical order (for cache normalization).

**Workaround**: Pre-populate `mt_struct_type` cache via subclass constructor.

---

## Part 2: Bugs / Incorrect Behavior

### 2.1 NBSP (U+00A0) causes parse failure in Z_UI2_JSON2

**Severity**: Medium — real-world JSON from web sources contains NBSP; V23 fixed this in PL17.

Z_UI2_JSON2 re-exposes as a kernel defect (`IF_JSON_READER` does not treat NBSP as whitespace). No workaround.

Filed as Bug 2 in `docs/if_json_reader_writer_requests.md`.

---

### 2.2 `skip_node(writer)` broken mid-document

**Severity**: High — required a 70-line workaround in production code.

When `IF_JSON_READER` is positioned on an object member, `skip_node(writer)` does not correctly pipe the value subtree. Result is empty or malformed.

Filed as Bug 1 in `docs/if_json_reader_writer_requests.md`.
Workaround: `lcl_util=>read_json_to_string` in `z_ui2_json2.clas.locals_imp.abap`.

---

### 2.3 ASSOC_ARRAYS key values not escaped/unescaped on PL20 and older

**Severity**: Medium — fixed in PL21. Silent data corruption on older patch levels.

When ASSOC_ARRAYS is used, table key values containing special characters (quotes, backslash, Unicode) were not escaped in JSON output and not unescaped on read.

---

### 2.4 Special characters in JSON attribute names not escaped/unescaped

**Severity**: Medium — known limitation, deliberately not fixed for performance.

Attribute names with `"`, `\`, etc. are not escaped in serialized output and not unescaped on read. Affects consumers receiving JSON from external systems with escaped attribute names.

---

### 2.5 Empty objects `{}` not initialized in GENERATE on PL19 and older

**Severity**: Medium — fixed in PL20. Silent data corruption (previous field value bleeds through).

---

### 2.6 Cyclic references: V23 emits `{}`, V1 emits `null`

**Severity**: Low — `{}` is semantically incorrect (misleads consumers). Fixed in Z_UI2_JSON2 (emits `null`).

---

## Part 3: Usability Pain Points

### 3.1 Static methods cannot be subclassed — verbose wrapper pattern required

**Frequency**: Extremely common — documented extensively in `docs/class-extension.md`.

`SERIALIZE` and `DESERIALIZE` are static and cannot be redefined. Subclasses must define their own static wrapper methods (`SERIALIZE_EX`, etc.), copy the full signature, and maintain them.

**Suggestion**: Class-specific convenience subclass pattern should be codified as a project template.

---

### 3.2 Post-GENERATE data access requires verbose dynamic programming

**Frequency**: Very high — `Z_UI2_DATA_ACCESS` helper class was built specifically for this.

`GENERATE` produces `REF TO data` requiring verbose `ASSIGN COMPONENT ... FIELD-SYMBOL` chains. Even with `Z_UI2_DATA_ACCESS`, path-string navigation adds overhead.

---

### 3.3 Private/protected attribute serialization requires FRIENDS declaration

**Frequency**: Medium — documented in `docs/advanced.md` but NOT in the FAQ.

To serialize private/protected attributes of ABAP objects, the caller must declare the serializer class as a FRIENDS of the target class — not possible without modifying the target.

---

### 3.4 Class instance deserialization fails silently when constructor has required parameters

**Frequency**: Medium — documented in `docs/advanced.md:157` but NOT in the FAQ.

Deserializing JSON into ABAP class instances with mandatory constructor parameters silently fails. No exception, no data.

**Suggestion**: Add to FAQ; consider throwing a specific exception in this case rather than silently producing nothing.

---

### 3.5 Coarse error reporting in strict mode

**Frequency**: High — any developer doing error diagnosis hits this.

All deserialization errors are `CX_SY_MOVE_CAST_ERROR` — no structured access to which field failed, at what JSON path. PL19 partially improved this but Z_UI2_JSON2 degrades it further (kernel reader throws before delivering field names).

Filed as Enhancement 3 in `docs/if_json_reader_writer_requests.md`.

---

### 3.6 No `IS_VALID( json )` check method

**Frequency**: Medium — inferred from STRICT_MODE usage patterns.

To validate JSON without side effects, users must attempt DESERIALIZE into a dummy target and catch exceptions. No side-effect-free validation method exists.

**Suggestion**: `CLASS-METHODS is_valid IMPORTING json TYPE string RETURNING VALUE(rv_valid) TYPE abap_bool.`

---

### 3.7 ENUM deserialization not supported — throws silently on old BASIS

**Frequency**: Low-medium — ENUM types increasingly used in modern ABAP.

Serialization works (enums emit underlying values). Deserialization falls through to `throw_error` when the dynamic call fails on old BASIS. Not in FAQ.

Source: `src/z_ui2_json.clas.abap:2350`, `src/z_ui2_json2.clas.abap:1791`

---

### 3.8 UTCLONG ignores TS_AS_ISO8601 flag (always emits ISO 8601)

**Frequency**: Low-medium — inconsistency surprises users comparing TIMESTAMP vs UTCLONG behavior.

`TIMESTAMP` respects `TS_AS_ISO8601`; `UTCLONG` always emits ISO 8601 regardless of the flag. Not documented.

---

## Part 4: Z_UI2_JSON2 Migration-Specific Issues

These affect consumers migrating from Z_UI2_JSON (V23) to Z_UI2_JSON2 (V1):

| # | Issue | Documented |
|---|-------|-----------|
| M1 | `FORMAT_OUTPUT` produces different whitespace (1-space vs 2-space+CRLF) | `z_ui2_json2.md` |
| M2 | `GEN_OPTIMIZE` parameter removed — code expecting REF TO data wrappers silently breaks | `z_ui2_json2.md` |
| M3 | Trailing comma tolerance removed (strict RFC 8259) | `z_ui2_json2.md` |
| M4 | NBSP parse failure (kernel defect) | `z_ui2_json2.md` |
| M5 | Error path granularity reduced ($.struct instead of $.struct.field) | `z_ui2_json2.md` |
| M6 | `DUMP_TYPE` signature changed (added `WRITER` + `NAME` params) — all subclasses must be updated | `z_ui2_json2.md` |
| M7 | Cyclic references now emit `null` instead of `{}` (semantically correct but a change) | `z_ui2_json2.md` |

---

## Part 5: GitHub Issues (from https://github.com/SAP/abap-to-json/issues)

All issues are currently closed. Key findings:

### Issue #11: Ignore Escaping in Serialization (closed)
- **Request**: Option to disable JSON escaping for specific values or entirely.
- **Context**: Windows file paths get double-backslash-escaped; receiving system cannot handle it.
- **Status**: Closed without documented resolution (likely declined or out of scope).
- **Relevance**: This is the "raw string" use case — covered by Enhancement 5 (`write_string_unescaped`) and Enhancement 6 (`R` kind in bulk tree API) in `docs/if_json_reader_writer_requests.md`. The user-facing ask is: "I know this value needs no escaping, let me opt out."

### Issue #12: Scientific notation from DATA_ACCESS deserialization (closed)
- **Request**: Numeric value `93.5` becomes `9.35E+01` after deserializing via `/UI2/CL_DATA_ACCESS`.
- **Context**: User bypasses typed structures and accesses dynamically via data access class.
- **Status**: Closed, no documented resolution. Likely a display/conversion issue in DATA_ACCESS, not the serializer itself.
- **Relevance**: Floating point TYPE F always serializes with scientific notation in ABAP; when accessing via dynamic data access, the type is not known to guide formatting. Users expect `93.5` but get ABAP's native F representation.

### Issue #19: Apache Parquet support (closed, declined)
- **Request**: Serialize ABAP data to Apache Parquet format (compressed columnar).
- **Status**: Closed — out of scope (this is a JSON-only library).

### Issue #20: Unit test off-by-one-second in OData date deserialization (closed)
- **Request**: Bug report — OData `/Date(1689670138545)/` deserializes with 1-second discrepancy.
- **Context**: Millisecond rounding when converting OData timestamp to ABAP `TIMESTAMP` type.
- **Status**: Closed. Likely fixed in a patch level.
- **Relevance**: Timestamp precision handling is subtle and error-prone.

### Issue #7: Endless loop on invalid JSON (closed)
- **Request**: Bug — `z_ui2_json=>generate('{"error":"n \"<\"\">')` causes infinite loop in `lcl_util=>read_string`.
- **Context**: Malformed JSON with unescaped special chars. Appeared in PL19, not in PL16.
- **Status**: Closed (fixed in subsequent patch level).
- **Relevance**: Invalid JSON robustness — the class should never hang; Z_UI2_JSON2 avoids this entirely since the kernel reader handles it.

---

## Part 6: SAP System Usage Patterns (ER1)

Analysis of 3,053 references across 385 distinct objects — both Z* custom development and standard SAP packages (APPL_MM_PUR_OA, CRMS4, BW4_REST, S_CFD_*, etc.).

### 6.1 Pre-processing Patterns (very common)

**Stripping markdown code fences before deserialization** — found in multiple LLM-integration classes:
```abap
REPLACE ALL OCCURRENCES OF '```json' IN rv_json WITH ''.
REPLACE ALL OCCURRENCES OF '```'     IN rv_json WITH ''.
CONDENSE rv_json.
/ui2/cl_json=>deserialize( ... ).
```
Any code that asks an LLM to "output JSON" hits this — the LLM wraps output in markdown fences. This is a copy-paste boilerplate pattern across many classes.

**Stripping CR/LF / newlines from LLM responses** before deserializing:
```abap
REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN response WITH space.
REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN response WITH space.
/ui2/cl_json=>deserialize( EXPORTING json = response pretty_name = ... CHANGING data = ... ).
```

**Stripping single quotes from OData key parameters** before deserializing.

**JSON shape rewriting** (`ZCL_SRF_HTTP_CLIENT=>JSON_COMPATIBILITY_CONVERSION`): Examines the first key in a JSON response — if it's `"DATA"`, rewrites `[...]` to `{...}` or vice versa to match the ABAP target type shape. Works around the limitation that `DESERIALIZE` cannot handle shape mismatch between a `{"data":[...]}` envelope and a flat ABAP target type.

**Suggested improvement**: A `pre_clean` option or a documented utility for stripping common noise (markdown fences, CR/LF) would eliminate widespread boilerplate. Worth adding a `strip_markdown_fences( json )` class method.

---

### 6.2 Post-processing Patterns

**Embedding serialized JSON as a field in OData entities or DB columns** ("JSON-in-a-field"):
```abap
er_entity-element_json = /ui2/cl_json=>serialize( lt_or_properties ).
er_entity-source       = /ui2/cl_json=>serialize( lt_source ).
```

**Building hybrid JSON by embedding serialized sub-structures into string templates**:
```abap
DATA(lv_errors_json) = /ui2/cl_json=>serialize( data = it_errors compress = abap_false
  pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
rv_json = |\{ "success": { lv_success_flag }, "errors": { lv_errors_json }, "message": "{ lv_message }" \}|.
```
The consumer serializes a sub-structure and manually assembles the envelope in a string template. Signal that a "named root wrapper" or partial document building capability would be valued.

**Serializing ABAP data as LLM prompt context** — very frequent in AI-related code:
```abap
DATA(lv_context) = /ui2/cl_json=>serialize( data = lt_comparison compress = abap_true
  pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
" pass lv_context as variable to cl_aic_islm_prompt_tpl_factory
```
The combination `compress = abap_true` + `pretty_mode-low_case` dominates this use case (compact tokens, lowercase keys that LLMs understand better).

---

### 6.3 Parameter Combinations Observed

| Combination | Frequency | Context |
|---|---|---|
| `pretty_name = pretty_mode-camel_case` only | **Very high** | REST/BTP/Cloud API payloads |
| `pretty_name = pretty_mode-camel_case` + `name_mappings` | **High** | camelCase almost right, few fields need explicit overrides (e.g., `@context`, `senderRegistrationID`) |
| `compress = abap_true` + `pretty_mode-low_case` | Medium | LLM prompt context — compact, lowercase |
| `pretty_mode-low_case` + `assoc_arrays = abap_true` | Seen | REST APIs returning dicts/maps |
| `pretty_name = abap_true` (deprecated boolean form) | Seen | Legacy/copy-paste — still in production code |
| No parameters | Common | Simple round-trips with trusted uppercase field names |
| Inline `name_mappings = VALUE #(...)` | Seen | Preferred for small one-off mappings |
| `pretty_mode-extended` | Rare | Complex field name handling |

**Note**: The deprecated boolean `pretty_name = abap_true` form is still in active production use. A deprecation warning in the docs would help.

---

### 6.4 Error Handling Patterns

**No error handling (most common by far)**: The vast majority of `DESERIALIZE` calls have no TRY/CATCH. Consumers rely on non-strict mode silently ignoring mismatches — which works but produces silent data loss.

**IS INITIAL check after DESERIALIZE** (used as validation):
```abap
/ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = abap_struc->* ).
IF abap_struc->* IS INITIAL.
  RAISE EXCEPTION ...
ENDIF.
```
This compensates for DESERIALIZE silently producing empty results on mismatch.

**Two-attempt pattern** (try response type, then error type):
```abap
/ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = e_data ).
IF e_data IS INITIAL AND e_error IS REQUESTED.
  /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = e_error ).
ENDIF.
```

**Suggested improvement**: A variant that returns a success/failure boolean, or better documentation of STRICT_MODE for "I want to know when the JSON didn't match", would eliminate the IS-INITIAL workaround pattern.

---

### 6.5 Subclassing Patterns

Three confirmed subclasses found — all override `IS_COMPRESSABLE`:

**`ZCL_TEST_007`**: Overrides `IS_COMPRESSABLE` to suppress compression for a specific list of field names (`MT_NO_COMPRESS_FIELDS`). Note: declared as CLASS-DATA (bug — all instances share the same field list).

**`ZCL_UI2_JSON`**: Overrides `IS_COMPRESSABLE` to always return `abap_false` (never compress). Has dead code for a `MT_MANDATORY_FIELDS` inverse-compression concept, never completed.

**`ZCL_TAU_JSON_TRANSLATOR_GK`**: Configuration-driven wrapper with a local subclass that reads name mappings and compression rules from DB table `TAU_JSON_MAP` keyed by process ID.

**Key finding**: `IS_COMPRESSABLE` is the dominant — essentially the *only* — extension point that consumers use in practice. Both use cases observed are:
1. Exclude specific named fields from compression (don't compress field X even if empty).
2. Always compress regardless of type.

**Suggested improvement**: Add `it_no_compress_fields TYPE string_table OPTIONAL` and `it_always_compress_fields TYPE string_table OPTIONAL` to the constructor. This would eliminate the most common subclassing reason entirely.

---

### 6.6 Wrapper Patterns

- **Interface adapter** (`ZCL_WL_JSON_DESERIALIZATION`): 41-line class that wraps static methods for dependency injection / testability. Common pattern.
- **Configuration-driven wrapper** (`ZCL_TAU_JSON_TRANSLATOR_GK`): One instance per "process", reads field rules from DB.
- **Service-layer deserializer** (`Z_ECOM_SRV_DESERIALIZER`): Wraps DESERIALIZE with IS-INITIAL check, raises exception on mismatch. Used through an interface by multiple callers.
- **REST endpoint adapters**: Many classes use the class as a one-liner within HTTP handlers.

---

### 6.7 GENERATE and Dynamic Access

`GENERATE` not found called directly in sampled code, but the post-GENERATE navigation pattern is common:
```abap
ASSIGN lr_response->* TO <json>.
ASSIGN COMPONENT 'LLM_RESPONSE' OF STRUCTURE <json> TO <lv_any>.
ASSIGN <lv_any>->* TO <lv_content>.
```
Consumers DESERIALIZE into `REF TO DATA` (untyped) and then navigate the result generically with `ASSIGN COMPONENT`. This is the same problem that `Z_UI2_DATA_ACCESS` was built to solve, but consumers don't know about or use it.

---

### 6.8 JSON Passthrough (`TYPE /UI2/CL_JSON=>JSON`)

The type alias `TYPE /UI2/CL_JSON=>JSON` is used correctly in the few places that bother, but most code uses `TYPE STRING` directly. The alias is not widely adopted in practice.

---

## Part 7: IF_JSON_READER / IF_JSON_WRITER Requests for ABAP Kernel Team

See `docs/if_json_reader_writer_requests.md` for full details. Summary for June meeting:

| # | Type | Priority | Status |
|---|------|----------|--------|
| Bug 1 | `skip_node(writer)` broken on member positions | High | Open |
| Bug 2 | NBSP not treated as whitespace | Medium | Open |
| Enh 1 | Tolerant mode for trailing commas | Medium | Open |
| Enh 2 | `get_offset()` on IF_JSON_READER | Medium | Open |
| Enh 3 | Structured parse error exception class | Low | Open |
| Enh 4 | Eliminate open_member/close_member overhead | **Done** | Confirmed + implemented |
| Enh 5 | `write_string_unescaped` for pre-validated content | Low-Medium | Open |
| Enh 6 | Bulk tree handoff (pre-resolved node table → kernel in one call) | High | Open |
