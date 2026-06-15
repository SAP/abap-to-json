# Migration Decisions Log — Z_UI2_JSON → Kernel JSON API

All architectural decisions made during the migration of `Z_UI2_JSON` to `Z_UI2_JSON2` using `IF_JSON_READER` / `IF_JSON_WRITER`. The migration is complete. This log is retained for reference.

- **Source**: `Z_UI2_JSON` VERSION 23
- **Result**: `Z_UI2_JSON2` VERSION 1 (independent class, coexists with V23)
- **Minimum SAP_BASIS**: 7.57

---

## Decision 1 — Target SAP_BASIS Version

**Decision**: Minimum SAP_BASIS 7.57.

**Rationale**: The `SJSON` package (`IF_JSON_READER`, `IF_JSON_WRITER`, `CL_JSON_STRING_READER`, `CL_JSON_STRING_WRITER`) is stable from 7.57. Users on older systems keep `Z_UI2_JSON` (VERSION 23+).

---

## Decision 2 — Raw JSON Passthrough (e_typekind-json)

**Decision**: Use `lcl_util=>read_json_to_string( reader )` for raw JSON passthrough. This manually walks the tree with depth tracking, writing to a fresh `cl_json_string_writer`.

**Rationale**: `skip_node(writer)` does not work correctly when the reader is positioned on an object member mid-document — confirmed by testing. The workaround is encapsulated in one utility method. When `skip_node(writer)` is fixed in the kernel, it becomes a one-line replacement.

---

## Decision 3 — FORMAT_OUTPUT Output Difference Accepted

**Decision**: Pretty-printed JSON output (`FORMAT_OUTPUT`) differs from the old implementation. No attempt is made to replicate the old CRLF+2-space indentation.

**Rationale**: `IF_JSON_WRITER` has its own indentation logic. Replicating the old format would add complexity for no functional benefit. FORMAT_OUTPUT is a display feature, not a contract.

---

## Decision 4 — Public API Methods Removed

| Method | Reason |
|--------|--------|
| `DUMP` | Thin wrapper over `SERIALIZE`; use `SERIALIZE` directly |
| `BOOL_TO_TRIBOOL` | Utility unrelated to JSON; callers can inline |
| `TRIBOOL_TO_BOOL` | Same |
| `UNESCAPE` | `IF_JSON_READER` returns unescaped values natively |
| `GET_INDENT` | `IF_JSON_WRITER` handles indentation internally |
| `GET_CONVEXIT_FUNC` | Moved to `lcl_util=>get_convexit_func()` (private) |
| `EDM_DATETIME_TO_TS` | Moved to `lcl_util=>read_edm_datetime()` |
| `ESCAPE` | Callers use `escape()` BIF directly |
| `RAW_TO_STRING` / `STRING_TO_RAW` | Use `cl_abap_codepage` directly |
| `XSTRING_TO_STRING` / `STRING_TO_XSTRING` | Use `cl_http_utility` directly |

---

## Decision 5 — Protected/Private Method Signature Changes

**`RESTORE` / `RESTORE_TYPE`**: Changed from `IMPORTING json/length/offset` to `IMPORTING reader TYPE REF TO if_json_reader`. The reader maintains its own position state.

**`DUMP_TYPE`** (virtual method for subclass override): Changed from `RETURNING r_json TYPE json` to `IMPORTING writer TYPE REF TO if_json_writer, name TYPE string optional`. Subclasses write directly to the writer. Call `super->dump_type(...)` for fallback.

**`DUMP_INT` / `DUMP_SYMBOLS`**: Changed from returning string to accepting `writer TYPE REF TO if_json_writer`. Eliminates intermediate string allocation.

---

## Decision 6 — GENERATE_INT_R Replaces GENERATE_INT_EX

Private method `GENERATE_INT_R( reader TYPE REF TO if_json_reader )` is the reader-based workhorse. `GENERATE_INT` creates a reader and delegates. `GENERATE_INT_EX` is removed.

---

## Decision 7 — Macro Strategy

Kept the `dump_type_int` macro for performance (avoids virtual method call per field). Rewrote its body to emit writer calls. Deleted: `escape_json`, `eat_white`, `eat_char`, `eat_name`, `eat_number`, `eat_bool`, `while_offset_cs`, `while_offset_not_cs`, `restore_reference`, `restore_reference_ex`, `throw_error`, `format_list_output`, `create_regexp`.

---

## Decision 8 — escape_json Removal

`IF_JSON_WRITER` handles all string escaping internally. The macro was removed; `dump_type` method uses the `escape()` BIF directly.

---

## Decision 9 — lcl_util Expansion

Moved from class to `lcl_util`: `get_convexit_func`, RTTI descriptor caches for UTCLONG/ENUM types, `detect_typekind`, all `SO_REGEX_*` and `SO_TYPE_*` class-data. Reduces public/protected surface of the main class.

---

## Decision 10 — Dynamic Calls Replaced

Dynamic calls for UTCLONG and ENUM typekinds replaced with direct static calls (BASIS 7.57 guaranteed). `CREATE OBJECT TYPE (mc_me_type)` retained as intentional polymorphism; `mc_me_type` retained for subclass detection in `constructor`.

---

## Decision 11 — VERSION Reset to 1

Z_UI2_JSON2 VERSION constant is **1** (not 24, which was an internal development number). Both classes have independent version tracks.

---

## Decision 12 — GEN_OPTIMIZE Removed (Always Optimized)

`GEN_OPTIMIZE` parameter removed from `GENERATE`, `DESERIALIZE`, and constructor. Optimized generation is always active. The non-optimized path (REF TO data wrappers) was removed as a new class taking incompatible changes.

---

## Decision 13 — Trailing Commas Not Supported

Input JSON must be valid per RFC 8259. Trailing commas cause parse errors. Documented as a requirement; tolerant mode requested from IF_JSON_READER author (see `if_json_reader_writer_requests.md`).

---

## Decision 14 — Performance: Writer API Constrains String Operations

String templates and `&&` concatenation used for timestamp/date/time formatting passed to the writer. Fixed-length `TYPE c` for intermediate timestamp storage where offset/length access is needed.

---

## Decision 15 — Cyclic References Serialize as null

Cyclic data/object references serialize as `null` (was `{}`). `null` correctly represents "cannot be serialized"; `{}` was misleading.

---

## Decision 16 — DUMP_TYPE Writes Directly to Writer

`DUMP_TYPE` signature changed to accept `WRITER` and `NAME` parameters, writing directly rather than returning a JSON string. Eliminates the serialize-then-reparse round-trip for extended classes.

---

## Decision 17 — Struct Cache Without Level Key

Removed `level` from `mt_struct_cache` key. `IF_JSON_WRITER` handles indentation natively — the level was only needed when symbols embedded indentation strings. Removing `level` eliminates duplicate cache entries for the same type at different nesting depths.

---

## Decision 18 — TRY/CATCH Restructured for Deserialization

`restore_type_int` extracted as private method without TRY. `restore_type` is a thin wrapper adding TRY/CATCH. Recursive calls use `restore_type_int` directly. SAT showed 7.37M TRY entries consuming 14M µs; this restructuring reduced TRY overhead by 68%.

---

## Decision 19 — detect_typekind Generalized

`lcl_util=>detect_typekind` accepts `cl_abap_typedescr` (base class) instead of `cl_abap_elemdescr`. `CONVEXIT` parameter is optional. Handles non-elementary types by returning `type_kind` directly.

---

## Decision 20 — Initial Value Defaults Without Quotes

`INITIAL_TS`, `INITIAL_DATE`, `INITIAL_TIME` constructor defaults stored without surrounding quotes. The writer adds quotes automatically via `write_string()`.

---

## Decision 21 — Performance Trade-off: Serialization vs Deserialization

Accepted 7-35% serialization overhead from `IF_JSON_WRITER` method calls in exchange for 17-33% deserialization improvement and 73% generation improvement. Serialization gap is the inherent cost of one `write_*()` call per field vs. V23's direct `CONCATENATE`. Cannot be eliminated without bypassing the writer API.

---

## Decision 22 — Eliminate open_member/close_member Calls

All `open_member`/`close_member` calls removed. Member names are passed directly to `open_object( name = )`, `open_array( name = )`, and all `write_*( name = )` calls. Confirmed by IF_JSON_WRITER author. In practice, performance impact was not measurable — the gap vs V23 is dominated by the remaining `write_*` calls themselves.

---

## Decision 23 — JSONX_CP Removed

`JSONX_CP` parameter removed from `DESERIALIZE` and `DESERIALIZE_INT`. `CL_JSON_XSTRING_READER` handles encoding natively (UTF-8 default, which is the JSON standard). The parameter had no effect on xstring deserialization.
