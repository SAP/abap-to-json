# Migration Plan: Z_UI2_JSON → IF_JSON_READER / IF_JSON_WRITER

## Overview

Migrate `Z_UI2_JSON` (VERSION 23, manual character-level JSON parsing) to use SAP's kernel-native
JSON API (`IF_JSON_READER` / `IF_JSON_WRITER`). The class is renamed to keep the same identifier
in `src/`; the migration produces the next VERSION in the same file.

**Starting point:** z_ui2_json2 from ER1 (VERSION 21) — a partial migration already done.
**Reference for correctness:** z_ui2_json VERSION 23 in `src/` — the authoritative bug-fix baseline.
**Minimum SAP_BASIS:** 7.57 (stable `SJSON` package, `UTCLONG`, `INT8`, `ENUM` typekind constants).

---

## Kernel API Reference

### Reader
| Class | Factory | Input |
|---|---|---|
| `CL_JSON_STRING_READER` | `cl_json_string_reader=>create( json )` | `STRING` |
| `CL_JSON_XSTRING_READER` | `cl_json_xstring_reader=>create( json )` | `XSTRING` |

Both implement `IF_JSON_READER`:

```abap
next_node()    RETURNING node TYPE if_json_node=>node   " advance + return node
current_node() RETURNING node TYPE if_json_node=>node   " peek without advancing
push_back()                                              " un-read last node
skip_node( writer TYPE REF TO if_json_writer OPTIONAL ) " skip subtree, optionally copy to writer
set_option( option  TYPE if_json_reader=>options
            value   TYPE abap_bool DEFAULT abap_true )
```

`IF_JSON_NODE=>node` structure:
```abap
type  TYPE if_json_node=>type   " open_object(1) close_object(2) open_array(3) close_array(4)
                                 " open_member(5) close_member(6) string(7) number(8)
                                 " boolean(9) null(10) final(11) error(12)
name  TYPE string               " field name for object members
value TYPE string               " raw value for string/number/boolean nodes (already unescaped)
```

Reader option: `if_json_reader=>option_member` — when set, emits `open_member`/`close_member`
events around each key-value pair. Not used in this migration (we read `node-name` on value nodes
directly, which is simpler).

### Writer
| Class | Factory | Output |
|---|---|---|
| `CL_JSON_STRING_WRITER` | `cl_json_string_writer=>create()` | `STRING` via `get_json()` |
| `CL_JSON_XSTRING_WRITER` | `cl_json_xstring_writer=>create( encoding )` | `XSTRING` via `get_json()` |

Both implement `IF_JSON_WRITER`:

```abap
open_object(  name TYPE string OPTIONAL )   " emit {  (or "name":{)
close_object()                               " emit }
open_array(   name TYPE string OPTIONAL )   " emit [
close_array()                               " emit ]
open_member(  name TYPE string )            " emit "name":
close_member()                              " (no-op in current kernel — open_member is sufficient)
write_string(  name TYPE string OPTIONAL  value TYPE string )
write_number(  name TYPE string OPTIONAL  value TYPE string )
write_boolean( name TYPE string OPTIONAL  value TYPE string )  " value = 'true' or 'false'
write_null(    name TYPE string OPTIONAL )
set_option( option TYPE if_json_writer=>options  value TYPE abap_bool DEFAULT abap_true )
```

Writer options: `option_linebreaks`, `option_indent` — enable pretty-print formatting.
All write methods return `REF TO if_json_writer` (self) for chaining but we do not use chaining.

### Exceptions
- `CX_JSON_READER_ERROR` inherits `CX_DYNAMIC_CHECK` — structural/parse errors from reader
- `CX_JSON_WRITER_ERROR` inherits `CX_DYNAMIC_CHECK` — writer state errors

---

## Source File Map

```
src/z_ui2_json.clas.abap           — class definition + implementation
src/z_ui2_json.clas.locals_imp.abap — lcl_util, lcl_test, lc_json_custom
src/z_ui2_json.clas.macros.abap    — DEFINE macros
src/z_ui2_json.clas.testclasses.abap — 62+ unit tests
src/z_ui2_json.clas.xml            — abapGit metadata (do not edit)
```

---

## Phases

### Phase 1 — Patch sync and baseline (prerequisite)

**Goal:** bring z_ui2_json2 (VERSION 21) fully up to VERSION 23 correctness before touching
architecture.

**Tasks:**

1. Port the EDM DateTime rounding fix from VERSION 23:
   - Location in VERSION 23: `RESTORE_TYPE`, `WHEN e_typekind-ts_iso8601 OR e_typekind-tsl_iso8601`
   - Fix: `IF type_descr->decimals EQ 0. data = trunc( tstml ). ENDIF.`
   - In z_ui2_json2 this lives in the same CASE branch but the value now comes from
     `reader->node-value` instead of `sdummy` — apply identically.

2. Port the packed-field-length-8 fix from VERSION 22/23:
   - Prevent false-positive timestamp detection for packed fields of length 8.
   - In `RESTORE_TYPE` string branch: check `type_descr->length` before treating packed as
     timestamp domain.

3. Port the generation-struct duplicate-name fix (VERSION 22):
   - In `GENERATE_STRUCT`: improved collision detection for similar names (e.g. "a"+"bc" vs "ab"+"c").

4. Confirm `DESERIALIZE_INT` no longer has `JSONX_CP` parameter — z_ui2_json2 already dropped it;
   xstring reader handles encoding internally (UTF-8 default, kernel-managed).

5. Run all 62+ unit tests against the patched z_ui2_json2. All must pass before Phase 2 starts.
   Any failures here = pre-existing bugs to fix first.

---

### Phase 2 — Complete deserialization migration

**Goal:** remove all `JSON` / `LENGTH` / `OFFSET` threading from internal restore methods.
After this phase, the only place a raw JSON string appears is as the entry-point parameter
to `DESERIALIZE_INT` and `GENERATE_INT` (public API).

#### 2a. Change `RESTORE` and `RESTORE_TYPE` signatures

**Old (z_ui2_json2 intermediate state):**
```abap
METHODS restore
  IMPORTING reader TYPE REF TO if_json_reader
            json   TYPE json
            length TYPE i
            ...
  CHANGING  data TYPE data OPTIONAL  offset TYPE i DEFAULT 0.

METHODS restore_type
  IMPORTING reader  TYPE REF TO if_json_reader
            json    TYPE json
            length  TYPE i
            ...
  CHANGING  data TYPE data OPTIONAL  offset TYPE i DEFAULT 0.
```

**New (target):**
```abap
METHODS restore
  IMPORTING reader     TYPE REF TO if_json_reader
            type_descr TYPE REF TO cl_abap_typedescr OPTIONAL
            field_cache TYPE t_t_field_cache OPTIONAL
  CHANGING  data TYPE data OPTIONAL
  RAISING   cx_sy_move_cast_error.

METHODS restore_type
  IMPORTING reader      TYPE REF TO if_json_reader
            type_descr  TYPE REF TO cl_abap_typedescr OPTIONAL
            field_cache TYPE t_t_field_cache OPTIONAL
            convexit    TYPE string OPTIONAL
            typekind    TYPE abap_typekind OPTIONAL
  CHANGING  data TYPE data OPTIONAL
  RAISING   cx_sy_move_cast_error.
```

All internal recursive calls updated accordingly.

#### 2b. Rewrite `DESERIALIZE_INT`

```abap
METHOD deserialize_int.
  DATA reader TYPE REF TO if_json_reader.
  CHECK json IS NOT INITIAL OR jsonx IS NOT INITIAL.
  IF jsonx IS NOT INITIAL.
    reader = cl_json_xstring_reader=>create( jsonx ).
  ELSE.
    reader = cl_json_string_reader=>create( json ).
  ENDIF.
  TRY.
      reader->next_node( ).
      restore_type( EXPORTING reader = reader CHANGING data = data ).
    CATCH cx_json_reader_error.
      IF mv_strict_mode EQ abap_true.
        RAISE EXCEPTION TYPE cx_sy_move_cast_error.
      ENDIF.
    CATCH cx_sy_move_cast_error INTO DATA(lx).
      RAISE EXCEPTION TYPE cx_sy_move_cast_error
        EXPORTING previous        = lx
                  source_typename = `$.` && lx->source_typename
                  target_typename = lx->target_typename.
  ENDTRY.
ENDMETHOD.
```

Note: `JSONX_CP` parameter removed; xstring reader handles encoding natively.

#### 2c. Rewrite `RESTORE` body

```abap
" ... reference unwrapping logic (unchanged) ...

" Object parsing — replaces eat_white / eat_char('{') / WHILE loop:
ASSERT reader->node-type EQ if_json_node=>open_object.
reader->next_node( ).

WHILE reader->node-type NE if_json_node=>close_object
  AND reader->node-type NE if_json_node=>final
  AND reader->node-type NE if_json_node=>error.

  name_json = reader->node-name.
  " ... READ TABLE fields ... (unchanged logic)
  TRY.
      IF sy-subrc IS INITIAL.
        ASSIGN <field_cache>-value->* TO <value>.
        restore_type( EXPORTING reader      = reader
                                type_descr  = <field_cache>-type
                                typekind    = <field_cache>-typekind
                                convexit    = <field_cache>-convexit_in
                      CHANGING  data        = <value> ).
      ELSE.
        reader->skip_node( ).  " unknown field — skip entire subtree
      ENDIF.
    CATCH cx_sy_move_cast_error INTO lo_move_cast_error.
      " ... error path unchanged ...
  ENDTRY.

  reader->next_node( ).
ENDWHILE.
" No eat_char('}') needed — reader consumed close_object already
```

Key change: `reader->skip_node()` (no writer) replaces the old `restore_type( EXPORTING json=... )`
call used to skip unknown fields.

#### 2d. Rewrite `RESTORE_TYPE` CASE block

Replace `CASE json+offset(1)` with `CASE reader->node-type`:

| Old character check | New node type |
|---|---|
| `WHEN '{'` | `WHEN if_json_node=>open_object` |
| `WHEN '['` | `WHEN if_json_node=>open_array` |
| `WHEN '"'` | `WHEN if_json_node=>string` |
| `WHEN '-' OR '0'..'9'` | `WHEN if_json_node=>number` |
| `WHEN OTHERS` (bool/null) | `WHEN if_json_node=>boolean` / `WHEN if_json_node=>null` |

For the string branch:
- Replace `eat_name sdummy` with `sdummy = reader->node-value` (already unescaped — no UNESCAPE call)
- All downstream logic (ISO8601, EDM, GUID, convexit, type conversion) operates on `sdummy` unchanged

For the array branch — loop pattern:
```abap
reader->next_node( ).  " consume open_array
WHILE reader->node-type NE if_json_node=>close_array
  AND reader->node-type NE if_json_node=>final.
  array_index = array_index + 1.
  CLEAR <line>.
  restore_type( EXPORTING reader = reader ... CHANGING data = <line> ).
  INSERT <line> INTO TABLE <table>.
  reader->next_node( ).
ENDWHILE.
" close_array already consumed by next_node at top of next iteration
```

For the assoc-array branch:
```abap
reader->next_node( ).  " consume open_object
WHILE reader->node-type NE if_json_node=>close_object.
  key_value = reader->node-name.
  " ... key assignment logic unchanged ...
  restore_type( EXPORTING reader = reader ... ).
  INSERT <line> INTO TABLE <table>.
  reader->next_node( ).
ENDWHILE.
```

For JSON typekind passthrough (raw JSON field):
```abap
DATA writer TYPE REF TO if_json_writer.
writer = cl_json_string_writer=>create( ).
reader->skip_node( writer ).
data = CAST cl_json_string_writer( writer )->get_json( ).
```

#### 2e. Introduce `GENERATE_INT_R` (reader-based internal workhorse)

`GENERATE_INT` (public instance method) keeps its current signature for API compatibility.
Internally it creates a reader and delegates to a new private `GENERATE_INT_R`:

```abap
METHODS generate_int_r   " PRIVATE
  IMPORTING reader TYPE REF TO if_json_reader
  CHANGING  data   TYPE REF TO data
            type   TYPE REF TO cl_abap_datadescr OPTIONAL
  RAISING   cx_sy_move_cast_error.
```

`GENERATE_INT` implementation:
```abap
METHOD generate_int.
  DATA reader TYPE REF TO if_json_reader.
  IF length IS NOT SUPPLIED.
    reader = cl_json_string_reader=>create( json ).
  ELSE.
    reader = cl_json_string_reader=>create( json(length) ).
  ENDIF.
  reader->next_node( ).
  generate_int_r( EXPORTING reader = reader CHANGING data = data type = type ).
ENDMETHOD.
```

`GENERATE_INT_EX` sets the assoc-array flags, creates a reader, calls `generate_int_r`, restores flags.

`GENERATE_INT_R` branches on `reader->node-type` (replacing `CASE json+offset(1)`):
- `if_json_node=>open_object` → collect fields via member loop, call `generate_struct`
- `if_json_node=>open_array` → collect items via array loop, build table
- `if_json_node=>string` → type-detect from `reader->node-value`, restore into typed ref
- `if_json_node=>number` → detect int/float/packed from value string
- `if_json_node=>boolean` → `so_type_b`
- `if_json_node=>null` → CLEAR data

#### 2f. Macros deleted in Phase 2

`eat_white`, `eat_char`, `eat_name`, `eat_number`, `eat_bool`,
`while_offset_cs`, `while_offset_not_cs`,
`restore_reference`, `restore_reference_ex`,
`throw_error`

#### 2g. `lcl_util` methods deleted in Phase 2

- `read_string` — existed only to handle escaped quotes in manual string scanning; reader delivers
  already-unescaped values
- `_escape` — only used by `escape_json` macro (which survives temporarily until Phase 3 removes it)

---

### Phase 3 — Serialization: rewrite with `IF_JSON_WRITER`

**Goal:** `DUMP_INT` / `DUMP_SYMBOLS` write via a writer. `SERIALIZE_INT` owns the writer lifecycle.
`FORMAT_OUTPUT` becomes writer options. All string concatenation for JSON output is eliminated.

#### 3a. Change `DUMP_INT` and `DUMP_SYMBOLS` signatures

**Old:** `RETURNING value(R_JSON) TYPE json` + `IMPORTING level TYPE i`
**New:** `IMPORTING writer TYPE REF TO if_json_writer` (no return value, no level)

`level` is removed — the writer tracks nesting depth for pretty-print internally.

#### 3b. `SERIALIZE_INT` becomes writer owner

```abap
METHOD serialize_int.
  DATA writer TYPE REF TO if_json_writer.
  writer = cl_json_string_writer=>create( ).
  IF mv_format_output EQ abap_true.
    writer->set_option( if_json_writer=>option_linebreaks ).
    writer->set_option( if_json_writer=>option_indent ).
  ENDIF.
  IF name IS NOT INITIAL.
    writer->open_member( name ).
  ENDIF.
  dump_int( data = data type_descr = type_descr writer = writer ).
  IF name IS NOT INITIAL.
    writer->close_member( ).
  ENDIF.
  r_json = CAST cl_json_string_writer( writer )->get_json( ).
ENDMETHOD.
```

#### 3c. `DUMP_INT` type dispatch

| Kind | Writer pattern |
|---|---|
| `kind_elem` | `dump_type` macro → `dump_type_int` → direct writer calls |
| `kind_struct` | `writer->open_object()` → `dump_symbols( writer )` → `writer->close_object()` |
| `kind_table` (array) | `writer->open_array()` → per-row `dump_int( writer )` → `writer->close_array()` |
| `kind_table` (assoc) | `writer->open_object()` → per-row `writer->open_member(key)` + content + `writer->close_member()` → `writer->close_object()` |
| `kind_ref` (dref, initial) | `writer->write_null()` |
| `kind_ref` (dref, cycle) | `writer->open_object()` → `writer->close_object()` |
| `kind_ref` (oref, cycle) | `writer->open_object()` → `writer->close_object()` |

#### 3d. `DUMP_SYMBOLS` writer pattern

```abap
METHOD dump_symbols.
  LOOP AT it_symbols ASSIGNING <symbol>.
    ASSIGN <symbol>-value->* TO <value>.
    CHECK <symbol>-compressable EQ abap_false OR <value> IS NOT INITIAL OR opt_array EQ abap_true.
    IF opt_array EQ abap_false.
      writer->open_member( <symbol>-header ).   " header already contains the JSON name
    ENDIF.
    IF <symbol>-elem_type IS NOT INITIAL.
      dump_type <value> <symbol>-elem_type <symbol>-typekind writer <symbol>-convexit_out.
    ELSE.
      dump_int( data = <value> type_descr = <symbol>-type writer = writer ).
    ENDIF.
    IF opt_array EQ abap_false.
      writer->close_member( ).
    ENDIF.
  ENDLOOP.
ENDMETHOD.
```

Note: `<symbol>-header` currently stores `'"fieldname":'` — with writer it should store only the
plain field name. Adjust `GET_SYMBOLS_STRUCT` and `GET_SYMBOLS_CLASS` to store the plain name
(without quotes and colon). The `format_output` space suffix is also removed (writer handles spacing).

#### 3e. Rewrite `dump_type_int` macro

The macro gains `writer` as parameter `&5` (replacing the result string `&3`).
The convexit function parameter `&4` is retained.

```abap
DEFINE dump_type_int.  " &1=data  &2=typekind  &3=<obsolete/remove>  &4=convexit  &5=writer
  CASE &2.
    WHEN e_typekind-convexit.
      IF &1 IS INITIAL.
        &5->write_string( value = `` ).
      ELSE.
        DATA _cv_out TYPE string.
        CALL FUNCTION &4 EXPORTING input = &1 IMPORTING output = _cv_out EXCEPTIONS OTHERS = 1.
        IF sy-subrc IS INITIAL.
          &5->write_string( value = _cv_out ).
        ELSE.
          &5->write_string( value = `` ).
        ENDIF.
      ENDIF.
    WHEN e_typekind-utclong.
      IF &1 IS INITIAL.
        &5->write_string( value = mv_initial_ts ).  " strip outer quotes from mv_initial_ts
      ELSE.
        DATA _utcl TYPE c LENGTH 27.
        _utcl = &1.
        &5->write_string( value = |{ _utcl(10) }T{ _utcl+11(16) }Z| ).
      ENDIF.
    WHEN e_typekind-ts_iso8601.
      IF mv_ts_as_iso8601 EQ c_bool-true.
        IF &1 IS INITIAL.
          &5->write_string( value = mv_initial_ts ).
        ELSE.
          DATA _ts TYPE c LENGTH 14.
          _ts = &1.
          &5->write_string( value = |{ _ts(4) }-{ _ts+4(2) }-{ _ts+6(2) }T{ _ts+8(2) }:{ _ts+10(2) }:{ _ts+12(2) }Z| ).
        ENDIF.
      ELSE.
        DATA _tsn TYPE string.
        _tsn = &1.  CONDENSE _tsn.
        &5->write_number( value = _tsn ).
      ENDIF.
    WHEN e_typekind-tsl_iso8601.
      " analogous to ts_iso8601 with 7-digit subseconds
    WHEN e_typekind-float.
      IF &1 IS INITIAL.
        &5->write_number( value = `0` ).
      ELSE.
        DATA _fv TYPE string.
        _fv = &1.
        &5->write_number( value = _fv ).
      ENDIF.
    WHEN e_typekind-int OR e_typekind-int1 OR e_typekind-int2
      OR e_typekind-packed OR e_typekind-int8.
      IF &1 IS INITIAL.
        &5->write_number( value = `0` ).
      ELSE.
        DATA _iv TYPE string.
        _iv = &1.
        IF &1 LT 0. SHIFT _iv RIGHT CIRCULAR. ELSE. CONDENSE _iv. ENDIF.
        &5->write_number( value = _iv ).
      ENDIF.
    WHEN e_typekind-numc_string.
      IF &1 IS INITIAL.
        &5->write_string( value = `` ).
      ELSE.
        &5->write_string( value = &1 ).
      ENDIF.
    WHEN e_typekind-num.
      IF &1 IS INITIAL.
        &5->write_number( value = `0` ).
      ELSE.
        DATA _nv TYPE string.
        _nv = &1.  SHIFT _nv LEFT DELETING LEADING '0'.
        IF _nv IS INITIAL. _nv = `0`. ENDIF.
        &5->write_number( value = _nv ).
      ENDIF.
    WHEN e_typekind-json.
      " Raw JSON passthrough via reader->skip_node( writer )
      DATA _jr TYPE REF TO if_json_reader.
      _jr = cl_json_string_reader=>create( &1 ).
      _jr->next_node( ).
      _jr->skip_node( &5 ).
    WHEN e_typekind-string OR e_typekind-csequence OR e_typekind-clike OR e_typekind-char.
      IF &1 IS INITIAL.
        &5->write_string( value = `` ).
      ELSE.
        &5->write_string( value = &1 ).  " writer escapes internally — no escape_json needed
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
      IF &1 IS INITIAL.
        &5->write_string( value = `` ).
      ELSE.
        DATA _xv TYPE string.
        xstring_to_string_int &1 _xv.
        &5->write_string( value = _xv ).
      ENDIF.
    WHEN e_typekind-bool.
      IF &1 EQ c_bool-true.
        &5->write_boolean( value = `true` ).
      ELSE.
        &5->write_boolean( value = `false` ).
      ENDIF.
    WHEN e_typekind-tribool.
      IF &1 IS INITIAL.
        &5->write_null( ).
      ELSEIF &1 EQ c_bool-true.
        &5->write_boolean( value = `true` ).
      ELSE.
        &5->write_boolean( value = `false` ).
      ENDIF.
    WHEN e_typekind-date.
      IF &1 IS INITIAL.
        &5->write_string( value = mv_initial_date ).
      ELSE.
        &5->write_string( value = |{ &1(4) }-{ &1+4(2) }-{ &1+6(2) }| ).
      ENDIF.
    WHEN e_typekind-time.
      IF &1 IS INITIAL.
        &5->write_string( value = mv_initial_time ).
      ELSE.
        &5->write_string( value = |{ &1(2) }:{ &1+2(2) }:{ &1+4(2) }| ).
      ENDIF.
    WHEN e_typekind-enum.
      DATA _ev TYPE string.
      _ev = &1.
      &5->write_string( value = _ev ).
    WHEN OTHERS.
      IF &1 IS INITIAL.
        &5->write_null( ).
      ELSE.
        DATA _ov TYPE string.
        _ov = &1.
        &5->write_string( value = _ov ).
      ENDIF.
  ENDCASE.
END-OF-DEFINITION.
```

Note on `mv_initial_ts` / `mv_initial_date` / `mv_initial_time`: these are stored with surrounding
quotes (e.g. `""`). When passing to `write_string`, strip the surrounding quotes — pass only the
inner value. Alternatively store them without quotes and adjust the writer call.

#### 3f. `DUMP_TYPE` protected method — change signature

```abap
METHODS dump_type
  IMPORTING data       TYPE data
            type_descr TYPE REF TO cl_abap_elemdescr
            convexit   TYPE string
            typekind   TYPE abap_typekind OPTIONAL
            writer     TYPE REF TO if_json_writer.   " NEW — replaces RETURNING r_json
```

The `dump_type` dispatch macro:
```abap
DEFINE dump_type.  " &1=data  &2=type_descr  &3=typekind  &4=writer  &5=convexit
  IF mv_extended IS INITIAL.
    dump_type_int &1 &3 ##UNUSED &5 &4.
  ELSE.
    dump_type( data = &1 type_descr = &2 typekind = &3 convexit = &5 writer = &4 ).
  ENDIF.
END-OF-DEFINITION.
```

#### 3g. `DUMP_TYPE_EX` removal

`DUMP_TYPE_EX` was a protected convenience method; subclasses do not need it once `DUMP_TYPE`
takes a writer. Remove it.

#### 3h. `FORMAT_OUTPUT` and indentation

- `MV_FORMAT_OUTPUT` instance variable: kept (constructor parameter unchanged)
- `GET_INDENT` static method: **deleted** — no longer needed
- `MC_DEFAULT_INDENT` constant: **deleted**
- `SV_WHITE_SPACE` class-data: **deleted** (was only used by `eat_white` macro)
- All `lv_indent`, `indent`, `lv_lb` local variables in `DUMP_INT`/`DUMP_SYMBOLS`: **deleted**

#### 3i. `SYMBOL-HEADER` field adjustment

Currently `header` stores `'"FIELDNAME":'` (with quotes, colon, optional trailing space).
With writer, `open_member` takes a plain name. Change `GET_SYMBOLS_STRUCT` and `GET_SYMBOLS_CLASS`:
- Store plain pretty-printed name in `symbol-header` (no quotes, no colon)
- Pass `<symbol>-header` to `writer->open_member( <symbol>-header )`

#### 3j. Macros deleted in Phase 3

`format_list_output`, `escape_json`

#### 3k. `lcl_util` methods deleted in Phase 3

`_escape` (was only used by `escape_json` macro)

---

### Phase 4 — Class surface reduction and dynamic call cleanup

**Goal:** minimize the protected/public surface; move static infrastructure to `lcl_util`;
replace dynamic calls with direct calls where BASIS version allows.

#### 4a. Move from class `CLASS-DATA` / static methods → `lcl_util`

| Item | Action |
|---|---|
| `SO_REGEX_DATE/TIME/GUID/EDM_DATE_TIME/EDM_TIME` | Move to `lcl_util` class-data |
| `SO_REGEX_GENERATE_NORMALIZE/CAMEL_CASE/TYPE_DETECT` | Move to `lcl_util` class-data |
| `SO_TYPE_S/F/P/I/B/D/T/TS/TSL/T_JSON/T_NAME_VALUE` | Move to `lcl_util` class-data |
| `SO_TYPE_REFTAB` | Move to `lcl_util` |
| `MC_NAME_SYMBOLS_MAP` | Move to `lcl_util` |
| `detect_typekind` (protected final) | Move to `lcl_util=>detect_typekind()` |
| Regex initialization | Move from `class_constructor` to `lcl_util=>class_constructor` using `cl_abap_regex=>create_pcre( pattern = ... )` directly (no `create_regexp` macro) |

`CLASS_CONSTRUCTOR` of the main class shrinks to: `mc_bool_types`, `mc_bool_3state`,
`mc_json_type`, `mc_cov_error`, `mc_me_type` setup only.

#### 4b. Replace dynamic calls

| Dynamic call | Replacement |
|---|---|
| `CALL METHOD cl_abap_tstmp=>('TSTMP2UTCLONG')` | `cl_abap_tstmp=>tstmp2utclong(...)` |
| `CALL METHOD ('CL_ABAP_XSD')=>('TO_VALUE')` | `cl_abap_xsd=>to_value(...)` |
| `cl_abap_regex=>('CREATE_PCRE')` in `create_regexp` macro | `cl_abap_regex=>create_pcre(...)` directly |
| `CREATE OBJECT lo_json TYPE (mc_me_type)` | **Keep** — intentional runtime polymorphism |
| `CALL METHOD lo_json->(lc_method)` | **Keep** — intentional runtime polymorphism |

#### 4c. Move methods to PRIVATE

Methods currently in PROTECTED that no subclass needs to override or call directly:

| Method | Move to |
|---|---|
| `dump_int`, `dump_symbols`, `dump_type_ex` | PRIVATE |
| `generate_int`, `generate_int_ex`, `generate_struct`, `generate_int_r` (new) | PRIVATE |
| `get_symbols`, `get_symbols_struct`, `get_symbols_class`, `get_fields` | PRIVATE |
| `detect_typekind` | `lcl_util` (remove from class) |

#### 4d. Remove obsolete public/protected items

| Item | Reason |
|---|---|
| `EDM_DATETIME_TO_TS` (public static method) | Moved to `lcl_util=>read_edm_datetime` in z_ui2_json2 |
| `UNESCAPE` (public static method) | Reader delivers unescaped values; method deleted |
| `GET_INDENT` (public static method) | Writer handles formatting; method deleted |
| `GET_CONVEXIT_FUNC` (public static method) | Moved to `lcl_util=>get_convexit_func` |
| `ESCAPE` (public static method) | No longer needed internally; **keep** as documented public utility |
| `BOOL_TO_TRIBOOL` / `TRIBOOL_TO_BOOL` | Removed in z_ui2_json2; accept API change |
| `DUMP` (public static method) | Removed in z_ui2_json2; use `SERIALIZE` instead |
| `DUMP_TYPE_EX` (protected method) | Removed (see Phase 3g) |
| `SV_WHITE_SPACE` (public class-data) | Delete |
| `MC_DEFAULT_INDENT` (constant) | Delete |
| `MC_NAME_SYMBOLS_MAP` | Move to `lcl_util` |
| `mc_cov_error` (private class-data) | Delete (was a sentinel for conversion errors, no longer needed) |
| `ref_tab` type | Delete if unused after Phase 2/3 |

#### 4e. `create_regexp` macro

Delete after replacing all usages with `cl_abap_regex=>create_pcre( pattern = ... )`.

---

### Phase 5 — Testing and documentation

#### 5a. Unit tests

All 62+ existing tests must pass. Tests to update (expected output may change):

| Test method | Reason for update |
|---|---|
| `serialize_formatted` | `FORMAT_OUTPUT=true` output from writer may differ in whitespace from manual indentation. Accept new output. Update expected strings. |
| `serialize_associative_array` | Same for the format_output sub-case |

New test cases to add:

| Test | What it covers |
|---|---|
| `serialize_json_typekind_passthrough` | Field of TYPE `Z_UI2_JSON=>JSON` roundtrips via `skip_node(writer)` |
| `deserialize_cx_json_reader_error` | Malformed JSON raises no exception in non-strict mode; in strict mode raises `CX_SY_MOVE_CAST_ERROR` |
| `serialize_utclong_direct` | UTCLONG serialization without dynamic call |
| `serialize_enum_direct` | ENUM serialization without dynamic call |
| `subclass_dump_type_with_writer` | Subclass overriding `dump_type` receives a working writer |

#### 5b. Version and documentation

- Bump `VERSION` constant to **24**
- Update `docs/history.md` — new PL entry documenting:
  - Engine change to IF_JSON_READER / IF_JSON_WRITER
  - Minimum SAP_BASIS raised to 7.57
  - Removed public methods: `DUMP`, `BOOL_TO_TRIBOOL`, `TRIBOOL_TO_BOOL`, `UNESCAPE`,
    `GET_INDENT`, `GET_CONVEXIT_FUNC`, `EDM_DATETIME_TO_TS`
  - Changed protected method signatures: `DUMP_TYPE` (adds `writer`), `RESTORE` (reader only)
  - `FORMAT_OUTPUT` output format may differ slightly from previous versions

---

## Sequencing

```
Phase 1 (patch sync) → run tests
    ↓
Phase 2 (deserialization)    Phase 3 (serialization)
  — can proceed in parallel after Phase 1 completes —
    ↓                              ↓
Phase 4 (surface reduction) — after both 2 and 3 complete
    ↓
Phase 5 (tests + docs)
```

---

## Macro inventory after migration

| Macro | Status |
|---|---|
| `dump_type_int` | Rewritten (writer calls) |
| `dump_type` | Updated (writer parameter) |
| `is_compressable` | Unchanged |
| `xstring_to_string_int` | Unchanged |
| `string_to_xstring_int` | Unchanged |
| `format_name` | Unchanged |
| `eat_white` | **Deleted** (Phase 2) |
| `eat_char` | **Deleted** (Phase 2) |
| `eat_name` | **Deleted** (Phase 2) |
| `eat_number` | **Deleted** (Phase 2) |
| `eat_bool` | **Deleted** (Phase 2) |
| `while_offset_cs` | **Deleted** (Phase 2) |
| `while_offset_not_cs` | **Deleted** (Phase 2) |
| `restore_reference` | **Deleted** (Phase 2) |
| `restore_reference_ex` | **Deleted** (Phase 2) |
| `throw_error` | **Deleted** (Phase 2) |
| `escape_json` | **Deleted** (Phase 3) |
| `format_list_output` | **Deleted** (Phase 3) |
| `create_regexp` | **Deleted** (Phase 4) |

---

## Open questions (deferred to optimization phase)

1. `skip_node( writer OPTIONAL )` with no writer — confirm it purely advances the cursor
   (documented as optional but kernel behaviour should be verified empirically).
2. `write_number` value format — does it accept raw ABAP string assignments with leading spaces,
   or must the string be condensed first? Verify with `serialize_numbers` test.
3. Writer `open_member` / `close_member` — confirm `close_member` is a true no-op and
   `open_member` alone is sufficient (observed in kernel implementation but not documented).
4. Performance comparison old vs new — run SAT on a large-document benchmark after Phase 5.
