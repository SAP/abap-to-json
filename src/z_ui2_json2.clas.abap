*----------------------------------------------------------------------*
*       CLASS Z_UI2_JSON2 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS z_ui2_json2 DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .
    CLASS cl_abap_tstmp DEFINITION LOAD .
    CLASS cx_sy_conversion_error DEFINITION LOAD .

    TYPES json TYPE string .
    TYPES:
      BEGIN OF name_mapping,
        abap TYPE abap_compname,
        json TYPE string,
      END OF name_mapping .
    TYPES:
      name_mappings    TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY abap .
    TYPES bool TYPE char1 .
    TYPES tribool TYPE char1 .
    TYPES pretty_name_mode TYPE char1 .

    CONSTANTS:
      BEGIN OF pretty_mode,
        none          TYPE char1  VALUE ``,
        low_case      TYPE char1  VALUE 'L',
        camel_case    TYPE char1  VALUE 'X',
        pascal_case   TYPE char1  VALUE 'P',
        extended      TYPE char1  VALUE 'Y',
        user          TYPE char1  VALUE 'U',
        user_low_case TYPE char1  VALUE 'C',
      END OF  pretty_mode .
    CONSTANTS:
      BEGIN OF c_bool,
        true  TYPE bool  VALUE 'X',
        false TYPE bool  VALUE '',
      END OF  c_bool .
    CONSTANTS:
      BEGIN OF c_tribool,
        true      TYPE tribool  VALUE c_bool-true,
        false     TYPE tribool  VALUE '-',
        undefined TYPE tribool  VALUE ``,
      END OF  c_tribool .
    CONSTANTS mc_key_separator TYPE string VALUE `-` ##NO_TEXT.
    CLASS-DATA mc_bool_types TYPE string READ-ONLY VALUE `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL\TYPE=BOOLEAN\TYPE=BOOLE_D\TYPE=XFELD\TYPE=XSDBOOLEAN\TYPE=WDY_BOOLEAN` ##NO_TEXT.
    CLASS-DATA mc_bool_3state TYPE string READ-ONLY VALUE `\TYPE=BOOLEAN` ##NO_TEXT.
    CONSTANTS version TYPE i VALUE 1 ##NO_TEXT.
    CLASS-DATA mc_json_type TYPE string READ-ONLY .
    CONSTANTS:
      BEGIN OF e_typekind,
        " new extended pseudo typekind, hack and can clash with standard if new enums come...
        " always check for duplicates !!!
        convexit    TYPE abap_typekind VALUE '1' ##NO_TEXT,
        ts_iso8601  TYPE abap_typekind VALUE '2' ##NO_TEXT,
        tsl_iso8601 TYPE abap_typekind VALUE '3' ##NO_TEXT,
        numc_string TYPE abap_typekind VALUE '4' ##NO_TEXT,
        json        TYPE abap_typekind VALUE '5' ##NO_TEXT,
        bool        TYPE abap_typekind VALUE '6' ##NO_TEXT,
        tribool     TYPE abap_typekind VALUE '7' ##NO_TEXT,

        " redefine for existing typekinds for lower releases
        utclong     TYPE abap_typekind VALUE 'p' ##NO_TEXT, " CL_ABAP_TYPEDESCR=>TYPEKIND_UTCLONG -> 'p' only from 7.54
        int8        TYPE abap_typekind VALUE '8' ##NO_TEXT, " CL_ABAP_TYPEDESCR=>TYPEKIND_INT8 -> '8' only from 7.40
        enum        TYPE abap_typekind VALUE 'k' ##NO_TEXT, " CL_ABAP_TYPEDESCR=>TYPEKIND_ENUM -> 'k'

        " just aliasing
        float       TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_float,
        int         TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_int,
        int1        TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_int1,
        int2        TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_int2,
        packed      TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_packed,
        num         TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_num,
        string      TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_string,
        csequence   TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_csequence,
        clike       TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_clike,
        char        TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_char,
        date        TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_date,
        time        TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_time,
        xstring     TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_xstring,
        hex         TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_hex,
      END OF e_typekind .

    CLASS-METHODS class_constructor .
    CLASS-METHODS deserialize
      IMPORTING
        !json             TYPE json OPTIONAL
        !jsonx            TYPE xstring OPTIONAL
        !pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-none
        !assoc_arrays     TYPE bool DEFAULT c_bool-false
        !assoc_arrays_opt TYPE bool DEFAULT c_bool-false
        !name_mappings    TYPE name_mappings OPTIONAL
        !conversion_exits TYPE bool DEFAULT c_bool-false
        !hex_as_base64    TYPE bool DEFAULT c_bool-true
      CHANGING
        !data             TYPE data .
    CLASS-METHODS serialize
      IMPORTING
        !data             TYPE data
        !compress         TYPE bool DEFAULT c_bool-false
        !name             TYPE string OPTIONAL
        !pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-none
        !type_descr       TYPE REF TO cl_abap_typedescr OPTIONAL
        !assoc_arrays     TYPE bool DEFAULT c_bool-false
        !ts_as_iso8601    TYPE bool DEFAULT c_bool-false
        !expand_includes  TYPE bool DEFAULT c_bool-true
        !assoc_arrays_opt TYPE bool DEFAULT c_bool-false
        !numc_as_string   TYPE bool DEFAULT c_bool-false
        !name_mappings    TYPE name_mappings OPTIONAL
        !conversion_exits TYPE bool DEFAULT c_bool-false
        !format_output    TYPE bool DEFAULT c_bool-false
        !hex_as_base64    TYPE bool DEFAULT c_bool-true
      RETURNING
        VALUE(r_json)     TYPE json .
    METHODS deserialize_int
      IMPORTING
        !json  TYPE json OPTIONAL
        !jsonx TYPE xstring OPTIONAL
      CHANGING
        !data  TYPE data
      RAISING
        cx_sy_move_cast_error .
    CLASS-METHODS generate
      IMPORTING
        !json          TYPE json OPTIONAL
        !pretty_name   TYPE pretty_name_mode DEFAULT pretty_mode-none
        !name_mappings TYPE name_mappings OPTIONAL
        !jsonx         TYPE xstring OPTIONAL
          PREFERRED PARAMETER json
      RETURNING
        VALUE(rr_data) TYPE REF TO data .
    METHODS serialize_int
      IMPORTING
        !data         TYPE data
        !name         TYPE string OPTIONAL
        !type_descr   TYPE REF TO cl_abap_typedescr OPTIONAL
      RETURNING
        VALUE(r_json) TYPE json .
    METHODS constructor
      IMPORTING
        !compress         TYPE bool DEFAULT c_bool-false
        !pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-none
        !assoc_arrays     TYPE bool DEFAULT c_bool-false
        !ts_as_iso8601    TYPE bool DEFAULT c_bool-false
        !expand_includes  TYPE bool DEFAULT c_bool-true
        !assoc_arrays_opt TYPE bool DEFAULT c_bool-false
        !strict_mode      TYPE bool DEFAULT c_bool-false
        !numc_as_string   TYPE bool DEFAULT c_bool-false
        !name_mappings    TYPE name_mappings OPTIONAL
        !conversion_exits TYPE bool DEFAULT c_bool-false
        !format_output    TYPE bool DEFAULT c_bool-false
        !hex_as_base64    TYPE bool DEFAULT c_bool-true
        !bool_types       TYPE string DEFAULT mc_bool_types
        !bool_3state      TYPE string DEFAULT mc_bool_3state
        !initial_ts       TYPE string DEFAULT ``
        !initial_date     TYPE string DEFAULT ``
        !initial_time     TYPE string DEFAULT ``
        !time_zone        LIKE sy-zonlo DEFAULT 'UTC' .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF t_s_field_cache,
        name         TYPE string,
        type         TYPE REF TO cl_abap_datadescr,
        elem_type    TYPE REF TO cl_abap_elemdescr,
        typekind     TYPE abap_typekind,
        convexit_out TYPE string,
        convexit_in  TYPE string,
        value        TYPE REF TO data,
      END OF t_s_field_cache .
    TYPES:
      BEGIN OF t_s_symbol,
        header       TYPE string,
        compressable TYPE abap_bool,
        read_only    TYPE abap_bool.
        INCLUDE TYPE t_s_field_cache.
    TYPES: END OF t_s_symbol .
    TYPES:
      t_t_symbol TYPE STANDARD TABLE OF t_s_symbol WITH DEFAULT KEY .
    TYPES:
      t_t_field_cache  TYPE HASHED TABLE OF t_s_field_cache WITH UNIQUE KEY name .
    TYPES:
      name_mappings_ex TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY json .
    TYPES:
      BEGIN OF t_s_name_json,
        name  TYPE string,
        value TYPE json,
      END OF t_s_name_json .
    TYPES:
      t_t_name_json TYPE SORTED TABLE OF t_s_name_json WITH UNIQUE KEY name .
    TYPES:
      BEGIN OF t_s_name_value,
        name  TYPE string,
        value TYPE json,
        data  TYPE REF TO data,
        type  TYPE REF TO cl_abap_datadescr,
      END OF t_s_name_value .
    TYPES:
      t_t_name_value TYPE SORTED TABLE OF t_s_name_value WITH UNIQUE KEY name .
    TYPES:
      t_t_json TYPE STANDARD TABLE OF json WITH DEFAULT KEY .
    TYPES:
      BEGIN OF t_s_struct_type,
        keys TYPE string,
        type TYPE REF TO cl_abap_datadescr,
      END OF t_s_struct_type .
    TYPES:
      t_t_struct_type TYPE SORTED TABLE OF t_s_struct_type WITH UNIQUE KEY keys .
    TYPES:
      BEGIN OF t_s_struct_cache_res,
        data    TYPE REF TO data,
        symbols TYPE t_t_symbol,
      END OF t_s_struct_cache_res .
    TYPES:
      BEGIN OF t_s_struct_cache,
        type_descr      TYPE REF TO cl_abap_structdescr,
        include_aliases	TYPE abap_bool,
        result          TYPE t_s_struct_cache_res,
      END OF t_s_struct_cache .
    TYPES:
      t_t_struct_cache TYPE HASHED TABLE OF t_s_struct_cache WITH UNIQUE KEY type_descr include_aliases .

    DATA mv_bool_types TYPE string .
    DATA mv_bool_3state TYPE string .
    DATA mv_initial_ts TYPE string VALUE `` ##NO_TEXT.
    DATA mv_initial_date TYPE string VALUE `` ##NO_TEXT.
    DATA mv_initial_time TYPE string VALUE `` ##NO_TEXT.
    DATA mv_time_zone TYPE timezone VALUE `UTC` ##NO_TEXT.
    DATA mv_compress TYPE bool .
    DATA mv_pretty_name TYPE pretty_name_mode .
    DATA mv_assoc_arrays TYPE bool .
    DATA mv_ts_as_iso8601 TYPE bool .
    DATA mv_expand_includes TYPE bool .
    DATA mv_assoc_arrays_opt TYPE bool .
    DATA mv_strict_mode TYPE bool .
    DATA mv_numc_as_string TYPE bool .
    DATA mv_format_output TYPE bool .
    DATA mv_conversion_exits TYPE bool .
    DATA mv_hex_as_base64 TYPE bool .
    DATA mt_name_mappings TYPE name_mappings .
    DATA mt_name_mappings_ex TYPE name_mappings_ex .
    DATA mt_struct_type TYPE t_t_struct_type .
    DATA mt_struct_cache TYPE t_t_struct_cache .
    DATA:
      mt_ref_dump_idx TYPE SORTED TABLE OF REF TO data WITH UNIQUE DEFAULT KEY .
    DATA:
      mt_obj_dump_idx TYPE SORTED TABLE OF REF TO object WITH UNIQUE DEFAULT KEY .
    CONSTANTS mc_typekind_utclong TYPE abap_typekind VALUE 'p' ##NO_TEXT.   " CL_ABAP_TYPEDESCR=>TYPEKIND_UTCLONG -> 'p' only from 7.54
    CONSTANTS mc_typekind_int8 TYPE abap_typekind VALUE '8' ##NO_TEXT.   " TYPEKIND_INT8 -> '8' only from 7.40

    METHODS is_compressable
      IMPORTING
        !type_descr        TYPE REF TO cl_abap_typedescr   ##NEEDED
        !name              TYPE csequence   ##NEEDED
      RETURNING
        VALUE(rv_compress) TYPE abap_bool .
    METHODS restore
      IMPORTING
        !reader           TYPE REF TO if_json_reader
        VALUE(type_descr) TYPE REF TO cl_abap_typedescr OPTIONAL
        !field_cache      TYPE t_t_field_cache OPTIONAL
      CHANGING
        !data             TYPE data OPTIONAL
      RAISING
        cx_sy_move_cast_error .
    METHODS restore_type
      IMPORTING
        !reader           TYPE REF TO if_json_reader
        VALUE(type_descr) TYPE REF TO cl_abap_typedescr OPTIONAL
        !field_cache      TYPE t_t_field_cache OPTIONAL
        !convexit         TYPE string OPTIONAL
        !typekind         TYPE abap_typekind OPTIONAL
      CHANGING
        !data             TYPE data OPTIONAL
      RAISING
        cx_sy_move_cast_error .
    METHODS dump_type
      IMPORTING
        !data       TYPE data
        !type_descr TYPE REF TO cl_abap_elemdescr
        !convexit   TYPE string
        !typekind   TYPE abap_typekind
        !writer     TYPE REF TO if_json_writer
        !name       TYPE string OPTIONAL .
    METHODS pretty_name_ex
      IMPORTING
        !in        TYPE csequence
      RETURNING
        VALUE(out) TYPE string .
    METHODS pretty_name
      IMPORTING
        !in          TYPE csequence
        !pascal_case TYPE bool DEFAULT c_bool-false
      RETURNING
        VALUE(out)   TYPE string .
    METHODS generate_int
      IMPORTING
        !json TYPE json
      CHANGING
        !data TYPE REF TO data
        !type TYPE REF TO cl_abap_datadescr OPTIONAL
      RAISING
        cx_sy_move_cast_error .
  PRIVATE SECTION.

    DATA mv_extended TYPE bool .
    CLASS-DATA mc_me_type TYPE string .

    METHODS restore_type_int
      IMPORTING
        !reader           TYPE REF TO if_json_reader
        VALUE(type_descr) TYPE REF TO cl_abap_typedescr OPTIONAL
        !field_cache      TYPE t_t_field_cache OPTIONAL
        !convexit         TYPE string OPTIONAL
        !typekind         TYPE abap_typekind OPTIONAL
      CHANGING
        !data             TYPE data OPTIONAL
      RAISING
        cx_sy_move_cast_error
        cx_sy_conversion_no_number
        cx_sy_conversion_overflow .
    METHODS generate_int_r
      IMPORTING
        !reader TYPE REF TO if_json_reader
      CHANGING
        !data   TYPE REF TO data
        !type   TYPE REF TO cl_abap_datadescr OPTIONAL
      RAISING
        cx_sy_move_cast_error .
    METHODS dump_symbols
      IMPORTING
        !it_symbols   TYPE t_t_symbol
        !writer       TYPE REF TO if_json_writer
        !opt_array    TYPE bool OPTIONAL
        !format_scope TYPE bool DEFAULT abap_true
        !name         TYPE string OPTIONAL
        !level        TYPE i .
    METHODS get_symbols_struct
      IMPORTING
        !type_descr      TYPE REF TO cl_abap_structdescr
        !include_aliases TYPE abap_bool DEFAULT abap_false
        !data            TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(result)    TYPE t_s_struct_cache_res .
    METHODS get_symbols_class
      IMPORTING
        !type_descr   TYPE REF TO cl_abap_classdescr
        !object       TYPE REF TO object OPTIONAL
      RETURNING
        VALUE(result) TYPE t_t_symbol .
    METHODS get_symbols
      IMPORTING
        !type_descr      TYPE REF TO cl_abap_typedescr
        !data            TYPE REF TO data OPTIONAL
        !object          TYPE REF TO object OPTIONAL
        !include_aliases TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)    TYPE t_t_symbol .
    METHODS get_fields
      IMPORTING
        !type_descr      TYPE REF TO cl_abap_typedescr
        !data            TYPE REF TO data OPTIONAL
        !object          TYPE REF TO object OPTIONAL
      RETURNING
        VALUE(rt_fields) TYPE t_t_field_cache .
    METHODS dump_int
      IMPORTING
        !data       TYPE data
        !type_descr TYPE REF TO cl_abap_typedescr OPTIONAL
        !convexit   TYPE string OPTIONAL
        !writer     TYPE REF TO if_json_writer
        !name       TYPE string OPTIONAL
        !level      TYPE i DEFAULT 0 .
    METHODS generate_struct
      CHANGING
        !fields TYPE t_t_name_value
        !data   TYPE REF TO data
        !type   TYPE REF TO cl_abap_datadescr OPTIONAL .
*"* private components of class Z_UI2_JSON2
*"* do not include other source files here!!!
ENDCLASS.



CLASS Z_UI2_JSON2 IMPLEMENTATION.


  METHOD class_constructor.

    DATA(lv_json_string)         = VALUE json( ).
    DATA(lo_bool_type_descr)    = cl_abap_typedescr=>describe_by_data( c_bool-true ).
    DATA(lo_tribool_type_descr) = cl_abap_typedescr=>describe_by_data( c_tribool-true ).
    DATA(lo_json_type_descr)    = cl_abap_typedescr=>describe_by_data( lv_json_string ).

    mc_bool_types  = mc_bool_types  && lo_bool_type_descr->absolute_name && lo_tribool_type_descr->absolute_name.
    mc_bool_3state = mc_bool_3state && lo_tribool_type_descr->absolute_name.
    mc_json_type   = mc_json_type   && lo_json_type_descr->absolute_name.

    FIND FIRST OCCURRENCE OF '\TYPE=' IN lo_json_type_descr->absolute_name MATCH OFFSET DATA(lv_pos).
    IF sy-subrc IS INITIAL.
      mc_me_type = lo_json_type_descr->absolute_name(lv_pos).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    DATA(rtti) = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( me ) ).
    DATA pair LIKE LINE OF name_mappings.

    mv_compress         = compress.
    mv_pretty_name      = pretty_name.
    mv_assoc_arrays     = assoc_arrays.
    mv_ts_as_iso8601    = ts_as_iso8601.
    mv_expand_includes  = expand_includes.
    mv_assoc_arrays_opt = assoc_arrays_opt.
    mv_strict_mode      = strict_mode.
    mv_numc_as_string   = numc_as_string.
    mv_conversion_exits = conversion_exits.
    mv_format_output    = format_output.
    mv_hex_as_base64    = hex_as_base64.
    mv_bool_types       = bool_types.
    mv_bool_3state      = bool_3state.
    mv_initial_ts       = initial_ts.
    mv_initial_date     = initial_date.
    mv_initial_time     = initial_time.
    mv_time_zone        = time_zone.

    LOOP AT name_mappings INTO pair.
      TRANSLATE pair-abap TO UPPER CASE.
      INSERT pair INTO TABLE mt_name_mappings.
    ENDLOOP.

    IF mt_name_mappings IS NOT INITIAL.

      " if it dumps here, you have passed ambiguous mapping to the API
      " Please check your code for duplicates, pairs ABAP - JSON shall be unique
      " and there shall be no ambiguity for looking up only by JSON name (deserialize, generate)
      " and no ambiguity for looking up only by ABAP name (serialize)
      " per default, serializer/deserializer tries to have lossless conversion from JSON to ABAP and vice versa
      INSERT LINES OF mt_name_mappings INTO TABLE mt_name_mappings_ex.

      IF mv_pretty_name = pretty_mode-none.
        mv_pretty_name = pretty_mode-user.
      ELSEIF pretty_name = pretty_mode-low_case.
        mv_pretty_name = pretty_mode-user_low_case.
      ENDIF.

    ENDIF.

    IF rtti->absolute_name <> mc_me_type.
      mv_extended = c_bool-true.
    ENDIF.

  ENDMETHOD.


  METHOD deserialize.

    " **********************************************************************
    " Usage examples and documentation can be found on GitHub:
    " https://github.com/SAP/abap-to-json
    " **********************************************************************  "

    IF json IS NOT INITIAL OR jsonx IS NOT INITIAL.

      DATA(lo_json) = NEW z_ui2_json2(
        pretty_name      = pretty_name
        name_mappings    = name_mappings
        assoc_arrays     = assoc_arrays
        conversion_exits = conversion_exits
        hex_as_base64    = hex_as_base64
        assoc_arrays_opt = assoc_arrays_opt ).

      TRY.
          lo_json->deserialize_int(
            EXPORTING
              json  = json
              jsonx = jsonx
            CHANGING
              data  = data ).
        CATCH cx_sy_move_cast_error.                    "#EC NO_HANDLER
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD deserialize_int.

    " **********************************************************************
    " Usage examples and documentation can be found on GitHub:
    " https://github.com/SAP/abap-to-json
    " **********************************************************************  "

    CHECK json IS NOT INITIAL OR jsonx IS NOT INITIAL.

    DATA lo_reader TYPE REF TO if_json_reader.
    TRY.
        IF jsonx IS NOT INITIAL.
          lo_reader = cl_json_xstring_reader=>create( jsonx ).
        ELSE.
          lo_reader = cl_json_string_reader=>create( json ).
        ENDIF.

        lo_reader->next_node( ).
        TRY.
            DATA(lo_descr) = cl_abap_typedescr=>describe_by_data( data ).
            DATA(lv_init_typekind) = lcl_util=>detect_typekind( type_descr = lo_descr numc_as_string = mv_numc_as_string bool_types = mv_bool_types bool_3state = mv_bool_3state ).
            restore_type( EXPORTING reader = lo_reader type_descr = lo_descr typekind = lv_init_typekind CHANGING data = data ).
          CATCH cx_sy_move_cast_error INTO DATA(lx_move).
            RAISE EXCEPTION TYPE cx_sy_move_cast_error
              EXPORTING
                previous        = lx_move
                source_typename = `$.` && lx_move->source_typename
                target_typename = lx_move->target_typename.
        ENDTRY.
      CATCH cx_sy_move_cast_error INTO DATA(lx_strict).
        RAISE EXCEPTION lx_strict.
      CATCH cx_root INTO DATA(lx_parse) ##CATCH_ALL.
        IF mv_strict_mode = abap_true.
          RAISE EXCEPTION TYPE cx_sy_move_cast_error
            EXPORTING
              previous = lx_parse.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD dump_int.

    DATA: lo_typedesc   TYPE REF TO cl_abap_typedescr,
          lo_elem_descr TYPE REF TO cl_abap_elemdescr,
          lo_classdesc  TYPE REF TO cl_abap_classdescr,
          lo_structdesc TYPE REF TO cl_abap_structdescr,
          lo_tabledescr TYPE REF TO cl_abap_tabledescr,
          ls_struct_sym TYPE t_s_struct_cache_res,
          lt_symbols    TYPE t_t_symbol,
          lt_keys       TYPE STANDARD TABLE OF REF TO data WITH DEFAULT KEY,
          lo_obj_ref    TYPE REF TO object,
          lo_data_ref   TYPE REF TO data,
          ls_skip_key   TYPE LINE OF abap_keydescr_tab,
          lv_array_opt  TYPE abap_bool,
          lv_level      LIKE level,
          lv_prop_name  TYPE string,
          lv_keyval     TYPE string,
          lv_typekind   TYPE abap_typekind.

    FIELD-SYMBOLS: <line>   TYPE any,
                   <value>  TYPE any,
                   <data>   TYPE data,
                   <key>    TYPE LINE OF abap_keydescr_tab,
                   <symbol> TYPE t_s_symbol,
                   <table>  TYPE ANY TABLE.

    lv_level = level + 1.

    CASE type_descr->kind.
      WHEN cl_abap_typedescr=>kind_ref.

        IF data IS INITIAL.
          writer->write_null( name = name ).
        ELSEIF type_descr->type_kind = cl_abap_typedescr=>typekind_dref.
          lo_data_ref ?= data.
          INSERT lo_data_ref INTO TABLE mt_ref_dump_idx.
          IF sy-subrc IS INITIAL.
            lo_typedesc = cl_abap_typedescr=>describe_by_data_ref( lo_data_ref ).
            ASSIGN lo_data_ref->* TO <data>.
            dump_int( data = <data> type_descr = lo_typedesc writer = writer name = name level = level ).
            DELETE TABLE mt_ref_dump_idx WITH TABLE KEY table_line = lo_data_ref.
          ELSE.
            writer->write_null( name = name ).
          ENDIF.
        ELSE.
          lo_obj_ref ?= data.
          INSERT lo_obj_ref INTO TABLE mt_obj_dump_idx.
          IF sy-subrc IS INITIAL.
            lo_classdesc ?= cl_abap_typedescr=>describe_by_object_ref( lo_obj_ref ).
            lt_symbols = get_symbols_class( type_descr = lo_classdesc object = lo_obj_ref ).
            dump_symbols( it_symbols = lt_symbols writer = writer name = name level = level ).
            DELETE TABLE mt_obj_dump_idx WITH TABLE KEY table_line = lo_obj_ref.
          ELSE.
            writer->write_null( name = name ).
          ENDIF.
        ENDIF.

      WHEN cl_abap_typedescr=>kind_elem.
        lo_elem_descr ?= type_descr.
        lv_typekind = lcl_util=>detect_typekind( type_descr = lo_elem_descr convexit = convexit numc_as_string = mv_numc_as_string bool_types = mv_bool_types bool_3state = mv_bool_3state ).
        dump_type data lo_elem_descr lv_typekind writer convexit name.

      WHEN cl_abap_typedescr=>kind_struct.

        lo_structdesc ?= type_descr.
        GET REFERENCE OF data INTO lo_data_ref.
        ls_struct_sym = get_symbols_struct( type_descr = lo_structdesc data = lo_data_ref ).
        dump_symbols( it_symbols = ls_struct_sym-symbols writer = writer name = name level = level ).

      WHEN cl_abap_typedescr=>kind_table.

        lo_tabledescr ?= type_descr.
        lo_typedesc = lo_tabledescr->get_table_line_type( ).
        ASSIGN data TO <table>.

        IF lo_typedesc->kind = cl_abap_typedescr=>kind_struct.
          lo_structdesc ?= lo_typedesc.
          ls_struct_sym = get_symbols_struct( type_descr = lo_structdesc ).
          ASSIGN ls_struct_sym-data->* TO <line>.

          IF lo_tabledescr->has_unique_key IS NOT INITIAL AND mv_assoc_arrays IS NOT INITIAL.

            IF lo_tabledescr->key_defkind = lo_tabledescr->keydefkind_user.
              LOOP AT lo_tabledescr->key ASSIGNING <key>.
                READ TABLE ls_struct_sym-symbols WITH KEY name = <key>-name ASSIGNING <symbol>.
                APPEND <symbol>-value TO lt_keys.
              ENDLOOP.
            ENDIF.

            IF lines( lo_tabledescr->key ) = 1.
              READ TABLE lo_tabledescr->key INDEX 1 INTO ls_skip_key.
              DELETE ls_struct_sym-symbols WHERE name = ls_skip_key-name.
              IF mv_assoc_arrays_opt = abap_true AND lines( ls_struct_sym-symbols ) = 1.
                lv_array_opt = abap_true.
              ENDIF.
            ENDIF.

            writer->open_object( name = name ).
            LOOP AT <table> INTO <line>.
              CLEAR lv_prop_name.
              IF lo_tabledescr->key_defkind = lo_tabledescr->keydefkind_user.
                LOOP AT lt_keys INTO lo_data_ref.
                  ASSIGN lo_data_ref->* TO <value>.
                  lv_keyval = <value>.
                  CONDENSE lv_keyval.
                  IF lv_prop_name IS NOT INITIAL.
                    lv_prop_name = lv_prop_name && mc_key_separator && lv_keyval.
                  ELSE.
                    lv_prop_name = lv_keyval.
                  ENDIF.
                ENDLOOP.
              ELSE.
                LOOP AT ls_struct_sym-symbols ASSIGNING <symbol>.
                  ASSIGN <symbol>-value->* TO <value>.
                  lv_keyval = <value>.
                  CONDENSE lv_keyval.
                  IF lv_prop_name IS NOT INITIAL.
                    lv_prop_name = lv_prop_name && mc_key_separator && lv_keyval.
                  ELSE.
                    lv_prop_name = lv_keyval.
                  ENDIF.
                ENDLOOP.
              ENDIF.
              IF lv_array_opt = abap_false.
                dump_symbols( it_symbols = ls_struct_sym-symbols writer = writer opt_array = abap_false format_scope = abap_true name = lv_prop_name level = lv_level ).
              ELSE.
                dump_symbols( it_symbols = ls_struct_sym-symbols writer = writer opt_array = abap_true format_scope = abap_false name = lv_prop_name level = lv_level ).
              ENDIF.
            ENDLOOP.
            writer->close_object( ).

          ELSE.
            writer->open_array( name = name ).
            LOOP AT <table> INTO <line>.
              dump_symbols( it_symbols = ls_struct_sym-symbols writer = writer level = lv_level ).
            ENDLOOP.
            writer->close_array( ).
          ENDIF.
        ELSE.
          writer->open_array( name = name ).
          LOOP AT <table> ASSIGNING <value>.
            dump_int( data = <value> type_descr = lo_typedesc writer = writer level = lv_level ).
          ENDLOOP.
          writer->close_array( ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD dump_symbols.

    DATA: lv_level LIKE level,
          lv_name  TYPE string.

    FIELD-SYMBOLS: <value>  TYPE any,
                   <symbol> LIKE LINE OF it_symbols.

    lv_level = level + 1.

    IF format_scope = abap_true.
      writer->open_object( name = name ).
    ENDIF.

    LOOP AT it_symbols ASSIGNING <symbol>.
      ASSIGN <symbol>-value->* TO <value>.
      CHECK mv_compress = abap_false OR <symbol>-compressable = abap_false OR <value> IS NOT INITIAL OR opt_array = abap_true.

      IF opt_array = abap_false.
        lv_name = <symbol>-header.
      ELSE.
        lv_name = name. " empty for regular arrays, key name for assoc-array opt
      ENDIF.

      IF <symbol>-elem_type IS NOT INITIAL.
        dump_type <value> <symbol>-elem_type <symbol>-typekind writer <symbol>-convexit_out lv_name.
      ELSE.
        dump_int( data = <value> type_descr = <symbol>-type convexit = <symbol>-convexit_out writer = writer name = lv_name level = lv_level ).
      ENDIF.
    ENDLOOP.

    IF format_scope = abap_true.
      writer->close_object( ).
    ENDIF.

  ENDMETHOD.


  METHOD dump_type.

    dump_type_int data typekind writer convexit name.

  ENDMETHOD.


  METHOD generate.

    deserialize( EXPORTING json = json jsonx = jsonx pretty_name = pretty_name name_mappings = name_mappings CHANGING data = rr_data ).

  ENDMETHOD.


  METHOD generate_int.

    CLEAR type.

    CHECK json IS NOT INITIAL.

    TRY.
        DATA(lo_reader) = cl_json_string_reader=>create( json ).
        lo_reader->next_node( ).
        generate_int_r( EXPORTING reader = lo_reader CHANGING data = data type = type ).
      CATCH cx_root ##CATCH_ALL.
        CLEAR data.
    ENDTRY.

  ENDMETHOD.


  METHOD generate_int_r.

    DATA: lo_type       TYPE REF TO cl_abap_datadescr,
          lo_table_type TYPE REF TO cl_abap_tabledescr,
          lt_types      TYPE SORTED TABLE OF REF TO cl_abap_datadescr WITH UNIQUE KEY table_line,
          lt_fields     TYPE t_t_name_value,
          ls_name_data  LIKE LINE OF lt_fields,
          data_opt      LIKE data.

    FIELD-SYMBOLS: <data>      TYPE data,
                   <struct>    TYPE data,
                   <value>     TYPE data,
                   <name_data> LIKE LINE OF lt_fields,
                   <table>     TYPE STANDARD TABLE,
                   <table_opt> LIKE <table>.

    CLEAR type.

    CASE reader->node-type.

      WHEN if_json_node=>open_object. " { — result must be a structure
        reader->next_node( ).
        WHILE reader->node-type <> if_json_node=>close_object AND reader->node-type <> if_json_node=>final.
          ls_name_data-name = reader->node-name.
          generate_int_r( EXPORTING reader = reader CHANGING data = ls_name_data-data type = ls_name_data-type ).
          INSERT ls_name_data INTO TABLE lt_fields.
          reader->next_node( ).
        ENDWHILE.
        generate_struct( CHANGING fields = lt_fields data = data type = type ).
        IF data IS BOUND.
          ASSIGN data->* TO <struct>.
          LOOP AT lt_fields ASSIGNING <name_data>.
            ASSIGN COMPONENT sy-tabix OF STRUCTURE <struct> TO <data>.
            CHECK <name_data>-data IS NOT INITIAL.
            ASSIGN <name_data>-data->* TO <value>.
            <data> = <value>.
          ENDLOOP.
        ENDIF.

      WHEN if_json_node=>open_array. " [ — result must be a table of ref
        type = lcl_util=>so_type_reftab.
        CREATE DATA data TYPE HANDLE type.
        ASSIGN data->* TO <table>.
        reader->next_node( ).
        WHILE reader->node-type <> if_json_node=>close_array AND reader->node-type <> if_json_node=>final.
          APPEND INITIAL LINE TO <table> ASSIGNING <data>.
          generate_int_r( EXPORTING reader = reader CHANGING data = <data> type = lo_type ).
          IF lo_type IS NOT INITIAL.
            INSERT lo_type INTO TABLE lt_types.
          ENDIF.
          reader->next_node( ).
        ENDWHILE.
        IF lines( lt_types ) = 1.
          type = lo_table_type = cl_abap_tabledescr=>get( p_line_type = lo_type ).
          CREATE DATA data_opt TYPE HANDLE lo_table_type.
          ASSIGN data_opt->* TO <table_opt>.
          LOOP AT <table> ASSIGNING <data>.
            ASSIGN <data>->* TO <value>.
            APPEND <value> TO <table_opt>.
          ENDLOOP.
          data = data_opt.
        ENDIF.

      WHEN if_json_node=>string. " "value"
        DATA(lv_val) = reader->node-value.
        DATA(lv_vlen) = strlen( lv_val ).
        DATA lv_det_typekind TYPE abap_typekind.
        IF lv_vlen = 8 AND lv_val+2(1) = ':' AND lv_val+5(1) = ':' AND lv_val(2) CO '0123456789' AND lv_val+3(2) CO '0123456789'.
          type = lcl_util=>so_type_t.
          lv_det_typekind = e_typekind-time.
        ELSEIF lv_vlen >= 10 AND lv_val+4(1) = '-' AND lv_val+7(1) = '-' AND lv_val(4) CO '0123456789' AND lv_val+5(2) CO '0123456789' AND lv_val+8(2) CO '0123456789'.
          IF lv_vlen > 10 AND lv_val CA 'T'.
            IF lv_val CA '.'.
              type = lcl_util=>so_type_tsl.
              lv_det_typekind = e_typekind-tsl_iso8601.
            ELSE.
              type = lcl_util=>so_type_ts.
              lv_det_typekind = e_typekind-ts_iso8601.
            ENDIF.
          ELSE.
            type = lcl_util=>so_type_d.
            lv_det_typekind = e_typekind-date.
          ENDIF.
        ELSE.
          type = lcl_util=>so_type_s.
          lv_det_typekind = cl_abap_typedescr=>typekind_string.
        ENDIF.
        CREATE DATA data TYPE HANDLE type.
        ASSIGN data->* TO <value>.
        restore_type( EXPORTING reader = reader type_descr = type typekind = lv_det_typekind CHANGING data = <value> ).

      WHEN if_json_node=>number. " numeric
        DATA(lv_num) = reader->node-value.
        IF lv_num CA '.Ee'.
          type = lcl_util=>so_type_f.
        ELSEIF strlen( lv_num ) > 9.
          type = lcl_util=>so_type_p.
        ELSE.
          type = lcl_util=>so_type_i.
        ENDIF.
        CREATE DATA data TYPE HANDLE type.
        ASSIGN data->* TO <value>.
        <value> = lv_num.

      WHEN if_json_node=>boolean. " true / false / null
        IF reader->node-value = `true` OR reader->node-value = `false` ##NO_TEXT.
          type = lcl_util=>so_type_b.
          CREATE DATA data TYPE HANDLE type.
          ASSIGN data->* TO <value>.
          IF reader->node-value = `true` ##NO_TEXT.
            <value> = c_bool-true.
          ELSE.
            <value> = c_bool-false.
          ENDIF.
        ELSE. " null
          CLEAR data.
        ENDIF.

      WHEN if_json_node=>null.
        CLEAR data.

    ENDCASE.

  ENDMETHOD.


  METHOD generate_struct.

    DATA: lt_comp    TYPE abap_component_tab,
          lt_keys    TYPE STANDARD TABLE OF string,
          lv_invalid TYPE abap_bool,
          lv_name    TYPE string,
          ls_type    LIKE LINE OF mt_struct_type,
          lt_names   TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line,
          cache      LIKE LINE OF mt_name_mappings_ex,
          ls_comp    LIKE LINE OF lt_comp.

    FIELD-SYMBOLS: <field> LIKE LINE OF fields,
                   <cache> LIKE LINE OF mt_name_mappings_ex.

    CLEAR data.

    CHECK fields IS NOT INITIAL.

    " prepare structure type key
    LOOP AT fields ASSIGNING <field>.
      APPEND <field>-name TO lt_keys.
      IF <field>-type IS NOT INITIAL.
        APPEND <field>-type->absolute_name TO lt_keys.
      ENDIF.
    ENDLOOP.

    CONCATENATE LINES OF lt_keys INTO ls_type-keys SEPARATED BY '-'.

    READ TABLE mt_struct_type WITH TABLE KEY keys = ls_type-keys INTO ls_type.
    IF sy-subrc IS NOT INITIAL.

      LOOP AT fields ASSIGNING <field>.

        " create type name
        READ TABLE mt_name_mappings_ex WITH TABLE KEY json = <field>-name ASSIGNING <cache>.
        IF sy-subrc IS INITIAL.
          ls_comp-name = <cache>-abap.
        ELSE.
          cache-json = <field>-name.
          lv_name = <field>-name.
          REPLACE ALL OCCURRENCES OF REGEX lcl_util=>so_regex_generate_normalize IN lv_name WITH '_'.
          IF mv_pretty_name = pretty_mode-camel_case OR mv_pretty_name = pretty_mode-extended.
            REPLACE ALL OCCURRENCES OF REGEX lcl_util=>so_regex_generate_camel_case IN lv_name WITH '$1_$2'.
          ENDIF.
          TRANSLATE lv_name TO UPPER CASE.
          ls_comp-name = cache-abap = lv_name.
          INSERT cache INTO TABLE mt_name_mappings_ex.
        ENDIF.

        " detect type
        IF <field>-type IS INITIAL.
          ls_comp-type = cl_abap_refdescr=>get_ref_to_data( ).
        ELSE.
          ls_comp-type = <field>-type.
        ENDIF.

        INSERT ls_comp-name INTO TABLE lt_names.
        IF sy-subrc IS INITIAL.
          APPEND ls_comp TO lt_comp.
        ELSE.
          DELETE fields.
          lv_invalid = abap_true.
        ENDIF.
      ENDLOOP.

      TRY.
          ls_type-type = cl_abap_structdescr=>create( p_components = lt_comp p_strict = c_bool-false ).
        CATCH cx_sy_struct_creation.                    "#EC NO_HANDLER
      ENDTRY.

      IF lv_invalid = abap_false.
        INSERT ls_type INTO TABLE mt_struct_type.
      ENDIF.

    ENDIF.

    IF ls_type-type IS NOT INITIAL.
      TRY.
          CREATE DATA data TYPE HANDLE ls_type-type.
          type = ls_type-type.
        CATCH cx_sy_create_data_error.                  "#EC NO_HANDLER
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD get_fields.

    DATA: lv_name    TYPE char128,
          ls_field   LIKE LINE OF rt_fields,
          lt_symbols TYPE t_t_symbol.

    FIELD-SYMBOLS: <sym>   LIKE LINE OF lt_symbols,
                   <cache> LIKE LINE OF mt_name_mappings.

    lt_symbols = get_symbols( type_descr = type_descr data = data object = object include_aliases = abap_true ).

    LOOP AT lt_symbols ASSIGNING <sym> WHERE read_only = abap_false.
      MOVE-CORRESPONDING <sym> TO ls_field.

      " insert as UPPER CASE
      INSERT ls_field INTO TABLE rt_fields.

      " insert as lower case
      TRANSLATE ls_field-name TO LOWER CASE.
      INSERT ls_field INTO TABLE rt_fields.

      " as pretty printed
      IF mv_pretty_name <> pretty_mode-none AND mv_pretty_name <> pretty_mode-low_case.
        format_name <sym>-name mv_pretty_name ls_field-name.
        INSERT ls_field INTO TABLE rt_fields.
        " let us check for not well formed camelCase to be compatible with old logic
        lv_name = ls_field-name.
        TRANSLATE lv_name(1) TO UPPER CASE.
        ls_field-name = lv_name.
        INSERT ls_field INTO TABLE rt_fields.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_symbols.

    DATA: class_descr  TYPE REF TO cl_abap_classdescr,
          struct_descr TYPE REF TO cl_abap_structdescr,
          struct_cache TYPE t_s_struct_cache_res.

    IF type_descr->kind = cl_abap_typedescr=>kind_struct.

      struct_descr ?= type_descr.
      struct_cache = get_symbols_struct( type_descr = struct_descr data = data include_aliases = include_aliases ).
      result = struct_cache-symbols.

    ELSEIF type_descr->type_kind = cl_abap_typedescr=>typekind_class.

      class_descr ?= type_descr.
      result = get_symbols_class( type_descr = class_descr object = object ).

    ENDIF.

  ENDMETHOD.


  METHOD get_symbols_class.

    DATA: symb       LIKE LINE OF result.

    FIELD-SYMBOLS: <attr>  LIKE LINE OF cl_abap_objectdescr=>attributes,
                   <cache> LIKE LINE OF mt_name_mappings,
                   <field> TYPE any.

    LOOP AT type_descr->attributes ASSIGNING <attr>
      WHERE is_constant IS INITIAL AND alias_for IS INITIAL
      AND ( is_interface IS INITIAL OR type_kind <> cl_abap_typedescr=>typekind_intf ).
      ASSIGN object->(<attr>-name) TO <field>.
      " we can assign to public attributes or to protected and private for friend classes
      CHECK sy-subrc IS INITIAL.
      symb-name = <attr>-name.
      symb-read_only = <attr>-is_read_only.
      symb-type = type_descr->get_attribute_type( <attr>-name ).
      IF symb-type->kind = cl_abap_typedescr=>kind_elem.
        symb-elem_type ?= symb-type.
        IF mv_conversion_exits = abap_true.
          symb-convexit_in = lcl_util=>get_convexit_func( elem_descr = symb-elem_type input = abap_true ).
          symb-convexit_out = lcl_util=>get_convexit_func( elem_descr = symb-elem_type input = abap_false ).
        ENDIF.
        symb-typekind = lcl_util=>detect_typekind( type_descr = symb-elem_type convexit = symb-convexit_out numc_as_string = mv_numc_as_string bool_types = mv_bool_types bool_3state = mv_bool_3state ).
      ELSE.
        CLEAR: symb-elem_type, symb-typekind.
      ENDIF.
      is_compressable symb-type symb-name symb-compressable.
      GET REFERENCE OF <field> INTO symb-value.
      format_name symb-name mv_pretty_name symb-header.
      APPEND symb TO result.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_symbols_struct.

    DATA: comp_tab     TYPE cl_abap_structdescr=>component_table,
          sym_cache    LIKE result,
          symbol       TYPE t_s_symbol,
          struct_descr TYPE REF TO cl_abap_structdescr,
          struct_cache LIKE LINE OF mt_struct_cache.

    FIELD-SYMBOLS: <comp>   LIKE LINE OF comp_tab,
                   <symbol> LIKE symbol,
                   <cache>  LIKE LINE OF mt_name_mappings,
                   <struct> LIKE LINE OF mt_struct_cache,
                   <data>   TYPE data,
                   <field>  TYPE any.

    READ TABLE mt_struct_cache WITH TABLE KEY type_descr = type_descr include_aliases = include_aliases
    ASSIGNING <struct>.
    IF sy-subrc IS NOT INITIAL.
      struct_cache-type_descr       = type_descr.
      struct_cache-include_aliases  = include_aliases.

      CREATE DATA struct_cache-result-data TYPE HANDLE type_descr.
      INSERT struct_cache INTO TABLE mt_struct_cache ASSIGNING <struct>.
      ASSIGN <struct>-result-data->* TO <data>.

      comp_tab = type_descr->get_components( ).

      LOOP AT comp_tab ASSIGNING <comp>.
        IF <comp>-name IS NOT INITIAL AND
          ( <comp>-as_include = abap_false OR include_aliases = abap_true OR mv_expand_includes = abap_false ).
          symbol-name = <comp>-name.
          symbol-type = <comp>-type.
          IF symbol-type->kind = cl_abap_typedescr=>kind_elem.
            symbol-elem_type ?= symbol-type.
            IF mv_conversion_exits = abap_true.
              symbol-convexit_in = lcl_util=>get_convexit_func( elem_descr = symbol-elem_type input = abap_true ).
              symbol-convexit_out = lcl_util=>get_convexit_func( elem_descr = symbol-elem_type input = abap_false ).
            ENDIF.
            symbol-typekind = lcl_util=>detect_typekind( type_descr = symbol-elem_type convexit = symbol-convexit_out numc_as_string = mv_numc_as_string bool_types = mv_bool_types bool_3state = mv_bool_3state ).
          ELSE.
            CLEAR: symbol-elem_type, symbol-typekind.
          ENDIF.
          is_compressable symbol-type symbol-name symbol-compressable.
          ASSIGN COMPONENT symbol-name OF STRUCTURE <data> TO <field>.
          GET REFERENCE OF <field> INTO symbol-value.
          format_name symbol-name mv_pretty_name symbol-header.
          APPEND symbol TO <struct>-result-symbols.
        ENDIF.
        IF <comp>-as_include = abap_true AND mv_expand_includes = abap_true.
          struct_descr ?= <comp>-type.
          sym_cache = get_symbols_struct( type_descr = struct_descr include_aliases = include_aliases ).
          LOOP AT sym_cache-symbols INTO symbol.
            symbol-name = symbol-name && <comp>-suffix.
            IF symbol-type->kind = cl_abap_typedescr=>kind_elem.
              symbol-elem_type ?= symbol-type.
              IF mv_conversion_exits = abap_true.
                symbol-convexit_in = lcl_util=>get_convexit_func( elem_descr = symbol-elem_type input = abap_true ).
                symbol-convexit_out = lcl_util=>get_convexit_func( elem_descr = symbol-elem_type input = abap_false ).
              ENDIF.
            ELSE.
              CLEAR symbol-elem_type.
            ENDIF.
            is_compressable symbol-type symbol-name symbol-compressable.
            ASSIGN COMPONENT symbol-name OF STRUCTURE <data> TO <field>.
            GET REFERENCE OF <field> INTO symbol-value.
            format_name symbol-name mv_pretty_name symbol-header.
            APPEND symbol TO <struct>-result-symbols.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.

    result = <struct>-result.

    IF data IS BOUND AND data <> <struct>-result-data.
      result-data = data.
      ASSIGN data->* TO <data>.
      LOOP AT result-symbols ASSIGNING <symbol>.
        ASSIGN COMPONENT <symbol>-name OF STRUCTURE <data> TO <field>.
        GET REFERENCE OF <field> INTO <symbol>-value.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD is_compressable.
    rv_compress = abap_true.
  ENDMETHOD.


  METHOD pretty_name.

    DATA: tokens TYPE TABLE OF char128,
          cache  LIKE LINE OF mt_name_mappings.

    FIELD-SYMBOLS: <token> LIKE LINE OF tokens,
                   <cache> LIKE LINE OF mt_name_mappings.

    READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
    IF sy-subrc IS INITIAL.
      out = <cache>-json.
    ELSE.
      out = in.

      REPLACE ALL OCCURRENCES OF '__' IN out WITH '*'.

      TRANSLATE out TO LOWER CASE.
      TRANSLATE out USING '/_:_~_'.
      SPLIT out AT '_' INTO TABLE tokens.
      IF pascal_case = c_bool-true.
        LOOP AT tokens ASSIGNING <token>.
          TRANSLATE <token>(1) TO UPPER CASE.
        ENDLOOP.
      ELSE.
        LOOP AT tokens ASSIGNING <token> FROM 2.
          TRANSLATE <token>(1) TO UPPER CASE.
        ENDLOOP.
      ENDIF.

      CONCATENATE LINES OF tokens INTO out.
      REPLACE ALL OCCURRENCES OF '*' IN out WITH '_'.

      cache-abap = in.
      cache-json = out.
      INSERT cache INTO TABLE mt_name_mappings.
      INSERT cache INTO TABLE mt_name_mappings_ex.
    ENDIF.

  ENDMETHOD.


  METHOD pretty_name_ex.

    DATA: tokens TYPE TABLE OF char128,
          cache  LIKE LINE OF mt_name_mappings.

    FIELD-SYMBOLS: <token> LIKE LINE OF tokens,
                   <cache> LIKE LINE OF mt_name_mappings.

    READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
    IF sy-subrc IS INITIAL.
      out = <cache>-json.
    ELSE.
      out = in.

      TRANSLATE out TO LOWER CASE.
      TRANSLATE out USING '/_:_~_'.

      REPLACE ALL OCCURRENCES OF '__e__' IN out WITH '!'.
      REPLACE ALL OCCURRENCES OF '__n__' IN out WITH '#'.
      REPLACE ALL OCCURRENCES OF '__d__' IN out WITH '$'.
      REPLACE ALL OCCURRENCES OF '__p__' IN out WITH '%'.
      REPLACE ALL OCCURRENCES OF '__m__' IN out WITH '&'.
      REPLACE ALL OCCURRENCES OF '__s__' IN out WITH '*'.
      REPLACE ALL OCCURRENCES OF '__h__' IN out WITH '-'.
      REPLACE ALL OCCURRENCES OF '__t__' IN out WITH '~'.
      REPLACE ALL OCCURRENCES OF '__l__' IN out WITH '/'.
      REPLACE ALL OCCURRENCES OF '__c__' IN out WITH ':'.
      REPLACE ALL OCCURRENCES OF '__v__' IN out WITH '|'.
      REPLACE ALL OCCURRENCES OF '__a__' IN out WITH '@'.
      REPLACE ALL OCCURRENCES OF '__o__' IN out WITH '.'.
      REPLACE ALL OCCURRENCES OF '___'   IN out WITH '.'.

      REPLACE ALL OCCURRENCES OF '__' IN out WITH '"'.

      SPLIT out AT '_' INTO TABLE tokens.
      LOOP AT tokens ASSIGNING <token> FROM 2.
        TRANSLATE <token>(1) TO UPPER CASE.
      ENDLOOP.

      CONCATENATE LINES OF tokens INTO out.
      REPLACE ALL OCCURRENCES OF '"' IN out WITH '_'.

      cache-abap = in.
      cache-json = out.
      INSERT cache INTO TABLE mt_name_mappings.
      INSERT cache INTO TABLE mt_name_mappings_ex.
    ENDIF.

  ENDMETHOD.


  METHOD restore.

    DATA: ref_descr          TYPE REF TO cl_abap_refdescr,
          data_descr         TYPE REF TO cl_abap_datadescr,
          data_ref           TYPE REF TO data,
          object_ref         TYPE REF TO object,
          fields             LIKE field_cache,
          name_json          TYPE string,
          lo_move_cast_error TYPE REF TO cx_sy_move_cast_error,
          source_typename    TYPE string,
          target_typename    TYPE string.

    FIELD-SYMBOLS: <value>       TYPE any,
                   <field_cache> LIKE LINE OF field_cache.

    fields = field_cache.

    IF type_descr IS NOT INITIAL AND type_descr->kind = type_descr->kind_ref.
      ref_descr ?= type_descr.
      type_descr = ref_descr->get_referenced_type( ).
      IF ref_descr->type_kind = ref_descr->typekind_oref.
        IF data IS INITIAL.
          CREATE OBJECT data TYPE (type_descr->absolute_name).
        ELSE.
          type_descr = cl_abap_typedescr=>describe_by_object_ref( data ).
        ENDIF.
        object_ref ?= data.
        fields = get_fields( type_descr = type_descr object = object_ref ).
      ELSEIF ref_descr->type_kind = ref_descr->typekind_dref.
        IF data IS INITIAL.
          data_descr ?= type_descr.
          CREATE DATA data TYPE HANDLE data_descr.
        ELSE.
          type_descr = cl_abap_typedescr=>describe_by_data_ref( data ).
        ENDIF.
        data_ref ?= data.
        ASSIGN data_ref->* TO <value>.
        fields = get_fields( type_descr = type_descr data = data_ref ).
        restore( EXPORTING reader      = reader
                           type_descr  = type_descr
                           field_cache = fields
                 CHANGING  data        = <value> ).
        RETURN.
      ENDIF.
    ENDIF.

    IF fields IS INITIAL AND type_descr IS NOT INITIAL AND type_descr->kind = type_descr->kind_struct.
      GET REFERENCE OF data INTO data_ref.
      fields = get_fields( type_descr = type_descr data = data_ref ).
    ENDIF.

    CHECK reader->node-type = if_json_node=>open_object.
    reader->next_node( ).

    WHILE reader->node-type <> if_json_node=>close_object AND reader->node-type <> if_json_node=>final.

      name_json = reader->node-name.

      READ TABLE fields WITH TABLE KEY name = name_json ASSIGNING <field_cache>.
      IF sy-subrc IS NOT INITIAL.
        TRANSLATE name_json TO UPPER CASE.
        READ TABLE fields WITH TABLE KEY name = name_json ASSIGNING <field_cache>.
      ENDIF.

      IF sy-subrc IS INITIAL.
        ASSIGN <field_cache>-value->* TO <value>.
        IF mv_strict_mode = abap_true.
          TRY.
              restore_type( EXPORTING reader = reader type_descr = <field_cache>-type typekind = <field_cache>-typekind convexit = <field_cache>-convexit_in CHANGING data = <value> ).
            CATCH cx_sy_move_cast_error INTO lo_move_cast_error.
              IF lo_move_cast_error->source_typename IS NOT INITIAL AND lo_move_cast_error->source_typename(1) <> `[`.
                source_typename = name_json && |.| && lo_move_cast_error->source_typename.
              ELSE.
                source_typename = name_json && lo_move_cast_error->source_typename.
              ENDIF.
              target_typename = COND #( WHEN lo_move_cast_error->target_typename IS NOT INITIAL THEN lo_move_cast_error->target_typename
                                        WHEN <field_cache> IS ASSIGNED THEN lcl_util=>describe_type( <field_cache>-type )
                                        ELSE `?` ).
              RAISE EXCEPTION TYPE cx_sy_move_cast_error
                EXPORTING
                  previous        = lo_move_cast_error
                  source_typename = source_typename
                  target_typename = target_typename.
            CATCH cx_root INTO DATA(lo_parse_err) ##CATCH_ALL.
              RAISE EXCEPTION TYPE cx_sy_move_cast_error
                EXPORTING
                  previous        = lo_parse_err
                  source_typename = name_json.
          ENDTRY.
        ELSE.
          restore_type( EXPORTING reader = reader type_descr = <field_cache>-type typekind = <field_cache>-typekind convexit = <field_cache>-convexit_in CHANGING data = <value> ).
        ENDIF.
      ELSE.
        reader->skip_node( ).
      ENDIF.

      reader->next_node( ).

    ENDWHILE.

  ENDMETHOD.


  METHOD restore_type.

    DATA: lo_move_cast_error TYPE REF TO cx_sy_move_cast_error,
          lo_exp             TYPE REF TO cx_root,
          source_typename    TYPE string,
          target_typename    TYPE string.

    TRY.
        restore_type_int( EXPORTING reader = reader type_descr = type_descr field_cache = field_cache convexit = convexit typekind = typekind CHANGING data = data ).
      CATCH cx_sy_move_cast_error INTO lo_move_cast_error.
        CLEAR data.
        IF mv_strict_mode = abap_true.
          source_typename = lo_move_cast_error->source_typename.
          IF lo_move_cast_error->target_typename IS NOT INITIAL.
            target_typename = lo_move_cast_error->target_typename.
          ELSEIF type_descr IS BOUND.
            target_typename = lcl_util=>describe_type( type_descr ).
          ELSE.
            target_typename = `?`.
          ENDIF.
          RAISE EXCEPTION TYPE cx_sy_move_cast_error
            EXPORTING
              previous        = lo_move_cast_error
              source_typename = source_typename
              target_typename = target_typename.
        ENDIF.
      CATCH cx_sy_conversion_no_number cx_sy_conversion_overflow INTO lo_exp.
        CLEAR data.
        IF mv_strict_mode = abap_true.
          RAISE EXCEPTION TYPE cx_sy_move_cast_error EXPORTING previous = lo_exp.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD serialize.

    " **********************************************************************
    " Usage examples and documentation can be found on GitHub:
    " https://github.com/SAP/abap-to-json
    " **********************************************************************  "

    DATA(lo_json) = NEW z_ui2_json2(
      compress         = compress
      pretty_name      = pretty_name
      name_mappings    = name_mappings
      assoc_arrays     = assoc_arrays
      assoc_arrays_opt = assoc_arrays_opt
      expand_includes  = expand_includes
      numc_as_string   = numc_as_string
      conversion_exits = conversion_exits
      format_output    = format_output
      hex_as_base64    = hex_as_base64
      ts_as_iso8601    = ts_as_iso8601 ).

    r_json = lo_json->serialize_int(
      name       = name
      data       = data
      type_descr = type_descr ).

  ENDMETHOD.


  METHOD serialize_int.

    DATA: lo_descr    TYPE REF TO cl_abap_typedescr,
          lv_convexit TYPE string.

    DATA(lo_writer) = CAST cl_json_string_writer( cl_json_string_writer=>create( ) ).
    IF mv_format_output = abap_true.
      DATA(lo_json_writer) = CAST if_json_writer( lo_writer ).
      lo_json_writer->set_option( option = if_json_writer=>option_linebreaks ).
      lo_json_writer->set_option( option = if_json_writer=>option_indent ).
    ENDIF.

    lo_descr = COND #( WHEN type_descr IS NOT INITIAL THEN type_descr ELSE cl_abap_typedescr=>describe_by_data( data ) ).

    IF mv_conversion_exits = abap_true AND lo_descr->kind = cl_abap_typedescr=>kind_elem.
      lv_convexit = lcl_util=>get_convexit_func( elem_descr = CAST #( lo_descr ) input = abap_false ).
    ENDIF.

    dump_int( data = data type_descr = lo_descr convexit = lv_convexit writer = lo_writer ).
    r_json = lo_writer->get_json( ).

    IF name IS NOT INITIAL AND ( mv_compress IS INITIAL OR r_json IS NOT INITIAL ).
      r_json = |"{ name }":{ r_json }|.
    ENDIF.

  ENDMETHOD.


  METHOD restore_type_int.

    DATA: sdummy       TYPE string,
          line         TYPE REF TO data,
          key_ref      TYPE REF TO data,
          data_ref     TYPE REF TO data,
          key_value    TYPE string,
          key_name     TYPE string,
          lt_fields    LIKE field_cache,
          ls_symbols   TYPE t_s_struct_cache_res,
          lv_convexit  LIKE convexit,
          lv_typekind  LIKE typekind,
          lo_exp       TYPE REF TO cx_root,
          table_descr  TYPE REF TO cl_abap_tabledescr,
          struct_descr TYPE REF TO cl_abap_structdescr,
          ref_descr    TYPE REF TO cl_abap_refdescr,
          data_descr   TYPE REF TO cl_abap_datadescr,
          array_index  TYPE i,
          tstml        TYPE timestampl,
          date         TYPE c LENGTH 8,
          time         TYPE c LENGTH 6,
          guid         TYPE c LENGTH 32.

    FIELD-SYMBOLS: <line>      TYPE any,
                   <value>     TYPE any,
                   <data>      TYPE data,
                   <field>     LIKE LINE OF lt_fields,
                   <table>     TYPE ANY TABLE,
                   <value_sym> TYPE t_s_symbol.

    lv_convexit = convexit.
    lv_typekind = typekind.

    IF data IS SUPPLIED AND lv_typekind = e_typekind-json.
      " raw JSON passthrough
      IF reader->node-type = if_json_node=>string OR reader->node-type = if_json_node=>number OR reader->node-type = if_json_node=>boolean OR reader->node-type = if_json_node=>null.
        data = reader->node-value.
      ELSE.
        data = lcl_util=>read_json_to_string( reader ).
      ENDIF.
      RETURN.
    ENDIF.

    CASE reader->node-type.

      WHEN if_json_node=>open_object. " {

        IF data IS SUPPLIED AND mv_assoc_arrays = c_bool-true AND type_descr->kind = cl_abap_typedescr=>kind_table.
          table_descr ?= type_descr.
          data_descr = table_descr->get_table_line_type( ).
          IF table_descr->has_unique_key IS NOT INITIAL.
            reader->next_node( ).
            IF reader->node-type <> if_json_node=>close_object.
              ASSIGN data TO <table>.
              CLEAR <table>.
              CREATE DATA line LIKE LINE OF <table>.
              ASSIGN line->* TO <line>.
              lt_fields = get_fields( type_descr = data_descr data = line ).
              IF table_descr->key_defkind = table_descr->keydefkind_user AND lines( table_descr->key ) = 1.
                READ TABLE table_descr->key INDEX 1 INTO key_name.
                READ TABLE lt_fields WITH TABLE KEY name = key_name ASSIGNING <field>.
                key_ref = <field>-value.
                IF mv_assoc_arrays_opt = c_bool-true.
                  struct_descr ?= data_descr.
                  ls_symbols = get_symbols_struct( type_descr = struct_descr data = line ).
                  DELETE ls_symbols-symbols WHERE name = key_name.
                  IF lines( ls_symbols-symbols ) = 1.
                    READ TABLE ls_symbols-symbols INDEX 1 ASSIGNING <value_sym>.
                  ENDIF.
                ENDIF.
              ENDIF.
              WHILE reader->node-type <> if_json_node=>close_object AND reader->node-type <> if_json_node=>final.
                CLEAR <line>.
                key_value = reader->node-name.
                IF <value_sym> IS ASSIGNED.
                  ASSIGN <value_sym>-value->* TO <value>.
                  restore_type_int( EXPORTING reader     = reader
                                              type_descr = <value_sym>-type
                                              typekind   = <value_sym>-typekind
                                              convexit   = <value_sym>-convexit_in
                                    CHANGING  data       = <value> ).
                ELSE.
                  restore_type_int( EXPORTING reader      = reader
                                              type_descr  = data_descr
                                              typekind    = data_descr->type_kind
                                              field_cache = lt_fields
                                    CHANGING  data        = <line> ).
                ENDIF.
                IF table_descr->key_defkind = table_descr->keydefkind_user.
                  IF key_ref IS BOUND.
                    ASSIGN key_ref->* TO <value>.
                    IF <value> IS INITIAL.
                      <value> = key_value.
                    ENDIF.
                  ENDIF.
                ELSEIF <line> IS INITIAL.
                  <line> = key_value.
                ENDIF.
                INSERT <line> INTO TABLE <table>.
                reader->next_node( ).
              ENDWHILE.
            ELSE.
              CLEAR data.
            ENDIF.
          ELSE.
            restore( EXPORTING reader = reader CHANGING data = data ).
          ENDIF.

        ELSEIF data IS SUPPLIED AND type_descr->type_kind = cl_abap_typedescr=>typekind_dref.
          IF data IS INITIAL.
            ref_descr ?= type_descr.
            data_descr ?= ref_descr->get_referenced_type( ).
            IF data_descr->type_kind = data_descr->typekind_data. " REF TO DATA
              generate_int_r( EXPORTING reader = reader CHANGING data = data ).
              RETURN.
            ELSEIF data_descr->kind <> data_descr->kind_elem.
              CREATE DATA data TYPE HANDLE data_descr.
              data_ref ?= data.
              ASSIGN data_ref->* TO <data>.
              restore( EXPORTING reader     = reader
                                 type_descr = data_descr
                       CHANGING  data       = <data> ).
            ELSE. " primitive typed ref — skip object, can't deserialize
              reader->skip_node( ).
            ENDIF.
          ELSE.
            data_ref ?= data.
            type_descr = cl_abap_typedescr=>describe_by_data_ref( data_ref ).
            ASSIGN data_ref->* TO <data>.
            restore_type_int( EXPORTING reader = reader type_descr = type_descr typekind = type_descr->type_kind CHANGING data = <data> ).
          ENDIF.

        ELSEIF data IS SUPPLIED.
          restore( EXPORTING reader      = reader
                             type_descr  = type_descr
                             field_cache = field_cache
                   CHANGING  data        = data ).
        ELSE.
          restore( EXPORTING reader = reader CHANGING data = data ).
        ENDIF.

      WHEN if_json_node=>open_array. " [

        IF data IS SUPPLIED AND type_descr->type_kind = cl_abap_typedescr=>typekind_dref.
          IF data IS INITIAL.
            ref_descr ?= type_descr.
            data_descr ?= ref_descr->get_referenced_type( ).
            IF data_descr->type_kind = data_descr->typekind_data. " REF TO DATA
              generate_int_r( EXPORTING reader = reader CHANGING data = data ).
              RETURN.
            ELSEIF data_descr->kind = data_descr->kind_table. " deserialize in typed table
              CREATE DATA data TYPE HANDLE data_descr.
              data_ref ?= data.
              ASSIGN data_ref->* TO <data>.
              restore_type_int( EXPORTING reader = reader type_descr = data_descr typekind = data_descr->type_kind CHANGING data = <data> ).
            ELSE. " invalid type - skip
              reader->skip_node( ).
            ENDIF.
          ELSE.
            data_ref ?= data.
            type_descr = cl_abap_typedescr=>describe_by_data_ref( data_ref ).
            ASSIGN data_ref->* TO <data>.
            restore_type_int( EXPORTING reader = reader type_descr = type_descr typekind = type_descr->type_kind CHANGING data = <data> ).
          ENDIF.

        ELSE.
          reader->next_node( ).
          IF reader->node-type <> if_json_node=>close_array.
            IF data IS SUPPLIED AND type_descr->kind = cl_abap_typedescr=>kind_table.
              table_descr ?= type_descr.
              data_descr = table_descr->get_table_line_type( ).
              ASSIGN data TO <table>.
              CLEAR <table>.
              CREATE DATA line LIKE LINE OF <table>.
              ASSIGN line->* TO <line>.
              lt_fields = get_fields( type_descr = data_descr data = line ).
              array_index = 0.
              WHILE reader->node-type <> if_json_node=>close_array AND reader->node-type <> if_json_node=>final.
                array_index = array_index + 1.
                CLEAR <line>.
                TRY.
                    restore_type_int( EXPORTING reader      = reader
                                                type_descr  = data_descr
                                                typekind    = data_descr->type_kind
                                                field_cache = lt_fields
                                      CHANGING  data        = <line> ).
                    INSERT <line> INTO TABLE <table>.
                    reader->next_node( ).
                  CATCH cx_sy_move_cast_error INTO DATA(lx_arr_move).
                    IF mv_strict_mode = abap_true.
                      DATA(lv_arr_src) = |[{ array_index }]|.
                      IF lx_arr_move->source_typename IS NOT INITIAL.
                        IF lx_arr_move->source_typename(1) <> `[`.
                          lv_arr_src = lv_arr_src && `.` && lx_arr_move->source_typename.
                        ELSE.
                          lv_arr_src = lv_arr_src && lx_arr_move->source_typename.
                        ENDIF.
                      ENDIF.
                      RAISE EXCEPTION TYPE cx_sy_move_cast_error
                        EXPORTING
                          previous        = lx_arr_move
                          source_typename = lv_arr_src
                          target_typename = lx_arr_move->target_typename.
                    ELSE.
                      CLEAR <line>.
                      INSERT <line> INTO TABLE <table>.
                      reader->next_node( ).
                    ENDIF.
                  CATCH cx_root INTO DATA(lx_arr_parse) ##CATCH_ALL.
                    IF mv_strict_mode = abap_true.
                      RAISE EXCEPTION TYPE cx_sy_move_cast_error
                        EXPORTING
                          previous        = lx_arr_parse
                          source_typename = |[{ array_index + 1 }]|
                          target_typename = lcl_util=>describe_type( data_descr ).
                    ELSE.
                      CLEAR <line>.
                      INSERT <line> INTO TABLE <table>.
                      reader->next_node( ).
                    ENDIF.
                ENDTRY.
              ENDWHILE.
            ELSE.
              " skip array - consume all elements
              WHILE reader->node-type <> if_json_node=>close_array AND reader->node-type <> if_json_node=>final.
                reader->skip_node( ).
                reader->next_node( ).
              ENDWHILE.
              IF data IS SUPPLIED. " JSON to ABAP type mismatch
                RAISE EXCEPTION TYPE cx_sy_move_cast_error.
              ENDIF.
            ENDIF.
          ELSEIF data IS SUPPLIED.
            CLEAR data.
          ENDIF.
        ENDIF.

      WHEN if_json_node=>string. " "value"

        sdummy = reader->node-value.

        IF data IS NOT SUPPLIED.
          " just skip - sdummy consumed
        ELSEIF type_descr->type_kind = cl_abap_typedescr=>typekind_dref.
          restore_dref reader data type_descr.
        ELSEIF type_descr->kind = cl_abap_typedescr=>kind_elem.

          IF lv_convexit IS NOT INITIAL.
            restore_convexit lv_convexit sdummy data.
          ENDIF.

          CASE lv_typekind.
            WHEN e_typekind-bool OR e_typekind-tribool.
              IF sdummy(1) CA 'XxTt1'.
                data = c_bool-true.
              ELSE.
                data = c_bool-false.
              ENDIF.
              RETURN.
            WHEN e_typekind-xstring.
              IF mv_hex_as_base64 IS INITIAL.
                MOVE sdummy TO data.
              ELSE.
                data = cl_http_utility=>decode_x_base64( sdummy ).
              ENDIF.
              RETURN.
            WHEN e_typekind-hex.
              FIND FIRST OCCURRENCE OF REGEX lcl_util=>so_regex_guid IN sdummy SUBMATCHES guid guid+8 guid+12 guid+16 guid+20.
              IF sy-subrc = 0.
                TRANSLATE guid TO UPPER CASE.
                data = guid.
              ELSE.
                IF mv_hex_as_base64 IS INITIAL.
                  MOVE sdummy TO data.
                ELSE.
                  data = cl_http_utility=>decode_x_base64( sdummy ).
                ENDIF.
              ENDIF.
              RETURN.
            WHEN e_typekind-date.
              FIND FIRST OCCURRENCE OF REGEX lcl_util=>so_regex_date IN sdummy SUBMATCHES date date+4 date+6.
              IF sy-subrc = 0.
                data = date.
                RETURN.
              ELSE.
                read_timestamp sdummy tstml.
                IF tstml IS NOT INITIAL.
                  CONVERT TIME STAMP tstml TIME ZONE mv_time_zone INTO DATE data.
                  RETURN.
                ELSE.
                  REPLACE FIRST OCCURRENCE OF REGEX lcl_util=>so_regex_edm_time IN sdummy WITH '$1$2$3' REPLACEMENT LENGTH DATA(lv_match).
                  IF sy-subrc = 0.
                    sdummy = sdummy(lv_match).
                  ENDIF.
                ENDIF.
              ENDIF.
            WHEN e_typekind-time.
              FIND FIRST OCCURRENCE OF REGEX lcl_util=>so_regex_time IN sdummy SUBMATCHES time time+2 time+4.
              IF sy-subrc = 0.
                data = time.
                RETURN.
              ELSE.
                read_timestamp sdummy tstml.
                IF tstml IS NOT INITIAL.
                  CONVERT TIME STAMP tstml TIME ZONE mv_time_zone INTO TIME data.
                  RETURN.
                ELSE.
                  REPLACE FIRST OCCURRENCE OF REGEX lcl_util=>so_regex_edm_time IN sdummy WITH '$4$5$6' REPLACEMENT LENGTH lv_match.
                  IF sy-subrc = 0.
                    sdummy = sdummy(lv_match).
                  ENDIF.
                ENDIF.
              ENDIF.
            WHEN e_typekind-utclong.
              read_timestamp sdummy tstml.
              IF tstml IS NOT INITIAL.
                TRY.
                    data = cl_abap_tstmp=>tstmp2utclong( timestamp = tstml ).
                    RETURN.
                  CATCH cx_sy_dyn_call_error.
                    RAISE EXCEPTION TYPE cx_sy_move_cast_error.
                ENDTRY.
              ELSE.
                RAISE EXCEPTION TYPE cx_sy_move_cast_error.
              ENDIF.
            WHEN e_typekind-ts_iso8601 OR e_typekind-tsl_iso8601.
              read_timestamp sdummy tstml.
              IF tstml IS INITIAL.
                REPLACE FIRST OCCURRENCE OF REGEX lcl_util=>so_regex_edm_time IN sdummy WITH '$1$2$3$4$5$6.$7' REPLACEMENT LENGTH lv_match.
                IF sy-subrc = 0.
                  tstml = sdummy(lv_match).
                ENDIF.
              ENDIF.
              IF tstml IS NOT INITIAL.
                data = tstml.
                IF type_descr->decimals = 0.
                  data = trunc( tstml ).
                ENDIF.
                RETURN.
              ENDIF.
            WHEN e_typekind-enum.
              TRY.
                  cl_abap_xsd=>to_value(
                    EXPORTING
                      cs  = sdummy
                    CHANGING
                      val = data ).
                  RETURN.
                CATCH cx_sy_dyn_call_error.
                  RAISE EXCEPTION TYPE cx_sy_move_cast_error. " Deserialization of enums is not supported
              ENDTRY.
          ENDCASE.
        ELSE.
          RAISE EXCEPTION TYPE cx_sy_move_cast_error. " Otherwise dumps with OBJECTS_MOVE_NOT_SUPPORTED
        ENDIF.
        TRY.
            data = sdummy.
          CATCH cx_sy_move_cast_error cx_sy_conversion_no_number cx_sy_conversion_overflow INTO lo_exp.
            CLEAR data.
            IF mv_strict_mode = abap_true.
              RAISE EXCEPTION TYPE cx_sy_move_cast_error EXPORTING previous = lo_exp.
            ENDIF.
        ENDTRY.

      WHEN if_json_node=>number. " numeric value

        sdummy = reader->node-value.

        IF data IS NOT SUPPLIED.
          " skip
        ELSEIF type_descr->type_kind = cl_abap_typedescr=>typekind_dref.
          restore_dref reader data type_descr.
        ELSEIF type_descr->kind = type_descr->kind_elem.
          IF lv_convexit IS NOT INITIAL.
            restore_convexit lv_convexit sdummy data.
          ENDIF.
          TRY.
              data = sdummy.
            CATCH cx_sy_move_cast_error cx_sy_conversion_no_number cx_sy_conversion_overflow INTO lo_exp.
              CLEAR data.
              IF mv_strict_mode = abap_true.
                RAISE EXCEPTION TYPE cx_sy_move_cast_error EXPORTING previous = lo_exp.
              ENDIF.
          ENDTRY.
        ELSE.
          RAISE EXCEPTION TYPE cx_sy_move_cast_error.
        ENDIF.

      WHEN if_json_node=>boolean. " true / false

        IF data IS SUPPLIED.
          IF type_descr->type_kind = cl_abap_typedescr=>typekind_dref.
            IF reader->node-value = 'null'.
              CLEAR data.
            ELSE.
              restore_dref reader data type_descr.
            ENDIF.
          ELSEIF type_descr->kind = type_descr->kind_elem.
            IF reader->node-value = `true` ##NO_TEXT.
              data = c_bool-true.
            ELSEIF reader->node-value = `false` ##NO_TEXT.
              IF mv_bool_3state CS type_descr->absolute_name.
                data = c_tribool-false.
              ELSE.
                data = c_bool-false.
              ENDIF.
            ELSE. " null
              CLEAR data.
            ENDIF.
          ELSE.
            IF reader->node-value = 'null'.
              CLEAR data.
            ELSE.
              RAISE EXCEPTION TYPE cx_sy_move_cast_error.
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN if_json_node=>null. " null

        IF data IS SUPPLIED.
          CLEAR data.
        ENDIF.

      WHEN OTHERS.
        " error / final node — skip
        IF data IS SUPPLIED AND mv_strict_mode = abap_true.
          RAISE EXCEPTION TYPE cx_sy_move_cast_error.
        ENDIF.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.