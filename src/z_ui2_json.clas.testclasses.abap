
*----------------------------------------------------------------------*
*       CLASS lcl_util DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

CLASS lcl_util DEFINITION FINAL FRIENDS z_ui2_json.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.

  PRIVATE SECTION.
    CLASS-DATA:
      so_regex_iso8601 TYPE REF TO cl_abap_regex.

    CLASS-METHODS:
      _escape IMPORTING in TYPE data EXPORTING out TYPE string,
      to_md5  IMPORTING iv_value TYPE string RETURNING VALUE(rv_result) TYPE string,
      read_string IMPORTING json TYPE string mark TYPE i CHANGING offset TYPE i DEFAULT 0 text TYPE string RAISING cx_sy_move_cast_error,
      read_iso8601 IMPORTING in TYPE string RETURNING VALUE(rv_tstm) TYPE timestampl,
      describe_type IMPORTING io_type_descr TYPE REF TO cl_abap_typedescr RETURNING VALUE(rv_typename) TYPE string.

ENDCLASS.                    "lcl_util DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_util IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_util IMPLEMENTATION.

  METHOD class_constructor.

    " support for ISO8601 => https://en.wikipedia.org/wiki/ISO_8601
    create_regexp so_regex_iso8601 '^(?:(\d{4})-?(\d{2})-?(\d{2}))?(?:T(\d{2}):?(\d{2})(?::?(\d{2}))?(?:[\.,](\d{0,7}))?(?:Z|(?:([+-])(\d{2})(?::?(\d{2}))?))?)?\s*$'.

  ENDMETHOD.

  METHOD describe_type.

    DATA: lv_kind_name  TYPE string,
          lv_pos        TYPE i.

    rv_typename = `?`.

    CHECK io_type_descr IS NOT INITIAL.

    FIND FIRST OCCURRENCE OF '\TYPE=' IN io_type_descr->absolute_name MATCH OFFSET lv_pos.
    IF sy-subrc IS INITIAL.
      lv_pos = lv_pos + 6.
      IF io_type_descr->absolute_name+lv_pos(1) NE '%'.
        rv_typename = io_type_descr->absolute_name+lv_pos.
      ELSE.
        CLEAR rv_typename.
      ENDIF.
    ELSE.
      rv_typename = io_type_descr->absolute_name.
    ENDIF.

    CASE io_type_descr->kind.
      WHEN cl_abap_typedescr=>kind_table.
        lv_kind_name = `TABLE`.
      WHEN cl_abap_typedescr=>kind_struct.
        lv_kind_name = `STRUCTURE`.
      WHEN cl_abap_typedescr=>kind_class.
        lv_kind_name = `CLASS`.
      WHEN cl_abap_typedescr=>kind_intf.
        lv_kind_name = `INTERFACE`.
      WHEN cl_abap_typedescr=>kind_ref.
        lv_kind_name = `REFERENCE`.
    ENDCASE.

    IF lv_kind_name IS NOT INITIAL.
      IF rv_typename IS NOT INITIAL.
        CONCATENATE lv_kind_name `(` rv_typename `)` INTO rv_typename.
      ELSE.
        rv_typename = lv_kind_name.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD read_string.

    DATA: match LIKE offset,
          pos   LIKE offset.

    " ASSERT json+offset(1) EQ `"`.

    DO.
      FIND FIRST OCCURRENCE OF `"` IN SECTION OFFSET offset OF json MATCH OFFSET pos.
      IF sy-subrc IS NOT INITIAL.
        throw_error.
      ENDIF.

      offset = pos.
      pos = pos - 1.
      " if escaped search further
      WHILE json+pos(1) EQ `\`.
        pos = pos - 1.
      ENDWHILE.
      match = ( offset - pos ) MOD 2.
      IF match NE 0.
        EXIT.
      ENDIF.
      offset = offset + 1.
    ENDDO.

    match = offset - mark.
    text = json+mark(match).

  ENDMETHOD.                    "read_string

  METHOD read_iso8601.

    DATA: offset_sign    TYPE c,
          offset_hours   TYPE c LENGTH 2,
          offset_minutes TYPE c LENGTH 2,
          stimestmp      TYPE c LENGTH 22,
          seconds        TYPE i.

    FIND FIRST OCCURRENCE OF REGEX so_regex_iso8601 IN in SUBMATCHES stimestmp stimestmp+4 stimestmp+6 stimestmp+8 stimestmp+10 stimestmp+12 stimestmp+15 offset_sign offset_hours offset_minutes.
    CHECK sy-subrc IS INITIAL.

    IF stimestmp+15(1) IS NOT INITIAL. " msec provided
      stimestmp+14(1) = '.'.
    ELSEIF stimestmp(1) IS INITIAL. " timeonly, default to current date
      stimestmp(8) = sy-datlo.
    ENDIF.

    rv_tstm = stimestmp.

    IF offset_sign IS NOT INITIAL.
      seconds = offset_hours * 3600 + offset_minutes * 60.
      IF offset_sign EQ '+'.
        rv_tstm = cl_abap_tstmp=>subtractsecs( tstmp = rv_tstm secs = seconds ).
      ELSE.
        rv_tstm = cl_abap_tstmp=>add( tstmp = rv_tstm secs = seconds ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  " Creates MD5 hash from string
  METHOD to_md5.
    DATA: lv_md5_key TYPE hash160.

    IF iv_value IS NOT INITIAL.
      CALL FUNCTION 'CALCULATE_HASH_FOR_CHAR'
        EXPORTING
          alg    = 'MD5'
          data   = iv_value
        IMPORTING
          hash   = lv_md5_key
        EXCEPTIONS
          OTHERS = 4.

      IF sy-subrc EQ 0.
        rv_result = lv_md5_key.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "to_md5

  METHOD _escape.
    out = escape( val = in format = cl_abap_format=>e_json_string ).
  ENDMETHOD.                    "_escape

ENDCLASS.                    "lcl_util IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION FINAL FRIENDS z_ui2_json.

  PUBLIC SECTION.
    DATA: id TYPE i.
    DATA: children TYPE STANDARD TABLE OF REF TO lcl_test.

    METHODS: constructor.

  PROTECTED SECTION.                                    "#EC SEC_PROTEC
    DATA: prot TYPE i.                                      "#EC NEEDED

  PRIVATE SECTION.
    DATA: priv TYPE i.                                      "#EC NEEDED

ENDCLASS.                    "lcl_test DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.

  METHOD constructor.
    priv = 1.
    prot = 2.
  ENDMETHOD.                    "constructor

ENDCLASS.                    "lcl_test IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lc_json_custom DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lc_json_custom DEFINITION FINAL INHERITING FROM z_ui2_json.
  PUBLIC SECTION.
    CLASS-METHODS:
      serialize_ex IMPORTING data          TYPE data
                             compress      TYPE bool DEFAULT c_bool-false
                             pretty_name   TYPE pretty_name_mode DEFAULT pretty_mode-none
                   RETURNING VALUE(r_json) TYPE json,
      deserialize_ex IMPORTING json        TYPE json OPTIONAL
                               pretty_name TYPE pretty_name_mode DEFAULT pretty_mode-none
                     CHANGING  data        TYPE data.

  PROTECTED SECTION.
    METHODS:
      is_compressable REDEFINITION,
      pretty_name_ex REDEFINITION,
      dump_type REDEFINITION.
ENDCLASS.                    "lc_json_custom DEFINITION

*----------------------------------------------------------------------*
*       CLASS lc_json_custom IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lc_json_custom IMPLEMENTATION.

  METHOD serialize_ex.
    DATA: lo_json  TYPE REF TO lc_json_custom.
    CREATE OBJECT lo_json
      EXPORTING
        compress         = compress
        pretty_name      = pretty_name
        assoc_arrays     = abap_true
        assoc_arrays_opt = abap_true
        expand_includes  = abap_true
        numc_as_string   = abap_true
        bool_types       = `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL\TYPE=BOOLEAN\TYPE=/UI2/BOOLEAN`
        ts_as_iso8601    = abap_true.
    r_json = lo_json->serialize_int( data = data ).
  ENDMETHOD.                    "serialize_ex

  METHOD deserialize_ex.
    DATA: lo_json TYPE REF TO lc_json_custom.
    IF json IS NOT INITIAL.
      CREATE OBJECT lo_json
        EXPORTING
          pretty_name      = pretty_name
          assoc_arrays     = abap_true
          assoc_arrays_opt = abap_true.
      TRY .
          lo_json->deserialize_int( EXPORTING json = json CHANGING data = data ).
        CATCH cx_sy_move_cast_error.                    "#EC NO_HANDLER
      ENDTRY.
    ENDIF.
  ENDMETHOD.                    "deserialize_ex

  METHOD is_compressable.
    IF type_descr->absolute_name EQ `\TYPE=STRING` OR name EQ `INITIAL`.
      rv_compress = abap_false.
    ELSE.
      rv_compress = abap_true.
    ENDIF.
  ENDMETHOD.                    "is_compressable

  METHOD pretty_name_ex.
    out = super->pretty_name_ex( in ).
    CONCATENATE out 'Xxx' INTO out.
  ENDMETHOD.                    "pretty_name

  METHOD dump_type.

    DATA: is_ddic    TYPE abap_bool,
          ddic_field TYPE dfies.

    is_ddic = type_descr->is_ddic_type( ).
    IF is_ddic EQ abap_true.
      ddic_field = type_descr->get_ddic_field( ).
      IF mv_ts_as_iso8601 EQ c_bool-true AND ddic_field-domname EQ `TZNTSTMPL`.
        r_json = data.
        CONCATENATE `"` r_json(4) `-` r_json+4(2) `-` r_json+6(2) `T` r_json+8(2) `:` r_json+10(2) `:` r_json+12(2) `.` r_json+15(7) `Z"`  INTO r_json.
        RETURN.
      ENDIF.
    ENDIF.
    IF mv_ts_as_iso8601 EQ c_bool-true AND type_descr->absolute_name EQ `\TYPE=LCM_CHANGED_ON`.
      r_json = data.
      CONCATENATE `"` r_json(4) `-` r_json+4(2) `-` r_json+6(2) `T` r_json+8(2) `:` r_json+10(2) `:` r_json+12(2) `.` r_json+15(7) `Z"`  INTO r_json.
      RETURN.
    ENDIF.

    r_json = super->dump_type( data = data type_descr = type_descr convexit = convexit ).

  ENDMETHOD. "dump_type

ENDCLASS.                    "lc_json_custom IMPLEMENTATION

* ----------------------------------------------------------------------
CLASS abap_unit_testclass DEFINITION FOR TESTING FINAL "#AU Duration Medium
  "#AU Risk_Level Harmless
INHERITING FROM z_ui2_json.
  PRIVATE SECTION.

    TYPES t_json TYPE REF TO z_ui2_json.

    CONSTANTS crlf TYPE string VALUE cl_abap_char_utilities=>cr_lf. "#EC NOTEXT

    METHODS: abap_to_json_simple_transform IMPORTING data           TYPE any
                                           RETURNING VALUE(rv_json) TYPE string.

    METHODS: deserialize_form_factor FOR TESTING.
    METHODS: deserialize_target_mapping FOR TESTING.
    METHODS: deserialize_array FOR TESTING.
    METHODS: deserialize_malformed FOR TESTING.
    METHODS: deserialize_malformed_type FOR TESTING.
    METHODS: deserialize_non_fitting_table FOR TESTING.
    METHODS: serialize_form_factor FOR TESTING.
    METHODS: serialize_table FOR TESTING.
    METHODS: serialize_numbers FOR TESTING.
    METHODS: serialize_associative_array FOR TESTING.
    METHODS: serialize_types FOR TESTING.
    METHODS: serialize_booleans FOR TESTING.
    METHODS: serialize_ref FOR TESTING.
    METHODS: serialize_enums FOR TESTING.
    METHODS: serialize_upper_camel_case FOR TESTING.
    METHODS: deserialize_camel_case FOR TESTING.
    METHODS: serialize_included_types FOR TESTING.
    METHODS: deserialize_ref FOR TESTING.
    METHODS: deserialize_types FOR TESTING.
    METHODS: deserialize_white_space FOR TESTING.
    METHODS: deserialize_news FOR TESTING.
    METHODS: deserialize_dynamic_tile FOR TESTING.
    METHODS: deserialize_associative_array FOR TESTING.
    METHODS: deserialize_array_table_line FOR TESTING.
    METHODS: deserialize_empty_structure FOR TESTING.
    METHODS: serialize_recursive FOR TESTING.
    METHODS: serialize_object FOR TESTING.
    METHODS: deserialize_object FOR TESTING.
    METHODS: deserialize_alias_type FOR TESTING.
    METHODS: deserialize_partial FOR TESTING.
    METHODS: serialize_partial FOR TESTING.
    METHODS: serialize_dynamic_type FOR TESTING.
    METHODS: custom_compressible FOR TESTING.
    METHODS: custom_pretty_name FOR TESTING.
    METHODS: custom_pretty_name2 FOR TESTING.
    METHODS: extended_pretty_name FOR TESTING.
    METHODS: name_value_map FOR TESTING.
    METHODS: dynamic_types FOR TESTING.
    METHODS: deserialze_to_read_only FOR TESTING.
    METHODS: generate_simple FOR TESTING.
    METHODS: generate_for_odata FOR TESTING.
    METHODS: deserialize_odata FOR TESTING.
    METHODS: initialize_on_deserialize FOR TESTING.
    METHODS: deserialize_ref_to_data FOR TESTING.
    METHODS: conversion_exits FOR TESTING.
    METHODS: generate_special_attr_names FOR TESTING.
    METHODS: escape_and_unescape FOR TESTING.
    METHODS: serialize_formatted FOR TESTING.
    METHODS: serialize_cycle_reference FOR TESTING.

    "! deserialize a null value for a table field in strict mode
    METHODS deserialize_strict_table_null FOR TESTING.
    "! deserialize a null value for a structure field in strict mode
    METHODS deserialize_strict_struct_null FOR TESTING.
    "! deserialize a null value for a string field in strict mode
    METHODS deserialize_strict_string_null FOR TESTING.

    "! test error message with path to the field with an invalid value<br/>
    "! try to deserialize a JSON string with an invalid value for a table type
    METHODS deser_table_invalid_value FOR TESTING.
    "! test error message with path to the field with an invalid value<br/>
    "! try to deserialize a JSON string with an invalid value for a structure type
    METHODS deser_structure_invalid_value FOR TESTING.
    "! test error message with path to the field with an invalid value<br/>
    "! try to deserialize a JSON string with an invalid value for a string type
    METHODS deser_field_invalid_value FOR TESTING.
    "! serialized timestamps with domain XSDDATETIME_Z
    METHODS serialize_time_stamp FOR TESTING.

ENDCLASS.       "abap_unit_testclass
* ----------------------------------------------------------------------
CLASS abap_unit_testclass IMPLEMENTATION.

  METHOD serialize_numbers.
    TYPES:
      BEGIN OF t_root,
        negative_i TYPE i,
        positive_i TYPE i,
        positive_n TYPE n LENGTH 6,
        negative_f TYPE f,
        positive_b TYPE int1,
        negative_s TYPE int2,
        negative_p TYPE p LENGTH 16 DECIMALS 12,
        quan       TYPE p LENGTH 13 DECIMALS 3,
      END OF t_root.

    DATA: ls_data  TYPE t_root,
          ls_data2 LIKE ls_data,
          lv_act   TYPE string,
          lv_exp   LIKE lv_act.

    ls_data-negative_i = -1.
    ls_data-positive_i = 10000.
    ls_data-positive_n = 1.
    ls_data-negative_f = '-1.7976931348623158E+30' ##LITERAL.
    ls_data-positive_b = 255.
    ls_data-negative_s = -32768.
    ls_data-negative_p = '-2343.342454332245' ##LITERAL.
    ls_data-quan       = 10000000.

    lv_exp = '{"NEGATIVE_I":-1,"POSITIVE_I":10000,"POSITIVE_N":1,"NEGATIVE_F":-1.7976931348623158E+30,"POSITIVE_B":255,"NEGATIVE_S":-32768,"NEGATIVE_P":-2343.342454332245,"QUAN":10000000.000}'.
    lv_act = serialize( data = ls_data ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of numeric types fails' ).

    deserialize( EXPORTING json = lv_act CHANGING data = ls_data2 ).
    cl_aunit_assert=>assert_equals( act = ls_data2 exp = ls_data msg = 'Deserialization of numeric types fails' ).

    lv_exp = '{"NEGATIVE_I":-1,"POSITIVE_I":10000,"POSITIVE_N":"000001","NEGATIVE_F":-1.7976931348623158E+30,"POSITIVE_B":255,"NEGATIVE_S":-32768,"NEGATIVE_P":-2343.342454332245,"QUAN":10000000.000}'.
    lv_act = serialize( data = ls_data numc_as_string = abap_true ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of numeric types fails' ).

    lv_exp = '{"negative_i":-1,"positive_i":10000,"positive_n":1,"negative_f":-1.7976931348623158E+30,"positive_b":255,"negative_s":-32768,"negative_p":-2343.342454332245,"quan":10000000.000}'.
    lv_act = serialize( data = ls_data pretty_name = pretty_mode-low_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Low case name prety printing fails' ).

  ENDMETHOD.                    "serialize_numbers

  METHOD serialize_booleans.

    TYPES:
      BEGIN OF t_data,
        bool_true           TYPE abap_bool,
        bool_false          TYPE abap_bool,
        xsdb                TYPE xsdboolean,
        tribool_true        TYPE boolean,
        tribool_false       TYPE boolean,
        tribool_undefined   TYPE boolean,
        x_bool_true         TYPE bool,
        x_bool_false        TYPE bool,
        x_tribool_true      TYPE tribool,
        x_tribool_false     TYPE tribool,
        x_tribool_undefined TYPE tribool,
      END OF t_data.

    DATA: ls_data    TYPE t_data,
          lv_json_st TYPE string,                           "#EC NEEDED
          lv_act     TYPE string,
          ls_act     TYPE t_data,
          lv_exp     LIKE lv_act.

    ls_data-bool_true           = abap_true.
    ls_data-bool_false          = abap_false.
    ls_data-xsdb                = abap_true.
    ls_data-tribool_true        = c_tribool-true.
    ls_data-tribool_false       = c_tribool-false.
    ls_data-tribool_undefined   = c_tribool-undefined.
    ls_data-x_bool_true         = c_bool-true.
    ls_data-x_bool_false        = c_bool-false.
    ls_data-x_tribool_true      = c_tribool-true.
    ls_data-x_tribool_false     = c_tribool-false.
    ls_data-x_tribool_undefined = c_tribool-undefined.

    " for reference, let us check how simple transformation works
    lv_json_st = abap_to_json_simple_transform( ls_data ).

    lv_exp = '{"BOOL_TRUE":true,"BOOL_FALSE":false,"XSDB":true,"TRIBOOL_TRUE":true,"TRIBOOL_FALSE":false,"TRIBOOL_UNDEFINED":null,"X_BOOL_TRUE":true,"X_BOOL_FALSE":false,"X_TRIBOOL_TRUE":true,"X_TRIBOOL_FALSE":false,"X_TRIBOOL_UNDEFINED":null}'.
    lv_act = serialize( data = ls_data ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of boolean types fails' ).

    deserialize( EXPORTING json = lv_act CHANGING data = ls_act ).

    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_data msg = 'Deserialization of boolean types fails' ).

    lv_exp = '{"BOOL_TRUE":true,"XSDB":true,"TRIBOOL_TRUE":true,"TRIBOOL_FALSE":false,"X_BOOL_TRUE":true,"X_TRIBOOL_TRUE":true,"X_TRIBOOL_FALSE":false}'.
    lv_act = serialize( data = ls_data compress = abap_true ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Compressed serialization of boolean types fails' ).

    CLEAR ls_act.
    deserialize( EXPORTING json = lv_act CHANGING data = ls_act ).

    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_data msg = 'Deserialization of compressed boolean types fails' ).

  ENDMETHOD.                    "serialize_booleans

  METHOD abap_to_json_simple_transform.

    """ CODE BELOW ONLY FOR 7.31 and higher """

    DATA: lo_writer        TYPE REF TO cl_sxml_string_writer,
          lv_output_length TYPE i,
          lv_jsonx         TYPE xstring.

    FIELD-SYMBOLS: <xt_json> TYPE i.

    ASSIGN ('IF_SXML=>CO_XT_JSON') TO <xt_json>.
    IF <xt_json> IS ASSIGNED.

      lo_writer = cl_sxml_string_writer=>create( type = <xt_json> ).

      CALL TRANSFORMATION id SOURCE text = data RESULT XML lo_writer.

      lv_jsonx = lo_writer->get_output( ).
      rv_json = raw_to_string( lv_jsonx ).

      " strip {"TEXT":
      rv_json = rv_json+8.

      " strip trailing }
      lv_output_length = strlen( rv_json ) - 1.
      rv_json = rv_json(lv_output_length).

    ENDIF.

  ENDMETHOD.                    "abap_to_json_simple_transform

  METHOD serialize_types.
    TYPES:
      BEGIN OF ty_s_data,
        flag    TYPE xfeld,
        char    TYPE c LENGTH 16,
        numc    TYPE n LENGTH 8,
        string  TYPE string,
        xstring TYPE xstring,
        integer TYPE i,
        float   TYPE f,
        packed  TYPE p LENGTH 10 DECIMALS 6,
        hex     TYPE x LENGTH 10,
        guid    TYPE guid_16,
        tsl     TYPE timestampl,
        tsl2    TYPE timestampl,
        tsl3    TYPE timestampl,
        ts      TYPE timestamp,
        date    TYPE d,
        time    TYPE t,
        date_i  TYPE d,
        time_i  TYPE t,
      END OF ty_s_data.

    CONSTANTS: pi TYPE p LENGTH 12 DECIMALS 14 VALUE '3.14159265358979',
               tz LIKE sy-zonlo VALUE 'UTC'.

    DATA: ls_data    TYPE ty_s_data,
          ls_data2   TYPE ty_s_data,
          lv_xml_st  TYPE string,
          lv_json_st TYPE string,                           "#EC NEEDED
          lv_act     TYPE string,
          lv_exp     LIKE lv_act.

    ls_data-flag    = abap_true.
    ls_data-char    = '"TEST\"/C:\temp'.
    ls_data-numc    = 12345678.
    ls_data-string  = 'ABCDEFG'.
    ls_data-xstring = ls_data-string.
    ls_data-integer = 42.
    ls_data-float   = pi.
    ls_data-packed  = pi.
    ls_data-hex     = 987654321.
    ls_data-tsl     = '20151002134450.5545900' ##LITERAL.
    ls_data-tsl2    = '20191227160050.4540000' ##LITERAL.
    ls_data-tsl3    = '20191227160050.4540000' ##LITERAL.
    ls_data-ts      = '20160708123456' ##LITERAL.

    " ISO8601 regexp test (+ positive - negative)
    " ===========================================

    "+"2015-10-02T13:44:50.5545900Z
    "+"2019-12-27T16:00:50.4540000Z
    "+"2016-07-08T12:34:56Z
    "+"2016-07-08T12:34:56+02:30
    "+"2016-07-08
    "+"T12:34:56Z
    "+"T09:30Z
                                                            "+"T0930Z
    "+"T14:45:15Z
    "+"2023-03-08T00:00:00
    "?"2024-01-06T11:30:00-ABS
    "-"20151002134450.5545900
    "-"50
    "-"-10
    "-"+05
    "-"-10.00

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = ls_data-guid.                        "#EC FB_OLDED

    CONVERT TIME STAMP ls_data-ts TIME ZONE tz INTO DATE ls_data-date TIME ls_data-time.

    " for reference, let us check how simple transformation works
    lv_json_st = abap_to_json_simple_transform( ls_data ).

    " and XML
    CALL TRANSFORMATION id SOURCE text = ls_data RESULT XML lv_xml_st.

    CLEAR ls_data-guid.

    CONCATENATE `{"FLAG":true,"CHAR":"\"TEST\\\"/C:\\temp","NUMC":12345678,"STRING":"ABCDEFG","XSTRING":"q83v","INTEGER":42,"FLOAT":3.1415926535897900E+00,`
                `"PACKED":3.141593,"HEX":"AAAAAAAAOt5osQ==","GUID":"","TSL":20151002134450.5545900,"TSL2":20191227160050.4540000,"TSL3":20191227160050.4540000,"TS":20160708123456,"DATE":"2016-07-08","TIME":"12:34:56","DATE_I":"","TIME_I":""}`
                INTO lv_exp.
    lv_act    = serialize( data = ls_data ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of data types fails' ).

    deserialize( EXPORTING json = lv_act CHANGING data = ls_data2 ).
    cl_aunit_assert=>assert_equals( act = ls_data2 exp = ls_data msg = 'Deserialization of data types fails' ).

    CONCATENATE `{"FLAG":true,"CHAR":"\"TEST\\\"/C:\\temp","NUMC":12345678,"STRING":"ABCDEFG","XSTRING":"ABCDEF","INTEGER":42,"FLOAT":3.1415926535897900E+00,`
                `"PACKED":3.141593,"HEX":"0000000000003ADE68B1","GUID":"","TSL":20151002134450.5545900,"TSL2":20191227160050.4540000,"TSL3":20191227160050.4540000,"TS":20160708123456,"DATE":"2016-07-08","TIME":"12:34:56","DATE_I":"","TIME_I":""}`
                INTO lv_exp.
    lv_act    = serialize( data = ls_data hex_as_base64 = abap_false ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of hex without base64 fails' ).

    deserialize( EXPORTING json = lv_act hex_as_base64 = abap_false CHANGING data = ls_data2 ).
    cl_aunit_assert=>assert_equals( act = ls_data2 exp = ls_data msg = 'Deserialization of hex without base64 fails' ).

    CONCATENATE `{"FLAG":true,"CHAR":"\"TEST\\\"/C:\\temp","NUMC":12345678,"STRING":"ABCDEFG","XSTRING":"q83v","INTEGER":42,"FLOAT":3.1415926535897900E+00,`
                `"PACKED":3.141593,"HEX":"AAAAAAAAOt5osQ==","GUID":"","TSL":"2015-10-02T13:44:50.5545900Z","TSL2":"2019-12-27T16:00:50.4540000Z","TSL3":"2019-12-27T16:00:50.4540000Z","TS":"2016-07-08T12:34:56Z","DATE":"2016-07-08","TIME":"12:34:56",`
                `"DATE_I":"","TIME_I":""}`
                INTO lv_exp.
    lv_act    = serialize( ts_as_iso8601 = abap_true data = ls_data ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of timestamp into ISO8601 fails' ).

    deserialize( EXPORTING json = lv_act CHANGING data = ls_data2 ).
    cl_aunit_assert=>assert_equals( act = ls_data2 exp = ls_data msg = 'Deserialization of timestamp in ISO8601 fails' ).

    CONCATENATE `{"FLAG":true,"CHAR":"\"TEST\\\"\/C:\\temp","NUMC":12345678,"STRING":"ABCDEFG","XSTRING":"q83v","INTEGER":42,"FLOAT":3.1415926535897900E+00,`
            `"PACKED":3.141593,"HEX":"AAAAAAAAOt5osQ==","GUID":"","TSL":"2015-10-02T13:44:50.5545900Z","TSL2":"2019-12-27T16:00:50.4540000Z","TSL3":"20191227160050.4540000","TS":"2016-07-08T12:34:56Z","DATE":"2016-07-08","TIME":"12:34:56",`
            `"DATE_I":"","TIME_I":""}` INTO lv_act.

    deserialize( EXPORTING json = lv_act CHANGING data = ls_data2 ).
    cl_aunit_assert=>assert_equals( act = ls_data2 exp = ls_data msg = 'Deserialization of redunant escapment fails' ).

    " https://blogs.sap.com/2017/01/05/date-and-time-in-sap-gateway-foundation/
    CONCATENATE `{"FLAG":true,"CHAR":"\"TEST\\\"/C:\\temp","NUMC":12345678,"STRING":"ABCDEFG","XSTRING":"q83v","INTEGER":42,"FLOAT":3.1415926535897900E+00,`
            `"PACKED":3.141593,"HEX":"AAAAAAAAOt5osQ==","GUID":"","TSL":"2015-10-02T13:44:50.5545900Z","TSL2":"\/Date(1577462450454)\/","TSL3":"\/Date(1577462450454)\/","TS":"\/Date(1467981296000)\/","DATE":"2016-07-08","TIME":"12:34:56",`
            `"DATE_I":"","TIME_I":""}` INTO lv_act.

    deserialize( EXPORTING json = lv_act CHANGING data = ls_data2 ).
    cl_aunit_assert=>assert_equals( act = ls_data2 exp = ls_data msg = 'Deserialization of timestamp in Edm.DateTime fails' ).

    " https://blogs.sap.com/2017/01/05/date-and-time-in-sap-gateway-foundation/
    CONCATENATE `{"FLAG":true,"CHAR":"\"TEST\\\"/C:\\temp","NUMC":12345678,"STRING":"ABCDEFG","XSTRING":"q83v","INTEGER":42,"FLOAT":3.1415926535897900E+00,`
            `"PACKED":3.141593,"HEX":"AAAAAAAAOt5osQ==","GUID":"","TSL":"2015-10-02T13:44:50.5545900Z","TSL2":"\/Date(1577462450454)\/","TSL3":"\/Date(1577462450454)\/","TS":"\/Date(1467981296000)\/","DATE":"2016-07-08","TIME":"PT12H34M56S",`
            `"DATE_I":"","TIME_I":""}` INTO lv_act.

    deserialize( EXPORTING json = lv_act CHANGING data = ls_data2 ).
    cl_aunit_assert=>assert_equals( act = ls_data2 exp = ls_data msg = 'Deserialization of timestamp in Edm.Time fails' ).

    DATA:
      BEGIN OF ls_test,
        date  TYPE d VALUE '20230308',
        date2 TYPE d VALUE '20160926',
        ts    TYPE timestampl VALUE '20151002134450.5545900',
        p1    TYPE p LENGTH 10 DECIMALS 6 VALUE 50,
        p2    TYPE p LENGTH 10 DECIMALS 6 VALUE -10,
        p3    TYPE p LENGTH 10 DECIMALS 6 VALUE 5,
        p4    TYPE p LENGTH 10 DECIMALS 6 VALUE -10,
      END OF ls_test.

    DATA: ls_test_act LIKE ls_test.

    lv_act = `{ "Date":"2023-03-08T00:00:00", "date2":"2016-09-26 00:00", "ts":"20151002134450.5545900", "p1":"50", "p2":"-10", "p3":"+05", "p4":"-10.00" }`.
    deserialize( EXPORTING json = lv_act CHANGING  data = ls_test_act ).

    cl_aunit_assert=>assert_equals( act = ls_test_act exp = ls_test msg = 'Negative test for deserialization of ISO8601 fails' ).

    DATA: lv_act_tsmp TYPE timestamp,
          lv_exp_tsmp LIKE lv_act_tsmp.

    lv_act = `"2024-02-01T00:00:00+01:00"`.

    deserialize( EXPORTING json = lv_act CHANGING data = lv_act_tsmp ).
    lv_exp_tsmp = '20240131230000' ##LITERAL.

    cl_aunit_assert=>assert_equals( act = lv_act_tsmp exp = lv_exp_tsmp msg = 'Deserialization of of ISO8601 field with time offset (+1:00) fails' ).

  ENDMETHOD.                    "serialize_types

  METHOD serialize_associative_array.
    TYPES:
      BEGIN OF ty_s_data,
        key1   TYPE string,
        key2   TYPE i,
        value1 TYPE string,
      END OF ty_s_data.

    DATA: lt_table1  TYPE STANDARD TABLE OF ty_s_data,
          lt_table2  TYPE SORTED TABLE OF ty_s_data WITH NON-UNIQUE KEY key1,
          lt_table3  TYPE SORTED TABLE OF ty_s_data WITH UNIQUE KEY key1,
          lt_table4  TYPE HASHED TABLE OF ty_s_data WITH UNIQUE KEY key1,
          lt_table5  TYPE HASHED TABLE OF ty_s_data WITH UNIQUE KEY key1 key2,
          lt_table6  TYPE HASHED TABLE OF ty_s_data WITH UNIQUE KEY table_line,
          ls_table   TYPE ty_s_data,
          lv_counter TYPE n LENGTH 2.

    DATA: lv_act TYPE string,
          lv_exp LIKE lv_act.

    DO 3 TIMES.
      CONCATENATE `k1` lv_counter INTO ls_table-key1.
      ls_table-key2 = lv_counter.
      CONCATENATE `v1` lv_counter INTO ls_table-value1.
      APPEND ls_table TO lt_table1.
      lv_counter = lv_counter + 1.
    ENDDO.

    lt_table6 = lt_table5 = lt_table4 = lt_table3 = lt_table2 = lt_table1.

    lv_exp = '[{"key1":"k100","key2":0,"value1":"v100"},{"key1":"k101","key2":1,"value1":"v101"},{"key1":"k102","key2":2,"value1":"v102"}]'.
    lv_act = serialize( data = lt_table1 pretty_name = pretty_mode-camel_case assoc_arrays = abap_true ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of standard fails' ).

    lv_exp = '[{"key1":"k100","key2":0,"value1":"v100"},{"key1":"k101","key2":1,"value1":"v101"},{"key1":"k102","key2":2,"value1":"v102"}]'.
    lv_act = serialize( data = lt_table2 pretty_name = pretty_mode-camel_case assoc_arrays = abap_true ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of sorted table with non-unique fails' ).

    lv_exp = '{"k100":{"key2":0,"value1":"v100"},"k101":{"key2":1,"value1":"v101"},"k102":{"key2":2,"value1":"v102"}}'.
    lv_act = serialize( data = lt_table3 pretty_name = pretty_mode-camel_case assoc_arrays = abap_true ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of sorted table with unique fails' ).

    lv_exp = '{"k100":{"key2":0,"value1":"v100"},"k101":{"key2":1,"value1":"v101"},"k102":{"key2":2,"value1":"v102"}}'.
    lv_act = serialize( data = lt_table4 pretty_name = pretty_mode-camel_case assoc_arrays = abap_true ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of hashed table with single unique key fails' ).

    lv_exp = '{"k100-0":{"key1":"k100","key2":0,"value1":"v100"},"k101-1":{"key1":"k101","key2":1,"value1":"v101"},"k102-2":{"key1":"k102","key2":2,"value1":"v102"}}'.
    lv_act = serialize( data = lt_table5 pretty_name = pretty_mode-camel_case assoc_arrays = abap_true ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of hashed table with multi unique keys fails' ).

    lv_exp = '{"k100-0-v100":{"key1":"k100","key2":0,"value1":"v100"},"k101-1-v101":{"key1":"k101","key2":1,"value1":"v101"},"k102-2-v102":{"key1":"k102","key2":2,"value1":"v102"}}'.
    lv_act = serialize( data = lt_table6 pretty_name = pretty_mode-camel_case assoc_arrays = abap_true ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of hashed table with unique key as table line fails' ).

    CONCATENATE '{' crlf
                 '  "k100-0-v100": {' crlf
                 '    "key1": "k100",' crlf
                 '    "key2": 0,' crlf
                 '    "value1": "v100"' crlf
                 '  },' crlf
                 '  "k101-1-v101": {' crlf
                 '    "key1": "k101",' crlf
                 '    "key2": 1,' crlf
                 '    "value1": "v101"' crlf
                 '  },' crlf
                 '  "k102-2-v102": {' crlf
                 '    "key1": "k102",' crlf
                 '    "key2": 2,' crlf
                 '    "value1": "v102"' crlf
                 '  }' crlf
                 '}'
                 INTO lv_exp.
    lv_act = serialize( data = lt_table6 pretty_name = pretty_mode-camel_case assoc_arrays = abap_true format_output = abap_true ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of hashed table with unique key and formatting fails' ).

    " Serializing of hash maps with empty values and the parameters assoc_arrays_opt and compress produces invalid json.
    " There is no placeholder for the empty value in the result string, for example '{"key1":}'.
    " https://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer?focusedCommentId=579405779#comment-579405779

    TYPES:
      BEGIN OF t_int_tab,
        key   TYPE string,
        value TYPE int4,
      END OF t_int_tab,
      BEGIN OF t_str_tab,
        key   TYPE string,
        value TYPE string,
      END OF t_str_tab,
      BEGIN OF t_data,
        f1 TYPE string,
        f2 TYPE HASHED TABLE OF t_int_tab WITH UNIQUE KEY key,
        f3 TYPE HASHED TABLE OF t_str_tab WITH UNIQUE KEY key,
      END OF t_data.

    DATA: ls_data TYPE t_data,
          ls_f2   LIKE LINE OF ls_data-f2,
          ls_f3   LIKE LINE OF ls_data-f3.

    ls_f2-key   = `key1`.
    ls_f2-value = 1.
    INSERT ls_f2 INTO TABLE ls_data-f2.

    ls_f2-key   = `key2`.
    ls_f2-value = 0.
    INSERT ls_f2 INTO TABLE ls_data-f2.

    ls_f3-key   = `key1`.
    ls_f3-value = `1`.
    INSERT ls_f3 INTO TABLE ls_data-f3.

    ls_f3-key   = `key2`.
    ls_f3-value = ``.
    INSERT ls_f3 INTO TABLE ls_data-f3.

    lv_exp = `{"F2":{"key1":1,"key2":0},"F3":{"key1":"1","key2":""}}`.
    lv_act = serialize( data = ls_data compress = abap_true assoc_arrays = abap_true assoc_arrays_opt = abap_true ).
    cl_aunit_assert=>assert_equals( exp = lv_exp act = lv_act ).

    CONCATENATE '{' crlf
                 '  "F2": {' crlf
                 '    "key1": 1,' crlf
                 '    "key2": 0' crlf
                 '  },' crlf
                 '  "F3": {' crlf
                 '    "key1": "1",' crlf
                 '    "key2": ""' crlf
                 '  }' crlf
                 '}'
                 INTO lv_exp.
    lv_act = serialize( data = ls_data compress = abap_true assoc_arrays = abap_true assoc_arrays_opt = abap_true format_output = abap_true ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of hashed table with array optimization and formatting fails' ).

  ENDMETHOD.                    "serialize_associative_array

  METHOD serialize_form_factor.
    TYPES:
      BEGIN OF t_form_factor,
        desktop TYPE abap_bool,
        tablet  TYPE boolean,
        phone   TYPE abap_bool,
      END OF t_form_factor,
      BEGIN OF t_form_factors,
        app_default TYPE boole_d,
        manual      TYPE t_form_factor,
      END OF t_form_factors,
      BEGIN OF t_root,
        form_factors TYPE t_form_factors,
      END OF t_root.

    DATA: ls_data TYPE t_root,
          lv_act  TYPE string,
          lv_exp  LIKE lv_act.

    ls_data-form_factors-app_default    = abap_true.
    ls_data-form_factors-manual-desktop = abap_false.
    ls_data-form_factors-manual-tablet  = abap_true.
    ls_data-form_factors-manual-phone   = abap_true.

    lv_exp = '{"formFactors":{"appDefault":true,"manual":{"desktop":false,"tablet":true,"phone":true}}}'.
    lv_act = serialize( data = ls_data pretty_name = pretty_mode-camel_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of form factor structure fails' ).

    lv_exp = '{"formFactors":{"appDefault":true,"manual":{"tablet":true,"phone":true}}}'.
    lv_act = serialize( data = ls_data pretty_name = pretty_mode-camel_case compress = abap_true ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of form factor structure with compression fails' ).

    lv_exp = '{"FORM_FACTORS":{"APP_DEFAULT":true,"MANUAL":{"TABLET":true,"PHONE":true}}}'.
    lv_act = serialize( data = ls_data pretty_name = abap_false compress = abap_true ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of form factor structure with NO PRETTY NAME fails' ).

  ENDMETHOD.                    "serialize_form_factor

  METHOD serialize_table.

    TYPES:
      BEGIN OF t_form_factor,
        desktop TYPE abap_bool,
        tablet  TYPE boolean,
        phone   TYPE boole_d,
      END OF t_form_factor,
      BEGIN OF t_line,
        index   TYPE i,
        user    LIKE sy-uname,
        client  LIKE sy-mandt,
        ff      TYPE t_form_factor,
        strings TYPE string_table.
        INCLUDE   TYPE t_form_factor.
    TYPES: END OF t_line .
    TYPES: t_table TYPE HASHED TABLE OF t_line WITH UNIQUE KEY index.

    DATA: lt_data TYPE t_table,
          ls_data LIKE LINE OF lt_data,
          lv_act  TYPE string,
          lv_exp  LIKE lv_act.

    ls_data-index       = 1.
    ls_data-user        = 'USER1'.
    ls_data-client      = '000'.
    ls_data-ff-desktop  = abap_false.
    ls_data-ff-tablet   = c_tribool-true.
    ls_data-ff-phone    = abap_false.
    ls_data-desktop     = abap_true.
    ls_data-tablet      = c_tribool-false.
    ls_data-phone       = abap_true.

    APPEND 'ABC' TO ls_data-strings.
    APPEND 'BCD' TO ls_data-strings.

    INSERT ls_data INTO TABLE lt_data.

    CLEAR: ls_data.

    ls_data-index       = 2.
    ls_data-user        = 'USER2'.
    ls_data-client      = '111'.
    ls_data-ff-desktop  = abap_true.
    ls_data-ff-tablet   = c_tribool-true.
    ls_data-ff-phone    = abap_false.
    ls_data-desktop     = abap_false.
    ls_data-tablet      = c_tribool-false.
    ls_data-phone       = abap_true.

    APPEND 'DEF' TO ls_data-strings.

    INSERT ls_data INTO TABLE lt_data.

    CONCATENATE `[{"index":1,"user":"USER1","client":"000","ff":{"desktop":false,"tablet":true,"phone":false},"strings":["ABC","BCD"],"desktop":true,"tablet":false,"phone":true},`
                `{"index":2,"user":"USER2","client":"111","ff":{"desktop":true,"tablet":true,"phone":false},"strings":["DEF"],"desktop":false,"tablet":false,"phone":true}]`
                INTO lv_exp.

    lv_act = serialize( data = lt_data pretty_name = pretty_mode-camel_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of the table in JSON fails' ).

  ENDMETHOD.                    "serialize_table

  METHOD serialize_ref.

    DATA: lt_data TYPE abap_parmbind_tab,
          ls_data LIKE LINE OF lt_data,
          lv_int  TYPE int4,
          lv_act  TYPE string,
          lv_exp  LIKE lv_act.

    ls_data-name = 'INTERGER'.
    ls_data-kind = 'E'.
    lv_int = 3.
    GET REFERENCE OF lv_int INTO ls_data-value.
    INSERT ls_data INTO TABLE lt_data.

    lv_exp = `[{"NAME":"INTERGER","KIND":"E","VALUE":3}]`.
    lv_act = serialize( data = lt_data ).

*    DATA: xml  TYPE string.
*    CALL TRANSFORMATION id OPTIONS data_refs = 'embedded'
*                           SOURCE lt_data = lt_data
*                           RESULT XML xml.                  "#EC NOTEXT

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of data reference fails' ).

  ENDMETHOD.                    "serialize_ref

  METHOD serialize_enums.
                                                            "#EC NEEDED
    " that does not compile in releases below SAP_BASIS 7.50

*    TYPES:
*      BEGIN OF ENUM tsize1,
*        size1_s,
*        size1_m,
*        size1_l,
*        size1_xl,
*      END OF ENUM tsize1.
*
*    TYPES:
*      basetype TYPE c LENGTH 2,
*      BEGIN OF ENUM tsize2 BASE TYPE basetype,
*        size2_i  VALUE IS INITIAL,
*        size2_s  VALUE `S`,
*        size2_m  VALUE `M`,
*        size2_l  VALUE `L`,
*        size2_xl VALUE `XL`,
*      END OF ENUM tsize2.
*
*    TYPES:
*      BEGIN OF t_data,
*        size1 TYPE tsize1,
*        size2 TYPE tsize2,
*      END OF t_data.
*
*    DATA:  ls_data TYPE t_data,
*           lv_act  TYPE json,
*           ls_act  LIKE ls_data,
*           lv_exp  LIKE lv_act.
*
*    ls_data-size1 = size1_m.
*    ls_data-size2 = size2_xl.
*
*    lv_exp = `{"SIZE1":"SIZE1_M","SIZE2":"SIZE2_XL"}`.
*
*    " for reference with simple transformation
*    lv_act = abap_to_json_simple_transform( ls_data ).
*    " => {"SIZE1":"SIZE1_M","SIZE2":"SIZE2_XL"}
*
*    " and with our serializer
*    lv_act = serialize( data = ls_data ).
*
*    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of enumerations fails' ).
*
*    deserialize( EXPORTING json = lv_act CHANGING data = ls_act ).
*
*    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_data msg = 'Deserialization of enumerations fails' ).

  ENDMETHOD.                    "serialize_enums

  METHOD serialize_upper_camel_case.
    DATA:
      BEGIN OF ls_data,
        __underscore      TYPE abap_bool VALUE abap_true,
        under__score      TYPE abap_bool VALUE abap_true,
        _upper_camel_case TYPE abap_bool VALUE abap_true,
        camel_case        TYPE abap_bool VALUE abap_true,
        lowcase           TYPE abap_bool VALUE abap_true,
        distance_a_b      TYPE abap_bool VALUE abap_true,
      END OF ls_data.

    DATA: ls_act LIKE ls_data,
          lv_act TYPE string,
          lv_exp LIKE lv_act.

    lv_exp = '{"_underscore":true,"under_score":true,"UpperCamelCase":true,"camelCase":true,"lowcase":true,"distanceAB":true}'.
    lv_act = serialize( data = ls_data pretty_name = pretty_mode-camel_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization in camel case fails' ).

    deserialize( EXPORTING json = lv_act pretty_name = pretty_mode-camel_case CHANGING data = ls_act ).

    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_data msg = 'Deserialization in camel case fails' ).

  ENDMETHOD.                    "serialize_upper_camel_case

  METHOD deserialize_camel_case.
    TYPES:
      BEGIN OF t_data,
        __underscore      TYPE abap_bool,
        under__score      TYPE abap_bool,
        _upper_camel_case TYPE abap_bool,
        camel_case        TYPE abap_bool,
        lowcase           TYPE abap_bool,
      END OF t_data.

    DATA: ls_act  TYPE t_data,
          ls_exp  TYPE t_data,
          lv_json TYPE string.

    ls_exp-__underscore      = abap_true.
    ls_exp-under__score      = abap_true.
    ls_exp-_upper_camel_case = abap_true.
    ls_exp-camel_case        = abap_true.
    ls_exp-lowcase           = abap_true.

    CLEAR: ls_act.
    lv_json = '{"_underscore":true,"under_score":true,"UpperCamelCase":true,"camelCase":true,"lowcase":true}'.
    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case CHANGING data = ls_act ).

    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_exp msg = 'Deserialization in camel case fails' ).

    CLEAR: ls_act.
    lv_json = '{"_underscore":true,"under_score":true,"UpperCamelCase":true,"CamelCase":true,"lowcase":true}'.
    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case CHANGING data = ls_act ).

    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_exp msg = 'Deserialization of not well formed JSON names in camel case fails' ).

  ENDMETHOD.                    "deserialize_camel_case

  METHOD serialize_included_types.
    TYPES:
      BEGIN OF lty_include_pos,
        include TYPE progname,
        pos     TYPE i,
      END OF lty_include_pos .
    TYPES:
      BEGIN OF lty_source_object,
        includes      TYPE SORTED TABLE OF lty_include_pos WITH UNIQUE KEY include,
        path          TYPE string,
        source        TYPE string,
        source_length TYPE i,
      END OF lty_source_object .
    TYPES:
      BEGIN OF lty_issue,
        id       TYPE string,
        msg      TYPE string,
        priority TYPE string,
        line     TYPE i,
      END OF lty_issue .
    TYPES: lty_issues TYPE STANDARD TABLE OF lty_issue  WITH DEFAULT KEY.
    TYPES: BEGIN OF lty_object,
             objtype TYPE string,
             objname TYPE string,
             issues  TYPE lty_issues.
             INCLUDE TYPE lty_source_object AS source_object RENAMING WITH SUFFIX _inc.
    TYPES: END OF lty_object .
    TYPES:
      lty_objects TYPE STANDARD TABLE OF lty_object  WITH DEFAULT KEY .
    TYPES:
      BEGIN OF lty_result,
        objects TYPE lty_objects,
      END OF lty_result .

    DATA: ls_data TYPE lty_result,
          lv_act  TYPE string,
          lv_exp  LIKE lv_act.

    DATA: ls_object TYPE LINE OF lty_objects.

    APPEND ls_object TO ls_data-objects.

    lv_exp = '{"objects":[{"objtype":"","objname":"","issues":[],"includesInc":[],"pathInc":"","sourceInc":"","sourceLengthInc":0}]}'.
    lv_act = serialize( data = ls_data pretty_name = pretty_mode-camel_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of included types with alias fails!' ).

  ENDMETHOD.                    "serialize_included_types

  METHOD deserialize_ref.

    DATA: lv_data TYPE string,
          lt_act  TYPE abap_parmbind_tab,
          ls_act  LIKE LINE OF lt_act,
          lv_int  TYPE int4,
          lv_str  TYPE string,
          lv_bool TYPE abap_bool,
          ls_data LIKE LINE OF lt_act,
          lt_exp  LIKE lt_act,
          ls_exp  LIKE LINE OF lt_exp.

    ls_data-name = 'INTERGER'.
    ls_data-kind = 'E'.
    lv_int = 3.
    GET REFERENCE OF lv_int INTO ls_data-value.
    INSERT ls_data INTO TABLE lt_exp.

    ls_data-name = 'STRING'.
    ls_data-kind = 'E'.
    lv_str = 'Test'.
    GET REFERENCE OF lv_str INTO ls_data-value.
    INSERT ls_data INTO TABLE lt_exp.

    ls_data-name = 'BOOL'.
    ls_data-kind = 'E'.
    lv_bool = abap_true.
    GET REFERENCE OF lv_bool INTO ls_data-value.
    INSERT ls_data INTO TABLE lt_exp.

    lv_data = `[{"NAME":"INTERGER","KIND":"E","VALUE":3},{"NAME":"STRING","KIND":"E","VALUE":"Test"},{"NAME":"BOOL","KIND":"E","VALUE":true}]`.
    deserialize( EXPORTING json = lv_data CHANGING data = lt_act ).

    READ TABLE lt_act INTO ls_act WITH TABLE KEY name = 'INTERGER'.
    READ TABLE lt_exp INTO ls_exp WITH TABLE KEY name = 'INTERGER'.

    cl_aunit_assert=>assert_equals( act = ls_act-name exp = ls_exp-name msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-kind exp = ls_exp-kind msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-value exp = ls_exp-value msg = 'Serialization of data reference fails' ).

    READ TABLE lt_act INTO ls_act WITH TABLE KEY name = 'STRING'.
    READ TABLE lt_exp INTO ls_exp WITH TABLE KEY name = 'STRING'.

    cl_aunit_assert=>assert_equals( act = ls_act-name exp = ls_exp-name msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-kind exp = ls_exp-kind msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-value exp = ls_exp-value msg = 'Serialization of data reference fails' ).

    READ TABLE lt_act INTO ls_act WITH TABLE KEY name = 'BOOL'.
    READ TABLE lt_exp INTO ls_exp WITH TABLE KEY name = 'BOOL'.

    cl_aunit_assert=>assert_equals( act = ls_act-name exp = ls_exp-name msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-kind exp = ls_exp-kind msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-value exp = ls_exp-value msg = 'Serialization of data reference fails' ).


  ENDMETHOD.                    "deserialize_ref

  METHOD deserialize_form_factor.

    DATA: json TYPE string.

    TYPES:
      BEGIN OF t_form_factor,
        desktop TYPE abap_bool,
        tablet  TYPE abap_bool,
        phone   TYPE abap_bool,
      END OF t_form_factor,
      BEGIN OF t_form_factors,
        app_default TYPE abap_bool,
        manual      TYPE t_form_factor,
      END OF t_form_factors,
      BEGIN OF t_root,
        form_factors TYPE t_form_factors,
      END OF t_root.

    DATA: lv_act TYPE t_root,
          lv_exp LIKE lv_act.

    lv_exp-form_factors-app_default    = abap_true.
    lv_exp-form_factors-manual-desktop = abap_false.
    lv_exp-form_factors-manual-tablet  = abap_true.
    lv_exp-form_factors-manual-phone   = abap_true.

    CONCATENATE '{ "formFactors": {'            cl_abap_char_utilities=>cr_lf
                '    "appDefault" :  true,'     cl_abap_char_utilities=>cr_lf
                '    "manual": {'               cl_abap_char_utilities=>cr_lf
                '         "desktop": false,'    cl_abap_char_utilities=>cr_lf
                '         "tablet": true,'      cl_abap_char_utilities=>cr_lf
                '         "phone": true'        cl_abap_char_utilities=>cr_lf
                '         }'                    cl_abap_char_utilities=>cr_lf
                '    }'                         cl_abap_char_utilities=>cr_lf
                '}' INTO json.

    deserialize( EXPORTING json = json pretty_name = pretty_mode-camel_case CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON fails' ).

  ENDMETHOD.       "deserialize_form_factor

  METHOD deserialize_malformed.

    DATA: json TYPE string.

    TYPES:
      BEGIN OF tp_s_rating,
        question_id TYPE string,
        value       TYPE i,
      END OF tp_s_rating,
      tp_t_rating TYPE STANDARD TABLE OF tp_s_rating WITH KEY question_id.

    DATA: lv_act TYPE tp_t_rating.

    TRY .
        json = `{"text": "x's feedback","ratings": [{"question_id":"Q1","value":3},{"question_id":"Q2","value":4},{"question_id":"Q3","value":6}`.
        deserialize( EXPORTING json = json CHANGING data = lv_act ).
      CATCH cx_sy_move_cast_error.
        CLEAR lv_act.
      CATCH cx_root.                                     "#EC CATCH_ALL
        CLEAR lv_act.
    ENDTRY.

    cl_aunit_assert=>assert_initial( act = lv_act msg = 'Deserialization of wrong JSON object fails' ).

    json = '{ "userName": "sap", "password": "123456" }'.
    DATA: BEGIN OF user,
            username TYPE string,
            password TYPE int4,
          END OF user.

    deserialize( EXPORTING json = json CHANGING data = user ).

    cl_aunit_assert=>assert_equals( act = user-username exp = 'sap'  msg = 'Deserialization of wrong JSON object fails' ).
    cl_aunit_assert=>assert_equals( act = user-password exp = 123456 msg = 'Deserialization of wrong JSON object fails' ).

    CLEAR: user.
    deserialize( EXPORTING json = `INVALID_JSON` CHANGING data = user ).
    cl_aunit_assert=>assert_initial( act = user msg = 'Deserialization of invalid JSON fails' ).

    DATA: lr_data TYPE REF TO data.
    lr_data = generate( json = `INVALID_JSON` ).
    cl_aunit_assert=>assert_initial( act = lr_data msg = 'Generation of invalid JSON fails' ).

    lr_data = generate( json = `{"error":"n \"<\"\">` ).
    cl_aunit_assert=>assert_initial( act = lr_data msg = 'Generation of invalid JSON fails' ).

  ENDMETHOD.                    "deserialize_malformed

  METHOD deserialize_non_fitting_table.

    TYPES:
      BEGIN OF tp_s_person,
        id    TYPE string,
        fname TYPE string,
        lname TYPE string,
        age   TYPE string,
        hobby TYPE string,
      END OF tp_s_person,
      tp_t_person TYPE STANDARD TABLE OF tp_s_person WITH KEY id.

    DATA: json TYPE string,
          itab TYPE tp_t_person.

    FIELD-SYMBOLS: <line> LIKE LINE OF itab.

    "*****************************************************************

    json = `[{"ID":"2222","FIRST_NAME":"Latha","LNAME":"BH","AGE":"40","HOBBY":"Reading"}]`.

    deserialize( EXPORTING json = json CHANGING data = itab ).

    cl_aunit_assert=>assert_not_initial( act = itab msg = 'Deserialization of table with missing field' ).
    READ TABLE itab INDEX 1 ASSIGNING <line>.
    cl_aunit_assert=>assert_initial( act = <line>-fname msg = 'Deserialization of table with missing field' ).

    "******************************************************************

    json = `[{"IDX":"2222","FNAMEX":"Latha","LNAMEX":"BH","AGEX":"40","HOBBYX":"Reading"}]`.

    CLEAR itab.
    deserialize( EXPORTING json = json CHANGING data = itab ).

    cl_aunit_assert=>assert_not_initial( act = itab msg = 'Deserialization of table with missing field' ).
    READ TABLE itab INDEX 1 ASSIGNING <line>.
    cl_aunit_assert=>assert_initial( act = <line> msg = 'Deserialization of table with missing field' ).

  ENDMETHOD.                    "deserialize_non_fitting_table

  METHOD deserialize_target_mapping.

    DATA: json TYPE string.

    TYPES:
      BEGIN OF t_form_factor,
        desktop TYPE abap_bool,
        tablet  TYPE abap_bool,
        phone   TYPE abap_bool,
      END OF t_form_factor,
      BEGIN OF t_form_factors,
        app_default TYPE abap_bool,
        manual      TYPE t_form_factor,
      END OF t_form_factors,
      BEGIN OF t_tm_config,
        semantic_object              TYPE string,
        semantic_action              TYPE string,
        navigation_provider          TYPE string,
        navigation_provider_role     TYPE string,
        navigation_provider_instance TYPE string,
        target_application_alias     TYPE string,
        mapping_signature            TYPE string,
        display_info_text            TYPE string,
        form_factors                 TYPE t_form_factors,
      END OF t_tm_config,
      BEGIN OF t_config,
        tile_configuration TYPE string,
      END OF t_config.

    DATA: lv_temp TYPE t_config,
          lv_act  TYPE t_tm_config,
          lv_exp  LIKE lv_act.

    lv_act-form_factors-app_default    = abap_false.
    lv_act-form_factors-manual-desktop = abap_true.
    lv_act-form_factors-manual-tablet  = abap_true.
    lv_act-form_factors-manual-phone   = abap_true.
    lv_act-display_info_text           = 'default text'.

    lv_exp = lv_act.
    lv_exp-semantic_object              = 'SalesOrder'.
    lv_exp-semantic_action              = 'showFactsheet'.
    lv_exp-navigation_provider          = 'LPD'.
    lv_exp-navigation_provider_role     = 'UI3_SRVC'.
    lv_exp-navigation_provider_instance = 'UI2_FIORI_CHECKS'.
    lv_exp-target_application_alias     = 'FactsheetApp'.
    lv_exp-display_info_text            = ''.

    CONCATENATE '{"tileConfiguration":"{\"semantic_object\":\"SalesOrder\",\"semantic_action\":\"showFactsheet\",\"navigation_provider\":\"LPD\",\"navigation_provider_role\":\"UI3_SRVC\",\"navigation_provider_instance\":\"UI2_FIORI_CHECKS\",'
                '\"target_application_alias\":\"FactsheetApp\",\"unknown\":100.00,\"display_info_text\":\"\"}"}' INTO json.

    deserialize( EXPORTING json = json pretty_name = pretty_mode-camel_case CHANGING data = lv_temp ).
    deserialize( EXPORTING json = lv_temp-tile_configuration pretty_name = pretty_mode-camel_case CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON fails' ).

**********************************************************************

    CLEAR lv_act.

    lv_act-form_factors-app_default    = abap_false.
    lv_act-form_factors-manual-desktop = abap_true.
    lv_act-form_factors-manual-tablet  = abap_true.
    lv_act-form_factors-manual-phone   = abap_true.
    lv_act-display_info_text           = 'default text'.

    lv_exp = lv_act.
    lv_exp-semantic_object              = ''.
    lv_exp-semantic_action              = ''.
    lv_exp-navigation_provider          = 'LPD'.
    lv_exp-navigation_provider_role     = ''.
    lv_exp-navigation_provider_instance = ''.
    lv_exp-target_application_alias     = ''.
    lv_exp-display_info_text            = ''.
    lv_exp-mapping_signature            = '{par1=vallkl}&[par2=Eyallk]'.
    lv_exp-form_factors-app_default    = abap_true.
    lv_exp-form_factors-manual-desktop = abap_true.
    lv_exp-form_factors-manual-tablet  = abap_true.
    lv_exp-form_factors-manual-phone   = abap_true.

    CONCATENATE  '{"tileConfiguration":"{\"semantic_object\":\"\",\"semantic_action\":\"\",\"navigation_provider\":\"LPD\",\"display_info_text\":\"\",\"form_factors\":{\"appDefault\":true,\"manual\":'
                 '{\"desktop\":true,\"tablet\":true,\"phone\":true}},\"mapping_signature\":\"{par1=vallkl}&[par2=Eyallk]\",\"rows\":[{\"mandatory\":true,\"defaultValue\":\"\",\"isRegularExpression\":true,'
                 '\"name\":\"par1\",\"value\":\"vallkl\",\"valEnabled\":true,\"defValEnabled\":false},{\"mandatory\":false,\"isRegularExpression\":false,\"value\":\"\",\"name\":\"par2\",\"defaultValue\":'
                 '\"Eyallk\",\"valEnabled\":false,\"defValEnabled\":true}]}"}' INTO json.

    deserialize( EXPORTING json = json pretty_name = pretty_mode-camel_case CHANGING data = lv_temp ).
    deserialize( EXPORTING json = lv_temp-tile_configuration pretty_name = pretty_mode-camel_case CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON with array fails' ).

**********************************************************************
    CLEAR lv_act.

    lv_act-form_factors-app_default    = abap_false.
    lv_act-form_factors-manual-desktop = abap_true.
    lv_act-form_factors-manual-tablet  = abap_true.
    lv_act-form_factors-manual-phone   = abap_true.
    lv_act-display_info_text           = 'default text'.

    lv_exp = lv_act.
    lv_exp-semantic_object              = 'Action'.
    lv_exp-semantic_action              = 'toUrlOnOtherServer'.
    lv_exp-navigation_provider          = 'LPD'.
    lv_exp-navigation_provider_role     = 'FLP_SAMPLE'.
    lv_exp-navigation_provider_instance = 'UI_INTEGRATION_SAMPLES'.
    lv_exp-target_application_alias     = 'toUrlOnOtherServer'.
    lv_exp-display_info_text            = '"Manage Products" app on another server'.
    lv_exp-mapping_signature            = '*=*'.
    lv_exp-form_factors-app_default    = abap_false.
    lv_exp-form_factors-manual-desktop = abap_true.
    lv_exp-form_factors-manual-tablet  = abap_true.
    lv_exp-form_factors-manual-phone   = abap_true.

    CONCATENATE  '{"tileConfiguration":"{\"semantic_object\":\"Action\",\"semantic_action\":\"toUrlOnOtherServer\",'
                 '\"display_title_text\":\"\",\"url\":\"\",\"ui5_component\":\"\",\"navigation_provider\":\"LPD\",'
                 '\"navigation_provider_role\":\"FLP_SAMPLE\",\"navigation_provider_instance\":\"UI_INTEGRATION_SAMPLES\",'
                 '\"target_application_id\":\"\",\"target_application_alias\":\"toUrlOnOtherServer\",'
                 '\"display_info_text\":\"\\\"Manage Products\\\" app on another server\",\"mapping_signature\":\"*=*\"}"}' INTO json.

    TRY.
        deserialize( EXPORTING json = json pretty_name = pretty_mode-camel_case CHANGING data = lv_temp ).
        deserialize( EXPORTING json = lv_temp-tile_configuration pretty_name = pretty_mode-camel_case CHANGING data = lv_act ).
      CATCH cx_sy_move_cast_error. " JSON structure is invalid
        CLEAR lv_act.
    ENDTRY.

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON with array fails' ).

  ENDMETHOD.       "deserialize_target_mapping

  METHOD deserialize_array.

    DATA: json TYPE string.

    TYPES:
      BEGIN OF t_form_factor,
        desktop TYPE abap_bool,
        tablet  TYPE abap_bool,
        phone   TYPE abap_bool,
      END OF t_form_factor,
      BEGIN OF t_form_factors,
        app_default TYPE abap_bool,
        manual      TYPE t_form_factor,
      END OF t_form_factors,
      BEGIN OF tp_s_sig_param,
        name                TYPE string,
        value               TYPE string,
        default_value       TYPE string,
        mandatory           TYPE abap_bool,
        isregularexpression TYPE abap_bool,
        val_enabled         TYPE abap_bool,
        def_val_enabled     TYPE abap_bool,
      END OF tp_s_sig_param,
      tp_t_sig_param TYPE SORTED TABLE OF tp_s_sig_param WITH NON-UNIQUE KEY name,
      BEGIN OF t_tm_config,
        semantic_object              TYPE string,
        semantic_action              TYPE string,
        navigation_provider          TYPE string,
        navigation_provider_role     TYPE string,
        navigation_provider_instance TYPE string,
        target_application_alias     TYPE string,
        mapping_signature            TYPE string,
        display_info_text            TYPE string,
        rows                         TYPE tp_t_sig_param,
        form_factors                 TYPE t_form_factors,
      END OF t_tm_config,
      BEGIN OF t_config,
        tile_configuration TYPE string,
      END OF t_config.

    DATA: lv_temp TYPE t_config,
          lv_act  TYPE t_tm_config,
          ls_row  TYPE LINE OF tp_t_sig_param,
          lv_exp  LIKE lv_act.

    lv_act-form_factors-app_default    = abap_false.
    lv_act-form_factors-manual-desktop = abap_true.
    lv_act-form_factors-manual-tablet  = abap_true.
    lv_act-form_factors-manual-phone   = abap_true.
    lv_act-display_info_text           = 'default text'.

    lv_exp = lv_act.
    lv_exp-form_factors-app_default     = abap_true.
    lv_exp-semantic_object              = 'SalesOrder'.
    lv_exp-semantic_action              = 'showFactsheet'.
    lv_exp-navigation_provider          = 'LPD'.
    lv_exp-navigation_provider_role     = 'UI3_SRVC'.
    lv_exp-navigation_provider_instance = 'UI2_FIORI_CHECKS'.
    lv_exp-target_application_alias     = 'FactsheetApp'.
    lv_exp-display_info_text            = ''.
    lv_exp-mapping_signature            = '{par1=vallkl}&[par2=Eyallk]'.

    ls_row-name                  = 'par1'.
    ls_row-value                 = 'vallkl'.
    ls_row-default_value         = ''.
    ls_row-mandatory             = abap_true.
    ls_row-isregularexpression   = abap_true.
    ls_row-val_enabled           = abap_true.
    ls_row-def_val_enabled       = abap_false.
    INSERT ls_row INTO TABLE lv_exp-rows.

    ls_row-name                  = 'par2'.
    ls_row-value                 = ''.
    ls_row-default_value         = 'Eyallk'.
    ls_row-mandatory             = abap_false.
    ls_row-isregularexpression   = abap_false.
    ls_row-val_enabled           = abap_false.
    ls_row-def_val_enabled       = abap_true.
    INSERT ls_row INTO TABLE lv_exp-rows.

    CONCATENATE  '{"tileConfiguration":"{\"semantic_object\":\"SalesOrder\",\"semantic_action\":\"showFactsheet\",\"navigation_provider\":\"LPD\",\"display_info_text\":\"\",\"form_factors\":{\"appDefault\":true,\"manual\":'
                 '{\"desktop\":true,\"tablet\":true,\"phone\":true}},\"mapping_signature\":\"{par1=vallkl}&[par2=Eyallk]\",\"rows\":[{\"mandatory\":true,\"defaultValue\":\"\",\"isRegularExpression\":true,'
                 '\"name\":\"par1\",\"value\":\"vallkl\",\"valEnabled\":true,\"defValEnabled\":false},{\"mandatory\":false,\"isRegularExpression\":false,\"value\":\"\",\"name\":\"par2\",\"defaultValue\":'
                 '\"Eyallk\",\"valEnabled\":false,\"defValEnabled\":true}],'
                 '\"target_application_alias\":\"FactsheetApp\",\"navigation_provider_role\":\"UI3_SRVC\",\"navigation_provider_instance\":\"UI2_FIORI_CHECKS\" }"}' INTO json.

    deserialize( EXPORTING json = json pretty_name = pretty_mode-camel_case CHANGING data = lv_temp ).
    deserialize( EXPORTING json = lv_temp-tile_configuration pretty_name = pretty_mode-camel_case CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON with array fails' ).

    DATA: lt_exp TYPE string_table,
          lt_act LIKE lt_exp.

    APPEND 'A' TO lt_exp.
    APPEND 'B' TO lt_exp.
    APPEND 'C' TO lt_exp.

    json = `["A", "B", "C"]`.

    deserialize( EXPORTING json = json CHANGING data = lt_act ).
    cl_aunit_assert=>assert_equals( act = lt_act exp = lt_exp msg = 'Deserialization of STRING_TABLE fails' ).

  ENDMETHOD.       "deserialize_target_mapping_array

  METHOD deserialize_types.
    TYPES:
      BEGIN OF t_struct,
        negative_i TYPE i,
        positive_i TYPE i,
        positive_n TYPE n LENGTH 6,
        timestamp  TYPE timestamp,
        boolean    TYPE abap_bool,
        special1   TYPE string,
        special2   TYPE string,
      END OF t_struct.

    DATA: lv_exp  TYPE t_struct,
          lv_act  TYPE t_struct,
          lv_data TYPE string.

    lv_exp-negative_i = -1.
    lv_exp-positive_i = 10000.
    lv_exp-positive_n = 1.
    lv_exp-timestamp  = 1419279663821.
    lv_exp-boolean    = abap_true.
    lv_exp-special1   = `2016/05/11`.
    CONCATENATE `tena` cl_abap_char_utilities=>newline `t` INTO lv_exp-special2.

    lv_data = '{"negative_i":-1,"positive_i":10000,"positive_n":1,"boolean":true,"timestamp":1419279663821,"timestamp_not_mapped":1419279663821,"special1":"2016/05/11","special2":"tena\nt"}'.
    deserialize( EXPORTING json = lv_data pretty_name = pretty_mode-low_case CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialize of types with low case name pretty printing fails' ).

    lv_data = '{"negative_i": -1, "positive_i":10000, "positive_n" : "000001", "boolean" : true, "timestamp" : 1419279663821, "timestamp_not_mapped" : 1419279663821,"special1":"2016/05/11","special2":"tena\nt"}'.
    deserialize( EXPORTING json = lv_data pretty_name = pretty_mode-low_case CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialize of types with low case name pretty printing fails' ).

  ENDMETHOD.                    "deserialize_types

  METHOD deserialize_white_space.

    DATA: lv_json TYPE json.
    DATA : lr_data  TYPE REF TO data.
    lv_json = `{    "clientid": "*******sb-7893798237498723b540", "clientsecret": "f8hdhkgjhdf=", "serviceurls": {        "AI_API_URL": "https://api.xxxx.hana.ondemand.com"    }}`.

    lr_data = generate( json = lv_json ).
    cl_aunit_assert=>assert_not_initial( act = lr_data msg = 'Generation with custom spaces fails JSON fails!' ).

  ENDMETHOD.                    "deserialize_white_space

  METHOD deserialize_news.
    TYPES: BEGIN OF tp_s_tile_news_config,
             defaultimage    TYPE string,
             cycleinterval   TYPE i,
             refreshinterval TYPE string,
             usedefaultimage TYPE abap_bool,
             feed1           TYPE string,
             feed2           TYPE string,
             feed3           TYPE string,
             feed4           TYPE string,
             feed5           TYPE string,
             feed6           TYPE string,
             feed7           TYPE string,
             feed8           TYPE string,
             feed9           TYPE string,
             feed10          TYPE string,
             ifilter1        TYPE string,
             ifilter2        TYPE string,
             ifilter3        TYPE string,
             ifilter4        TYPE string,
             ifilter5        TYPE string,
             efilter1        TYPE string,
             efilter2        TYPE string,
             efilter3        TYPE string,
             efilter4        TYPE string,
             efilter5        TYPE string,
           END OF tp_s_tile_news_config .

    DATA: lv_exp  TYPE tp_s_tile_news_config,
          lv_act  TYPE tp_s_tile_news_config,
          lv_data TYPE string.

    CONCATENATE '{"defaultImage":"http://\\","cycleInterval":"500","refreshInterval":"15 Minutes","useDefaultImage":"false",'
                '"feed1":"","feed2":"","feed3":"","feed4":"","feed5":"","feed6":"","feed7":"","feed8":"","feed9":"","feed10":"",'
                '"iFilter1":"","iFilter2":"","iFilter3":"","iFilter4":"","iFilter5":"","eFilter1":"","eFilter2":"","eFilter3":"","eFilter4":"","eFilter5":""}'
                INTO lv_data.

    lv_exp-defaultimage     = 'http://\'.
    lv_exp-cycleinterval    = '500'.
    lv_exp-refreshinterval  = '15 Minutes'.
    lv_exp-usedefaultimage  = abap_false.

    TRY.
        deserialize( EXPORTING json = lv_data pretty_name = pretty_mode-low_case CHANGING data = lv_act ).
      CATCH cx_sy_move_cast_error. " JSON structure is invalid
        CLEAR lv_act.
    ENDTRY.

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialize of types with low case name prety printing fails' ).

**********************************************************************

    CONCATENATE '{"defaultImage":"http://\\\\","cycleInterval":500,"refreshInterval":"15 Minutes","useDefaultImage":"true",'
                '"feed1":"","feed2":"","feed3":"","feed4":"","feed5":"","feed6":"","feed7":"","feed8":"","feed9":"","feed10":"",'
                '"iFilter1":"","iFilter2":"","iFilter3":"","iFilter4":"","iFilter5":"","eFilter1":"","eFilter2":"","eFilter3":"","eFilter4":"","eFilter5":""}'
                INTO lv_data.

    lv_exp-defaultimage     = 'http://\\'.
    lv_exp-cycleinterval    = '500'.
    lv_exp-refreshinterval  = '15 Minutes'.
    lv_exp-usedefaultimage  = abap_true.

    TRY.
        deserialize( EXPORTING json = lv_data pretty_name = pretty_mode-low_case CHANGING data = lv_act ).
      CATCH cx_sy_move_cast_error. " JSON structure is invalid
        CLEAR lv_act.
    ENDTRY.

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialize of types with low case name prety printing fails' ).

**********************************************************************

  ENDMETHOD.                    "deserialize_news

  METHOD deserialize_dynamic_tile.
    TYPES:
      BEGIN OF tp_s_tile_dynamic_config,
        display_icon_url               TYPE string,
        display_info_text              TYPE string,
        display_title_text             TYPE string,
        display_subtitle_text          TYPE string,
        navigation_use_semantic_object TYPE abap_bool,
        navigation_target_url          TYPE string,
        navigation_semantic_object     TYPE string,
        navigation_semantic_action     TYPE string,
        navigation_semantic_parameters TYPE string,
        display_search_keywords        TYPE string,
        display_number_unit            TYPE string,
        service_url                    TYPE string,
        service_refresh_interval       TYPE i,
      END OF tp_s_tile_dynamic_config .

    DATA: lv_exp  TYPE tp_s_tile_dynamic_config,
          lv_act  TYPE tp_s_tile_dynamic_config,
          lv_data TYPE string.

    CONCATENATE '{"display_icon_url":"","display_title_text":"","di'
                'splay_subtitle_text":"","display_info_text":"","di'
                'splay_number_unit":"","service_url":"","service_re'
                'fresh_interval":"2343q44we5e55","navigation_use_se'
                'mantic_object":true,"navigation_target_url":"#Qual'
                'ityNotificationActivity-,..,?ghgh","navigation_sem'
                'antic_object":"QualityNotificationActivity","navig'
                'ation_semantic_action":",..,","navigation_semantic'
                '_parameters":"ghgh","display_search_keywords":""}'
    INTO lv_data.

    lv_exp-navigation_use_semantic_object = abap_true.
    lv_exp-navigation_target_url = '#QualityNotificationActivity-,..,?ghgh'.
    lv_exp-navigation_semantic_object = 'QualityNotificationActivity'.
    lv_exp-navigation_semantic_action = ',..,'.
    lv_exp-navigation_semantic_parameters = 'ghgh'.

    deserialize( EXPORTING json = lv_data CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialize of dynamic tile fails!' ).


  ENDMETHOD.                    "deserialize_dynamic_tile

  METHOD deserialize_associative_array.

    TYPES:
      BEGIN OF tp_s_data,
        key    TYPE string,
        value1 TYPE string,
        value2 TYPE string,
      END OF tp_s_data,
      tp_t_data TYPE HASHED TABLE OF tp_s_data WITH UNIQUE KEY key.

    DATA: lt_exp   TYPE tp_t_data,
          ls_exp   LIKE LINE OF lt_exp,
          lt_act   TYPE tp_t_data,
          lv_lines TYPE i,
          lv_data  TYPE string.

    CONCATENATE '{ "key1": { "value1" : "test1", "value2" : "test1" },'
                '  "key2": { "value1" : "test2", "value2" : "test2" },'
                '  "key3": { "value1" : "test3", "value2" : "test3" } }'
    INTO lv_data.

    ls_exp-key    = 'key1'.
    ls_exp-value1 = 'test1'.
    ls_exp-value2 = 'test1'.
    INSERT ls_exp INTO TABLE lt_exp.

    ls_exp-key    = 'key2'.
    ls_exp-value1 = 'test2'.
    ls_exp-value2 = 'test2'.
    INSERT ls_exp INTO TABLE lt_exp.

    ls_exp-key    = 'key3'.
    ls_exp-value1 = 'test3'.
    ls_exp-value2 = 'test3'.
    INSERT ls_exp INTO TABLE lt_exp.

    deserialize( EXPORTING json = lv_data assoc_arrays = abap_true CHANGING data = lt_act ).
    lv_lines = lines( lt_act ).
    cl_aunit_assert=>assert_equals( act = lv_lines exp = 3 msg = 'Deserialize of associated array fails!' ).
    cl_aunit_assert=>assert_equals( act = lt_act exp = lt_exp msg = 'Deserialize of associated array fails!' ).

  ENDMETHOD.                    "deserialize_associative_array

  METHOD deserialize_array_table_line.

    TYPES:
     tp_t_data TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

    DATA: lt_exp   TYPE tp_t_data,
          ls_exp   LIKE LINE OF lt_exp,
          lt_act   TYPE tp_t_data,
          lv_lines TYPE i,
          lv_data  TYPE string.

    CONCATENATE '{ "key1": {},'
                '  "key2": {},'
                '  "key3": {} }'
    INTO lv_data.

    ls_exp = 'key1'.
    INSERT ls_exp INTO TABLE lt_exp.

    ls_exp = 'key2'.
    INSERT ls_exp INTO TABLE lt_exp.

    ls_exp = 'key3'.
    INSERT ls_exp INTO TABLE lt_exp.

    deserialize( EXPORTING json = lv_data assoc_arrays = abap_true CHANGING data = lt_act ).
    lv_lines = lines( lt_act ).
    cl_aunit_assert=>assert_equals( act = lv_lines exp = 3 msg = 'Deserialize of associated array with key as table_line fails!' ).
    cl_aunit_assert=>assert_equals( act = lt_act exp = lt_exp msg = 'Deserialize of associated array with key as table_line fails!' ).

  ENDMETHOD.                    "deserialize_array_table_line

  METHOD deserialize_empty_structure.

    TYPES: BEGIN OF t_cuan_loy_s_beacon,
             id       TYPE string,
             major_id TYPE i,
             minor_id TYPE i,
           END OF t_cuan_loy_s_beacon.
    TYPES: t_cuan_loy_t_beacon TYPE STANDARD TABLE OF t_cuan_loy_s_beacon WITH DEFAULT KEY.
    TYPES: BEGIN OF t_cuan_loy_s_offer,
             offer_id             TYPE string,
             offer_code           TYPE string,
             offer_type           TYPE string,
             target_group         TYPE string,
             valid_to             TYPE timestamp,
             enable_geo_marketing TYPE abap_bool,
             email_templ_id       TYPE string,
             beacon               TYPE t_cuan_loy_t_beacon,
           END OF t_cuan_loy_s_offer.
    TYPES: t_cuan_loy_t_offer TYPE STANDARD TABLE OF t_cuan_loy_s_offer WITH DEFAULT KEY.

    DATA: lt_exp    TYPE t_cuan_loy_t_offer,
          ls_exp    LIKE LINE OF lt_exp,
          ls_beacon TYPE LINE OF t_cuan_loy_t_beacon,
          lt_act    LIKE lt_exp,
          lv_data   TYPE string.

    CONCATENATE
      '[ {'
      '  "beacon" : [ { } ],'
      '  "enableGeoMarketing" : false,'
      '  "offerCode" : "lsFHQZHz",'
      '  "offerId" : "b70f045ff2214edaab8904d6b427b52d",'
      '  "offerStatus" : "INACTIVE",'
      '  "offerType" : "COUPON",'
      '  "targetGroup" : [ "31cd621d165b4c9f87d652760f414a53" ],'
      '  "validFrom" : "2016-01-14T19:02:14.320+0000",'
      '  "validTo" : "2016-01-27T19:02:14.324+0000"'
      '}, {'
      '  "beacon" : [ {'
      '    "id" : "B9407F30-F5F8-466E-AFF9-25556B57FE6D",'
      '    "majorId" : 103'
      '  } ],'
      '  "enableGeoMarketing" : true,'
      '  "offerCode" : "575tpNAk",'
      '  "offerId" : "a46e801e067e46098e93fcd1d9e34f01",'
      '  "offerStatus" : "INACTIVE",'
      '  "offerType" : "COUPON",'
      '  "targetGroup" : [ "31cd621d165b4c9f87d652760f414a53", "eb83bd07b89f462e8538cdcbf5c62164", "dcd160b0af6d495f9b8e0c6c5edae47c" ],'
      '  "validTo" : "2016-01-31T09:51:23.702+0000"'
      '}]'
    INTO lv_data.

    CLEAR: ls_exp.

    ls_exp-offer_id               = 'b70f045ff2214edaab8904d6b427b52d'.
    ls_exp-offer_code             = 'lsFHQZHz'.
    ls_exp-offer_type             = 'COUPON'.
    ls_exp-valid_to               = '20160127190214' ##LITERAL.
    ls_exp-enable_geo_marketing   = abap_false.
    APPEND ls_beacon TO ls_exp-beacon.
    APPEND ls_exp TO lt_exp.

    CLEAR: ls_exp.

    ls_exp-offer_id               = 'a46e801e067e46098e93fcd1d9e34f01'.
    ls_exp-offer_code             = '575tpNAk'.
    ls_exp-offer_type             = 'COUPON'.
    ls_exp-valid_to               = '20160131095124' ##LITERAL.
    ls_exp-enable_geo_marketing   = abap_true.

    ls_beacon-id                  = 'B9407F30-F5F8-466E-AFF9-25556B57FE6D'.
    ls_beacon-major_id            = 103.
    ls_beacon-minor_id            = 0.
    APPEND ls_beacon TO ls_exp-beacon.
    APPEND ls_exp TO lt_exp.

    deserialize( EXPORTING json = lv_data pretty_name = pretty_mode-camel_case CHANGING data = lt_act ).

    cl_aunit_assert=>assert_equals( act = lt_act exp = lt_exp msg = 'Deserialize of table with empty objects fails!' ).

  ENDMETHOD.                    "deserialize_empty_structure

  METHOD serialize_recursive.

    TYPES: BEGIN OF ts_node,
             id       TYPE i,
             children TYPE STANDARD TABLE OF REF TO data WITH DEFAULT KEY,
           END OF ts_node.

    DATA: lv_exp  TYPE string,
          lv_act  TYPE string,
          ls_data TYPE ts_node,
          lr_data LIKE REF TO ls_data.

    ls_data-id = 1.

    CREATE DATA lr_data.
    lr_data->id = 2.
    APPEND lr_data TO ls_data-children.

    lv_exp = '{"ID":1,"CHILDREN":[{"ID":2,"CHILDREN":[]}]}'.

    lv_act = serialize( data = ls_data ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of recursive data structure fails' ).

  ENDMETHOD.                    "serialize_recursive

  METHOD serialize_object.

    DATA: lv_exp   TYPE string,
          lv_act   TYPE string,
          lo_data  TYPE REF TO lcl_test,
          lo_child LIKE lo_data.

    CREATE OBJECT lo_data.
    lo_data->id = 1.

    CREATE OBJECT lo_child.
    lo_child->id = 2.
    APPEND lo_child TO lo_data->children.

    lv_exp = '{"CHILDREN":[{"CHILDREN":[],"ID":2,"PRIV":1,"PROT":2}],"ID":1,"PRIV":1,"PROT":2}'.

    lv_act = serialize( data = lo_data ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of simple recursive object fails' ).

    CONCATENATE '{' crlf
             '  "CHILDREN": [' crlf
             '    {' crlf
             '      "CHILDREN": [],' crlf
             '      "ID": 2,' crlf
             '      "PRIV": 1,' crlf
             '      "PROT": 2' crlf
             '    }' crlf
             '  ],' crlf
             '  "ID": 1,' crlf
             '  "PRIV": 1,' crlf
             '  "PROT": 2' crlf
             '}'
             INTO lv_exp.
    lv_act = serialize( data = lo_data format_output = abap_true ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of simple recursive object with formatting fails' ).

  ENDMETHOD.                    "serialize_object

  METHOD deserialize_object.

    DATA: lo_data  TYPE REF TO lcl_test,
          lv_act   TYPE i,
          lv_lines LIKE sy-tabix,
          lv_data  TYPE string.

    lv_data = '{"id":1,"children":[{"id":2,"children":[]}]}'.
    deserialize( EXPORTING json = lv_data CHANGING data =  lo_data ).

    cl_aunit_assert=>assert_not_initial( act = lo_data msg = 'Deserialization of simple recursive object fails' ).

    lv_act = lo_data->id.
    cl_aunit_assert=>assert_equals( act = lv_act exp = 1 msg = 'Deserialization of simple recursive object fails' ).

    lv_lines = lines( lo_data->children ).
    cl_aunit_assert=>assert_equals( act = lv_lines exp = 1 msg = 'Deserialization of simple recursive object fails' ).

    READ TABLE lo_data->children INDEX 1 INTO lo_data.
    lv_act = lo_data->id.
    cl_aunit_assert=>assert_equals( act = lv_act exp = 2 msg = 'Deserialization of simple recursive object fails' ).

  ENDMETHOD.                    "deserialize_object

  METHOD deserialize_partial.

    TYPES: BEGIN OF ts_record,
             id        TYPE string,
             m_columns TYPE json,
           END OF ts_record.

    DATA: lv_json TYPE string,
          lt_act  TYPE SORTED TABLE OF ts_record WITH UNIQUE KEY id,
          lt_exp  LIKE lt_act,
          ls_exp  LIKE LINE OF lt_exp.

    CONCATENATE '{"O000001ZZ_SO_GRES_CONTACTS":{"mColumns":{"AGE":{"bVisible":true,"iPosition":2},"BRSCH":{"bVisible":true}}},'
                '"O000001ZZ_TRANSIENT_TEST_A":{"mColumns":{"ABTNR":{"bVisible":false},"CITY1":{"bVisible":false},"IC_COMPANY_KEY":{"bVisible":true}}}}'
                INTO lv_json.

    deserialize( EXPORTING json = lv_json assoc_arrays = abap_true pretty_name = pretty_mode-camel_case CHANGING data = lt_act ).

    ls_exp-id = 'O000001ZZ_SO_GRES_CONTACTS'.
    ls_exp-m_columns = '{"AGE":{"bVisible":true,"iPosition":2},"BRSCH":{"bVisible":true}}'.
    INSERT ls_exp INTO TABLE lt_exp.

    ls_exp-id = 'O000001ZZ_TRANSIENT_TEST_A'.
    ls_exp-m_columns = '{"ABTNR":{"bVisible":false},"CITY1":{"bVisible":false},"IC_COMPANY_KEY":{"bVisible":true}}'.
    INSERT ls_exp INTO TABLE lt_exp.

    cl_aunit_assert=>assert_equals( act = lt_act exp = lt_exp msg = 'Partial deserialization fails' ).

  ENDMETHOD.                    "deserialize_partial

  METHOD serialize_partial.

    TYPES: BEGIN OF ts_record,
             id        TYPE string,
             m_columns TYPE json,
           END OF ts_record.

    DATA: lv_exp TYPE json,
          lv_act TYPE json,
          lt_act TYPE SORTED TABLE OF ts_record WITH UNIQUE KEY id,
          ls_act LIKE LINE OF lt_act.

    CONCATENATE '{"O000001ZZ_SO_GRES_CONTACTS":{"mColumns":{"AGE":{"bVisible":true,"iPosition":2},"BRSCH":{"bVisible":true}}},'
                '"O000001ZZ_TRANSIENT_TEST_A":{"mColumns":{"ABTNR":{"bVisible":false},"CITY1":{"bVisible":false},"IC_COMPANY_KEY":{"bVisible":true}}}}'
                INTO lv_exp.

    ls_act-id = 'O000001ZZ_SO_GRES_CONTACTS'.
    ls_act-m_columns = '{"AGE":{"bVisible":true,"iPosition":2},"BRSCH":{"bVisible":true}}'.
    INSERT ls_act INTO TABLE lt_act.

    ls_act-id = 'O000001ZZ_TRANSIENT_TEST_A'.
    ls_act-m_columns = '{"ABTNR":{"bVisible":false},"CITY1":{"bVisible":false},"IC_COMPANY_KEY":{"bVisible":true}}'.
    INSERT ls_act INTO TABLE lt_act.

    lv_act = serialize( data = lt_act assoc_arrays = abap_true pretty_name = pretty_mode-camel_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Partial serialization fails' ).

  ENDMETHOD.                    "serialize_partial

  METHOD custom_compressible.

    TYPES:
      BEGIN OF tp_s_data,
        tribool TYPE lc_json_custom=>tribool,
        bool    TYPE lc_json_custom=>bool,
        str     TYPE string,
        initial TYPE i,
        tmst    TYPE tzntstmpl,
      END OF tp_s_data.

    DATA: ls_data            TYPE tp_s_data,
          lo_json            TYPE t_json,
          lo_json_custom     TYPE REF TO lc_json_custom,
          lv_json            TYPE lc_json_custom=>json,
          lv_json_custom_exp LIKE lv_json,
          lv_json_custom     LIKE lv_json.

    ls_data-tribool = lc_json_custom=>c_tribool-false.
    ls_data-bool    = lc_json_custom=>c_bool-false.
    ls_data-str     = ''.
    ls_data-initial = 0.
    ls_data-tmst    = '20160708123456' ##LITERAL.

    CREATE OBJECT lo_json
      EXPORTING
        ts_as_iso8601 = abap_true
        compress      = abap_true.

    CREATE OBJECT lo_json_custom
      EXPORTING
        ts_as_iso8601 = abap_true
        compress      = abap_true.

    lv_json = lo_json->serialize_int( data = ls_data ).
    lv_json_custom = lo_json_custom->serialize_int( data = ls_data ).

    lv_json_custom_exp = `{"TRIBOOL":false,"STR":"","INITIAL":0,"TMST":"2016-07-08T12:34:56.0000000Z"}`.

    cl_aunit_assert=>assert_equals( act = lv_json_custom exp = lv_json_custom_exp msg = 'Custom compressable fails!' ).
    cl_aunit_assert=>assert_differs( act = lv_json exp = lv_json_custom msg = 'Custom compressable fails!' ).

  ENDMETHOD.                    "custom_compressible

  METHOD custom_pretty_name.
    TYPES:
      BEGIN OF tp_s_data,
        tribool TYPE lc_json_custom=>tribool,
        bool    TYPE lc_json_custom=>bool,
        str1    TYPE string,
        str2    TYPE string,
        initial TYPE i,
      END OF tp_s_data.

    DATA: ls_exp         TYPE tp_s_data,
          ls_act         LIKE ls_exp,
          lo_json_custom TYPE REF TO lc_json_custom,
          lv_json_custom TYPE lc_json_custom=>json.

    ls_exp-tribool = lc_json_custom=>c_tribool-false.
    ls_exp-bool    = lc_json_custom=>c_bool-false.
    ls_exp-str1     = ''.
    ls_exp-str2     = 'ABC'.
    ls_exp-initial  = 0.

    CREATE OBJECT lo_json_custom
      EXPORTING
        compress    = abap_true
        pretty_name = lc_json_custom=>pretty_mode-camel_case.

    lv_json_custom = lo_json_custom->serialize_int( data = ls_exp ).
    lo_json_custom->deserialize_int( EXPORTING json = lv_json_custom CHANGING data = ls_act ).

    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_exp msg = 'Custom pretty name fails!' ).

  ENDMETHOD.                    "custom_pretty_name

  METHOD custom_pretty_name2.

    TYPES:
      BEGIN OF tp_s_data,
        sschema             TYPE string,
        odatacontext        TYPE string,
        shortened_abap_name TYPE string,
        standard            TYPE string,
      END OF tp_s_data.

    DATA: ls_exp     TYPE tp_s_data,
          ls_act     LIKE ls_exp,
          lr_data    TYPE REF TO data,
          lv_string  TYPE string,
          lt_mapping TYPE name_mappings,
          ls_mapping LIKE LINE OF lt_mapping,
          lo_data    TYPE REF TO z_ui2_data_access,
          lo_json    TYPE t_json,
          lv_json    TYPE lc_json_custom=>json.

    ls_exp-sschema              = `abc1`.
    ls_exp-odatacontext         = `abc2`.
    ls_exp-shortened_abap_name  = `abc3`.
    ls_exp-standard             = `abc4`.

    " pre-fill name mapping table
    ls_mapping-abap = `SSCHEMA`.
    ls_mapping-json = `$schema`.
    INSERT ls_mapping INTO TABLE lt_mapping.

    ls_mapping-abap = `ODATACONTEXT`.
    ls_mapping-json = `@odata.context`.
    INSERT ls_mapping INTO TABLE lt_mapping.

    ls_mapping-abap = `SHORTENED_ABAP_NAME`.
    ls_mapping-json = `VeeeeryyyyyLooooongJSONAttrbuuuuuuuuuteeeeeeeeeee`.
    INSERT ls_mapping INTO TABLE lt_mapping.

*    lt_mapping = VALUE #(
*      ( in = `SSCHEMA` out = `$schema` )
*      ( in = `ODATACONTEXT` out = `@odata.context` )
*      ( in = `SHORTENED_ABAP_NAME` out = `VeeeeryyyyyLooooongJSONAttrbuuuuuuuuuteeeeeeeeeee` )
*    ).

    CREATE OBJECT lo_json
      EXPORTING
        pretty_name      = pretty_mode-low_case
        name_mappings    = lt_mapping
        assoc_arrays     = abap_true
        assoc_arrays_opt = abap_true.

    lv_json = lo_json->serialize_int( data = ls_exp ).
    lo_json->deserialize_int( EXPORTING json = lv_json CHANGING data = ls_act ).

    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_exp msg = 'Custom pretty name fails!' ).

    " test generation with custom name mappings
    lo_json->generate_int( EXPORTING json = lv_json CHANGING data = lr_data ).

    lo_data = z_ui2_data_access=>create( ir_data = lr_data iv_component = 'shortened_abap_name' ).
    lo_data->value( IMPORTING ev_data = lv_string ).

    cl_aunit_assert=>assert_equals( act = lv_string exp = `abc3` msg = 'Generation of OData structure with name mapping fails!' ).

  ENDMETHOD.                    "custom_pretty_name

  METHOD extended_pretty_name.

    TYPES:
      BEGIN OF tp_s_data,
        __d__schema          TYPE string,
        __a__odata___context TYPE string,
        __e____n____p____m__ TYPE string,
        __s____h____t____l__ TYPE string,
        __c____v____o__      TYPE string,
        _abap_name           TYPE string,
      END OF tp_s_data.

    DATA: ls_exp  TYPE tp_s_data,
          ls_act  LIKE ls_exp,
          lv_exp  TYPE string,
          lv_json TYPE lc_json_custom=>json.

    lv_exp = `{"$schema":"","@odata.context":"","!#%&":"","*-~/":"",":|.":"","AbapName":""}`.

    lv_json = serialize( data = ls_exp pretty_name = pretty_mode-extended ).

    cl_aunit_assert=>assert_equals( act = lv_json exp = lv_exp msg = 'Extended pretty name fails!' ).

    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-extended CHANGING data = ls_act ).

    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_exp msg = 'Extended pretty name fails!' ).

  ENDMETHOD.                    "custom_compressible

  METHOD deserialize_alias_type.

    TYPES:
      BEGIN OF lty_data,
        obj_type TYPE string,
        obj_name TYPE string,
      END OF lty_data,
      BEGIN OF lty_source_object,
        path          TYPE string,
        source        TYPE string,
        source_length TYPE i,
      END OF lty_source_object,
      BEGIN OF lty_object,
        id   TYPE string.
        INCLUDE TYPE lty_data.
        INCLUDE TYPE lty_source_object AS source_object.
    TYPES: test TYPE abap_bool.
    TYPES: END OF lty_object .

    DATA: ls_act  TYPE lty_object,
          ls_exp  LIKE ls_act,
          lv_json TYPE json.

    ls_exp-id             = '21321546'.
    ls_exp-obj_type       = 'ABC'.
    ls_exp-obj_name       = 'XXX'.
    ls_exp-path           = '/path/to/'.
    ls_exp-source         = 'hell.js'.
    ls_exp-source_length  = 256.
    ls_exp-test           = abap_true.

    lv_json = '{"id":"21321546","objType":"ABC","objName":"XXX","sourceObject":{"path":"/path/to/","source":"hell.js","sourceLength":256},"test":true}'.

    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case
                               CHANGING data = ls_act ).

    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_exp msg = 'Deserialisation with alias fails!' ).

  ENDMETHOD.                    "deserialize_alias_type

  METHOD name_value_map.

    TYPES: BEGIN OF ts_record,
             key   TYPE string,
             value TYPE string,
           END OF ts_record,
           BEGIN OF ts_record2,
             key   TYPE string,
             value TYPE ts_record,
           END OF ts_record2.

    DATA: lv_exp  TYPE json,
          lv_act  TYPE json,
          lt_act  TYPE SORTED TABLE OF ts_record WITH UNIQUE KEY key,
          lt_act2 TYPE SORTED TABLE OF ts_record2 WITH UNIQUE KEY key,
          lt_exp  LIKE lt_act,
          lt_exp2 LIKE  lt_act2,
          ls_exp  LIKE LINE OF lt_act2.

    ls_exp-key         = 'KEY1'.
    ls_exp-value-key   = ls_exp-key.
    ls_exp-value-value = 'VALUE1'.
    INSERT ls_exp-value INTO TABLE lt_exp.
    INSERT ls_exp INTO TABLE lt_exp2.

    ls_exp-key         = 'KEY2'.
    ls_exp-value-key   = ls_exp-key.
    ls_exp-value-value = 'VALUE2'.
    INSERT ls_exp-value INTO TABLE lt_exp.
    INSERT ls_exp INTO TABLE lt_exp2.

    lv_exp = '{"KEY1":"VALUE1","KEY2":"VALUE2"}'.
    lv_act = serialize( data = lt_exp assoc_arrays = abap_true assoc_arrays_opt = abap_true ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Name/Value map serialization fails!' ).

    deserialize( EXPORTING json = lv_act assoc_arrays = abap_true assoc_arrays_opt = abap_true
                               CHANGING  data = lt_act ).
    cl_aunit_assert=>assert_equals( act = lt_act exp = lt_exp msg = 'Name/Value map deserialization fails!' ).

    lv_exp = '{"KEY1":{"KEY":"KEY1","VALUE":"VALUE1"},"KEY2":{"KEY":"KEY2","VALUE":"VALUE2"}}'.
    lv_act = serialize( data = lt_exp2 assoc_arrays = abap_true assoc_arrays_opt = abap_true ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Name/Value map serialization fails!' ).

    deserialize( EXPORTING json = lv_act assoc_arrays = abap_true assoc_arrays_opt = abap_true
                               CHANGING  data = lt_exp2 ).
    cl_aunit_assert=>assert_equals( act = lt_exp2 exp = lt_exp2 msg = 'Name/Value map deserialization fails!' ).

  ENDMETHOD.                    "name_value_map

  METHOD dynamic_types.

    TYPES:
      BEGIN OF ts_json_meta,
        abap_type LIKE cl_abap_typedescr=>absolute_name,
        data      TYPE json,
      END OF ts_json_meta.

    DATA: lt_flight    TYPE STANDARD TABLE OF sflight WITH DEFAULT KEY,
          lv_json      TYPE json,
          lo_typedescr TYPE REF TO cl_abap_typedescr,
          lo_data      TYPE REF TO data,
          ls_exp       TYPE ts_json_meta,
          ls_act       LIKE ls_exp.

    FIELD-SYMBOLS: <data> TYPE any.

    SELECT * FROM sflight INTO TABLE lt_flight UP TO 1000 ROWS ORDER BY PRIMARY KEY. "#EC CI_NOWHERE

    " serialize table lt_flight into JSON, skipping initial fields and converting ABAP field names into camelCase
    ls_exp-data      = serialize( data = lt_flight compress = abap_true pretty_name = pretty_mode-camel_case ).
    lo_typedescr     = cl_abap_typedescr=>describe_by_data( lt_flight ).
    ls_exp-abap_type = lo_typedescr->absolute_name.
    lv_json          = serialize( data = ls_exp compress = abap_true pretty_name = pretty_mode-camel_case ).

    " deserialize JSON string json into internal table lt_flight doing camelCase to ABAP like field name mapping
    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case CHANGING data = ls_act ).

    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_exp msg = 'Dynamic serialization/deserialization fails!' ).

    CREATE DATA lo_data TYPE (ls_act-abap_type).
    ASSIGN lo_data->* TO <data>.
    deserialize( EXPORTING json = ls_act-data pretty_name = pretty_mode-camel_case CHANGING data = <data> ).

    cl_aunit_assert=>assert_equals( act = <data> exp = lt_flight msg = 'Dynamic serialization/deserialization fails!' ).

  ENDMETHOD.                    "dynamic_types

  METHOD deserialize_malformed_type.

    TYPES:
      BEGIN OF ts_record,
        attribute             TYPE c LENGTH 45,
        description           TYPE c LENGTH 60,
        datasource            TYPE string,
        datasourcedescription TYPE c LENGTH 60,
        groupname             TYPE string,
        groupdescription      TYPE c LENGTH 60,
      END OF ts_record.

    DATA: lv_json       TYPE json,
          lt_attributes TYPE STANDARD TABLE OF ts_record,
          lv_lines      TYPE i.

    CONCATENATE `["BO-CUAN_INTERACTION_CONTACT","BO-CUAN_INTERACTION_CONTACT/IC_TEAM_MEMBER/SEARCH/AEXE",`
                `"BO-CUAN_INTERACTION_CONTACT/IC_TEAM_MEMBER/SEARCH/MNGR","BO-CUAN_INTERACTION_CONTACT/IC_TEAM_MEMBER/SEARCH/PSAL",`
                `"BO-CUAN_INTERACTION_CONTACT/IC_TEAM_MEMBER/SEARCH/QUAL","BO-CUAN_INTERACTION_CONTACT/IC_TEAM_MEMBER/SEARCH/SUPP"]`
    INTO lv_json.

    deserialize( EXPORTING json         = lv_json
                                         pretty_name  = pretty_mode-camel_case
                               CHANGING  data         = lt_attributes  ).

    lv_lines = lines( lt_attributes ).

    " because type of record in JSON does not fit to type of record in ABAP we expect to get table with 6 empty rows
    cl_aunit_assert=>assert_equals( act = lv_lines exp = 6 msg = 'Deserialization of malformed type table fails!' ).

    " repeat the same in strict mode
    DATA: lo_json TYPE t_json.

    CREATE OBJECT lo_json
      EXPORTING
        strict_mode = abap_true
        pretty_name = pretty_mode-camel_case
        compress    = abap_true.

    CLEAR: lt_attributes.
    TRY .
        lo_json->deserialize_int( EXPORTING json = lv_json CHANGING data = lt_attributes ).
        cl_aunit_assert=>fail( msg = 'Test of strict mode with mailformed data fails!' ).
      CATCH cx_sy_move_cast_error.
        lv_lines = lines( lt_attributes ).
        cl_aunit_assert=>assert_equals( act = lv_lines exp = 0 msg = 'Deserialization of malformed type table fails!' ).
    ENDTRY.

    DATA: BEGIN OF d1,
            abc TYPE i,
            def TYPE string,
            ghi TYPE string_table,
          END OF d1.

    lv_json = `{"abc": 25, "def": [ "a", "b", "c"], "ghi" : "" }`.
    TRY .
        lo_json->deserialize_int( EXPORTING json = lv_json CHANGING data = d1 ).
        cl_aunit_assert=>fail( msg = 'Test of strict mode with mailformed data fails!' ).
      CATCH cx_sy_move_cast_error.
        cl_aunit_assert=>assert_initial( act = d1 msg = 'Deserialization of malformed type table fails!' ).
    ENDTRY.

  ENDMETHOD.                    "deserialize_malformed_type

  METHOD serialize_dynamic_type.

    DATA:
      ls_comp_descr   TYPE abap_componentdescr,
      lt_comp_descr   TYPE abap_component_tab,
      lo_table_descr  TYPE REF TO cl_abap_tabledescr,
      lo_struct_descr TYPE REF TO cl_abap_structdescr,
      lr_table        TYPE REF TO data,
      lr_line         TYPE REF TO data,
      test_data       TYPE REF TO data,
      lv_act          TYPE json,
      lv_exp          TYPE json.

    FIELD-SYMBOLS: <table> TYPE ANY TABLE,
                   <line>  TYPE any,
                   <field> TYPE any.

    ls_comp_descr-name = `FIELD1`.
    ls_comp_descr-type = cl_abap_elemdescr=>get_string( ).
    INSERT ls_comp_descr INTO TABLE lt_comp_descr.

    ls_comp_descr-name = `FIELD2`.
    ls_comp_descr-type = cl_abap_elemdescr=>get_c( p_length = 30 ).
    INSERT ls_comp_descr INTO TABLE lt_comp_descr.

    ls_comp_descr-name = `FIELD3`.
    ls_comp_descr-type = cl_abap_elemdescr=>get_i( ).
    INSERT ls_comp_descr INTO TABLE lt_comp_descr.

    ls_comp_descr-name = `FIELD4`.
    CREATE DATA test_data TYPE c LENGTH 30.
    ls_comp_descr-type ?= cl_abap_datadescr=>describe_by_data_ref( p_data_ref = test_data ).
    INSERT ls_comp_descr INTO TABLE lt_comp_descr.

    lo_struct_descr = cl_abap_structdescr=>create( p_components = lt_comp_descr ).
    lo_table_descr  = cl_abap_tabledescr=>create( p_line_type = lo_struct_descr ).

    CREATE DATA lr_table TYPE HANDLE lo_table_descr.
    ASSIGN lr_table->* TO <table>.

    CREATE DATA lr_line LIKE LINE OF <table>.
    ASSIGN lr_line->* TO <line>.

    ASSIGN COMPONENT `FIELD1` OF STRUCTURE <line> TO <field>.
    <field> = 'Hello World!'.

    ASSIGN COMPONENT `FIELD2` OF STRUCTURE <line> TO <field>.
    <field> = 'Eat me!'.

    ASSIGN COMPONENT `FIELD3` OF STRUCTURE <line> TO <field>.
    <field> = 20.

    ASSIGN COMPONENT `FIELD4` OF STRUCTURE <line> TO <field>.
    <field> = 'I am BIG!'.

    INSERT <line> INTO TABLE <table>.                      "#EC DUPL_OK
    INSERT <line> INTO TABLE <table>.

    lv_exp = '[{"field1":"Hello World!","field2":"Eat me!","field3":20,"field4":"I am BIG!"},{"field1":"Hello World!","field2":"Eat me!","field3":20,"field4":"I am BIG!"}]'.
    lv_act = serialize( data = lr_table pretty_name = pretty_mode-camel_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of dynamic type fails!' ).

  ENDMETHOD.                    "serialize_dynamic_type

  METHOD deserialze_to_read_only.

    DATA: lt_flight TYPE STANDARD TABLE OF sflight WITH DEFAULT KEY,
          lv_json   TYPE json,
          lo_exp    TYPE REF TO cl_abap_typedescr.

    lo_exp = cl_abap_typedescr=>describe_by_data( lt_flight ).
    lv_json = serialize( lo_exp ).
    deserialize( EXPORTING json = lv_json CHANGING data = lo_exp ).

  ENDMETHOD.                    "deserialze_to_read_only

  METHOD generate_simple.

    DATA: lv_json   TYPE json,
          lv_bool   TYPE abap_bool,
          lv_string TYPE string,
          lv_int    TYPE i,
          lv_float  TYPE f,
          lv_p      TYPE p,
          lv_date   TYPE d,
          lv_time   TYPE t,
          lv_ts     TYPE timestamp,
          lv_act    TYPE json,
          lo_data   TYPE REF TO z_ui2_data_access,
          lr_val    TYPE REF TO data,
          lr_act    TYPE REF TO data.

    CONCATENATE
      `{"code": "2000","message": "Resource CRUD success","output": {"$schema":`
      ` "http://json-schema.org/draft-04/schema#","title": "Rule Service","id":`
      ` "#root","description": "Rule service schema","required": ["vocabulary",`
      ` "executionContext"],"additionalProperties": true,"properties": {"descr`
      `iption": {"description": "Rule service description","type": "string","ma`
      `xLength": 256},"resultView": {"description": "Indicates if result view s`
      `hould be created","type": "string","enum": ["withResultView", "resultVie`
      `wOnly"]},"executionContext": {"description": "Service execution details"`
      `,"id": "#executionContext","type": "object","additionalProperties": fals`
      `e,"properties": {"parameters": {"description": "Input parameters;","type`
      `": "object","required": ["definition"],"additionalProperties": false,"pr`
      `operties": {"definition": {"description": "parameters definition","type"`
      `: "array","items": {"type": "object","oneOf": [{"$ref": "#basicParameter`
      `FirstLevel"}, {"$ref": "#structureParameterFirstLevel"}, {"$ref": "#data`
      `ObjectParameter"}]},"uniqueItems": true},}}},"executionContextDefinition`
      `s": {"businessDataType": {"id": "#businessDataType","description": "Mode`
      `l data type","type": "string","enum": ["String", "Number", "Timestamp", `
      `"Boolean", "TimeSpan", "Date", "Time"]},}},"conversion_Flags_Map": {"type"`
      `: "object","additionalProperties": false,"properties": {"is-Value.ListConv`
      `erted": {"type": "boolean","enum": [true]}}}},},"details": []}`
    INTO lv_json.

    lr_act = generate( json = lv_json pretty_name = pretty_mode-camel_case ).
    cl_aunit_assert=>assert_not_initial( act = lr_act msg = 'Generation of ABAP object fails!' ).

    lo_data = z_ui2_data_access=>create( ir_data = lr_act iv_component = 'output-additional_properties' ).
    lo_data->value( IMPORTING ev_data = lv_bool ).

    cl_aunit_assert=>assert_equals( act = lv_bool exp = abap_true msg = 'Generation of boolean for ABAP object fails!' ).

    lo_data = z_ui2_data_access=>create( ir_data = lr_act iv_component = 'output-properties-conversion_flags_map-properties-is_value_list_converted' ).
    lr_val = lo_data->ref( ).

    cl_aunit_assert=>assert_bound( act = lr_val msg = 'Generation of deep structure with different pretty name modes fails!' ).

    lo_data = z_ui2_data_access=>create( ir_data = lr_act iv_component = 'output-required[1]' ).
    lo_data->value( IMPORTING ev_data = lv_string ).

    cl_aunit_assert=>assert_equals( act = lv_string exp = 'vocabulary' msg = 'Generation of table fails!' ).

    DATA:
      ls_comp_descr TYPE abap_componentdescr,
      lt_comp_descr TYPE abap_component_tab.

    ls_comp_descr-name = `FIELD1`.
    ls_comp_descr-suffix = `ABC`.
    ls_comp_descr-type = cl_abap_elemdescr=>get_string( ).
    INSERT ls_comp_descr INTO TABLE lt_comp_descr.

    ls_comp_descr-name = `FIELD2`.
    ls_comp_descr-type = cl_abap_elemdescr=>get_c( p_length = 30 ).
    INSERT ls_comp_descr INTO TABLE lt_comp_descr.

    lo_data = z_ui2_data_access=>create( iv_data = lt_comp_descr iv_component = '[2]-name' ).
    lo_data->value( IMPORTING ev_data = lv_string ).

    cl_aunit_assert=>assert_equals( act = lv_string exp = 'FIELD2' msg = 'Dynamic access fails!' ).

    lo_data = z_ui2_data_access=>create( iv_data = lt_comp_descr iv_component = '[name=FIELD1]-suffix' ).
    lo_data->value( IMPORTING ev_data = lv_string ).

    cl_aunit_assert=>assert_equals( act = lv_string exp = 'ABC' msg = 'Dynamic access fails!' ).

    lv_json = `{"CODE": "2000", "code": "3000"}`.

    lr_act = generate( json = lv_json ).
    cl_aunit_assert=>assert_not_initial( act = lr_act msg = 'Generation of ABAP object from JSON with duplicate attributes fails!' ).

    lo_data = z_ui2_data_access=>create( iv_data = lr_act iv_component = 'CODE' ).
    lo_data->value( IMPORTING ev_data = lv_string ).

    cl_aunit_assert=>assert_equals( act = lv_string exp = '2000' msg = 'Generation of ABAP object from JSON with duplicate attributes fails!' ).

    lv_json = `{"OrderLinePriceOverrideHistory": "2000"}`.

    lr_act = generate( json = lv_json pretty_name = pretty_mode-camel_case ).
    cl_aunit_assert=>assert_not_initial( act = lr_act msg = 'Generation of ABAP object from JSON with long attributes fails!' ).

    lo_data = z_ui2_data_access=>create( iv_data = lr_act iv_component = 'ORDER_LINE_PRICE_OVERRIDE_HIST' ).
    lo_data->value( IMPORTING ev_data = lv_string ).

    cl_aunit_assert=>assert_equals( act = lv_string exp = '2000' msg = 'Generation of ABAP object from JSON with long attributes fails!' ).

    lv_json = `{"exponential":1E+3,"bool":true,"packed":12345678910,"int":123456789,"date":"2024-10-03","time":"23:34:12","ts":"2015-10-02T13:44:50.5545900Z"}`.

    lr_act = generate( json = lv_json ).
    cl_aunit_assert=>assert_not_initial( act = lr_act msg = 'Type conversion during generation object from JSON fails!' ).

    lo_data = z_ui2_data_access=>create( iv_data = lr_act iv_component = 'EXPONENTIAL' ).
    lo_data->value( IMPORTING ev_data = lv_float ).

    cl_aunit_assert=>assert_equals( act = lv_float exp = '1000' msg = 'Generation of exponential value fails!' ).

    lo_data = z_ui2_data_access=>create( iv_data = lr_act iv_component = 'BOOL' ).
    lo_data->value( IMPORTING ev_data = lv_bool ).

    cl_aunit_assert=>assert_equals( act = lv_bool exp = abap_true msg = 'Generation of boolean value fails!' ).

    lo_data = z_ui2_data_access=>create( iv_data = lr_act iv_component = 'PACKED' ).
    lo_data->value( IMPORTING ev_data = lv_p ).

    cl_aunit_assert=>assert_equals( act = lv_p exp = 12345678910 msg = 'Generation of packed value fails!' ).

    lo_data = z_ui2_data_access=>create( iv_data = lr_act iv_component = 'INT' ).
    lo_data->value( IMPORTING ev_data = lv_int ).

    cl_aunit_assert=>assert_equals( act = lv_int exp = 123456789 msg = 'Generation of integer value fails!' ).

    lo_data = z_ui2_data_access=>create( iv_data = lr_act iv_component = 'DATE' ).
    lo_data->value( IMPORTING ev_data = lv_date ).

    cl_aunit_assert=>assert_equals( act = lv_date exp = '20241003' msg = 'Generation of date value fails!' ).

    lo_data = z_ui2_data_access=>create( iv_data = lr_act iv_component = 'TIME' ).
    lo_data->value( IMPORTING ev_data = lv_time ).

    cl_aunit_assert=>assert_equals( act = lv_time exp = '233412' msg = 'Generation of time value fails!' ).

    lo_data = z_ui2_data_access=>create( iv_data = lr_act iv_component = 'TS' ).
    lo_data->value( IMPORTING ev_data = lv_ts ).

    cl_aunit_assert=>assert_equals( act = lv_ts exp = '20151002134451' msg = 'Generation of timestamp value fails!' ).

    lv_json = 'null'.
    lr_act  = generate( json = lv_json pretty_name = pretty_mode-user_low_case ).
    lv_act  = serialize( data = lr_act compress = abap_true numc_as_string = abap_false pretty_name = pretty_mode-user_low_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_json msg = 'Generation of null value fails!' ).

    lv_json = '5'.
    lr_act  = generate( json = lv_json pretty_name = pretty_mode-user_low_case ).
    lv_act  = serialize( data = lr_act compress = abap_true numc_as_string = abap_false pretty_name = pretty_mode-user_low_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_json msg = 'Generation of interger value fails!' ).

    lv_json = 'false'.
    lr_act  = generate( json = lv_json pretty_name = pretty_mode-user_low_case ).
    lv_act  = serialize( data = lr_act compress = abap_true numc_as_string = abap_false pretty_name = pretty_mode-user_low_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_json msg = 'Generation of boolean value fails!' ).

    lv_json = '"test"'.
    lr_act  = generate( json = lv_json pretty_name = pretty_mode-user_low_case ).
    lv_act  = serialize( data = lr_act compress = abap_true numc_as_string = abap_false pretty_name = pretty_mode-user_low_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_json msg = 'Generation of string value fails!' ).

    lv_json = `{"totalDirectCostAmount":100000000976457,"totalOverheadFeesCharges":987698798964543567658786745}`.
    lr_act = generate( json = lv_json pretty_name = pretty_mode-camel_case ).
    cl_aunit_assert=>assert_not_initial( act = lr_act msg = 'Generation of huge ints fails!' ).

    lv_act  = serialize( data = lr_act compress = abap_true pretty_name = pretty_mode-camel_case ).
    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_json msg = 'Generation of huge ints fails!' ).

    DATA:
      BEGIN OF target_struct,
        bool_ref   TYPE REF TO abap_bool,
        string_ref TYPE REF TO string,
        text_ref   TYPE REF TO char255,
        int_ref    TYPE REF TO i,
        data_ref   TYPE REF TO data,
      END OF target_struct.

    lv_json = '{"bool_ref":true,"string_ref":"true","text_ref":"false","int_ref":5,"data_ref":10}'.
    deserialize( EXPORTING json = lv_json CHANGING data = target_struct ).
    lv_act  = serialize( data = target_struct pretty_name = pretty_mode-low_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_json msg = 'Generation of references to simple types fails!' ).

    lv_json = '{"bool_ref":null,"string_ref":null,"text_ref":null,"int_ref":null,"data_ref":null}'.
    deserialize( EXPORTING json = lv_json CHANGING data = target_struct ).
    lv_act  = serialize( data = target_struct pretty_name = pretty_mode-low_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_json msg = 'Generation of null references to simple types fails!' ).

  ENDMETHOD.                    "generate_simple

  METHOD generate_for_odata.

    DATA: lv_json TYPE json,
          lv_etag TYPE string,
          lv_date TYPE p,
          lo_data TYPE REF TO z_ui2_data_access,
          lr_act  TYPE REF TO data.

    CONCATENATE
      `{"d":{"__metadata":{"uri":"http://localhost/sap/opu/odata/SAP/API_RECIPE/A_Recipe(guid'42f2e9af-c4ef-1ed8-93db-a01697362280')",`
      `"type":"API_RECIPE.A_RecipeType","etag":"W/\"datetimeoffset'2018-05-03T14%3A19%3A49Z'\""},"RecipeUUID":"42f2e9af-c4ef-1ed8-93db-a01697362280",`
      `"RecipeValidityStartDate": null,"RecipeLastChangeDateTime": "\/Date(1525357189000+0000)\/","RecipeIsDeleted": false,"beginDate":1520781590000,`
      `"to_Characteristics":{"__deferred":{"uri":"http://localhost/sap/opu/odata/SAP/API_RECIPE/A_Recipe(guid'42f2e9af-c4ef-1ed8-93db-a01697362280')/to_Characteristics"}}}}`
    INTO lv_json.

    lr_act = generate( json = lv_json pretty_name = pretty_mode-camel_case ).
    cl_aunit_assert=>assert_not_initial( act = lr_act msg = 'Generation of OData ABAP object fails!' ).

    lo_data = z_ui2_data_access=>create( ir_data = lr_act iv_component = 'd-__metadata-etag' ).
    lo_data->value( IMPORTING ev_data = lv_etag ).

    cl_aunit_assert=>assert_equals( act = lv_etag exp = `W/"datetimeoffset'2018-05-03T14%3A19%3A49Z'"` msg = 'Generation of OData structure fails!' ).

    lo_data = z_ui2_data_access=>create( ir_data = lr_act iv_component = 'd-begin_date' ).
    lo_data->value( IMPORTING ev_data = lv_date ).

    cl_aunit_assert=>assert_equals( act = lv_date exp = `1520781590000` msg = 'Generation of OData structure with long int fails!' ).

  ENDMETHOD.                    "generate

  METHOD deserialize_odata.

    DATA: lv_json TYPE json.

    DATA:
      BEGIN OF ls_odata_response,
        BEGIN OF d,
          BEGIN OF ____metadata,
            id   TYPE string,
            uri  TYPE string,
            type TYPE string,
          END OF ____metadata,
          id                     TYPE string,
          category               TYPE c LENGTH 1,
          validity               TYPE i,
          client_expiration_time TYPE timestamp,
          component              TYPE string,
          app_name               TYPE string,
          BEGIN OF pers_container_items,
            results TYPE STANDARD TABLE OF string,
          END OF pers_container_items,
        END OF d,
      END OF ls_odata_response.

    CONCATENATE
    `{"d":{"__metadata":{"id":"https://abc.com:1304/sap/opu/odata/UI2/INTEROP/PersContainers(id='sap.ushell.UserDefaultParameter',category='P')",`
    `"uri":"https://abc.com:1304/sap/opu/odata/UI2/INTEROP/PersContainers(id='sap.ushell.UserDefaultParameter',category='P')","type":"INTEROP.PersContainer"},`
    `"id":"sap.ushell.UserDefaultParameter","category":"P","validity":0,"clientExpirationTime":null,"component":"","appName":"","PersContainerItems":{"results":[]}}}`
    INTO lv_json.

    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case CHANGING data = ls_odata_response ).
    cl_aunit_assert=>assert_not_initial( act = ls_odata_response msg = 'Parsing of OData response fails!' ).

    TYPES:
      BEGIN OF ts_result,
        BEGIN OF ____metadata,
          id   TYPE string,
          uri  TYPE string,
          type TYPE string,
          etag TYPE string,
        END OF ____metadata,
        case_guid           TYPE guid,
        external_key        TYPE string,
        contact_person_name TYPE string,
      END OF ts_result.

    DATA:
      BEGIN OF ls_odata_response2,
        BEGIN OF d,
          results TYPE STANDARD TABLE OF ts_result WITH DEFAULT KEY,
        END OF d,
      END OF ls_odata_response2.

    CONCATENATE
    `{"d":{"results":[{"__metadata":{"id":"https://abc.com:44300/sap/opu/odata/sap/UDMO_MANAGE_DISPUTES_SRV/DisputeSet('.1~3863BB44F0201EE69AD5A3331AE45366')",`
    `"uri":"https://abc.com:44300/sap/opu/odata/sap/UDMO_MANAGE_DISPUTES_SRV/DisputeSet('.1~3863BB44F0201EE69AD5A3331AE45366')","type":"UDMO_MANAGE_DISPUTES_SRV.Dispute",`
    `"etag":"W/\"datetime'1999-12-14T10%3A18%3A46'\""},"CaseGuid":"0894ef45-77a9-1ed8-a495-bd69397619c0","ExternalKey":"1452","ContactPersonName":""}]}}`
    INTO lv_json.

    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case CHANGING data = ls_odata_response2 ).
    cl_aunit_assert=>assert_not_initial( act = ls_odata_response2 msg = 'Parsing of OData response fails!' ).

    " test deserialization of GUID
    CONSTANTS: lc_guid TYPE guid VALUE `0894EF4577A91ED8A495BD69397619C0`.
    DATA: ls_result LIKE LINE OF ls_odata_response2-d-results.

    READ TABLE ls_odata_response2-d-results INDEX 1 INTO ls_result.

    cl_aunit_assert=>assert_equals( act = ls_result-case_guid exp = lc_guid msg = 'Deserilaization of Edm.Guid fails!' ).


  ENDMETHOD.                    "deserialize_odata

  METHOD initialize_on_deserialize.

    DATA:
      BEGIN OF ls_data,
        int       TYPE i VALUE 1,
        num       TYPE n LENGTH 6 VALUE '000001',
        timestamp TYPE timestamp,
        boolean   TYPE abap_bool VALUE abap_true,
        str       TYPE string VALUE 'VALUE',
        items     TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      END OF ls_data.

    GET TIME STAMP FIELD ls_data-timestamp.
    APPEND 'ITEM' TO ls_data-items.

    DATA: lv_json TYPE string.

    lv_json = `{"int":0,"num":0,"timestamp":null,"boolean":false,"str":"","items":[]}`.
    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case CHANGING data = ls_data ).
    cl_aunit_assert=>assert_initial( act = ls_data msg = 'Initialize of elements on deserialize fails!' ).

  ENDMETHOD.                    "initialize_on_deserialize

  """""""""""""""""""""""""""""""""""""""""""""""""""
  " TODO:
  "  - deserilaize into field with REF TO data type, if field is bound, using refrenced data type
  "  - automatic generation of the data, if field has REF TO data type and bound data is initial
  METHOD deserialize_ref_to_data.

    DATA:
      BEGIN OF ls_data,
        str    TYPE string,
        table  TYPE REF TO data,
        struct TYPE REF TO data,
      END OF ls_data.

    DATA:
      ls_comp_descr   TYPE abap_componentdescr,
      lt_comp_descr   TYPE abap_component_tab,
      lo_table_descr  TYPE REF TO cl_abap_tabledescr,
      lo_struct_descr TYPE REF TO cl_abap_structdescr,
      lo_data         TYPE REF TO z_ui2_data_access,
      lv_json         TYPE json,
      lv_value        TYPE string,
      lv_lines        TYPE i.

    FIELD-SYMBOLS: <table>  TYPE ANY TABLE,
                   <struct> TYPE any,
                   <field>  TYPE any.

    " Test deserialize on REF TO DATA of known type

    ls_comp_descr-name = `FIELD1`.
    ls_comp_descr-type = cl_abap_elemdescr=>get_string( ).
    INSERT ls_comp_descr INTO TABLE lt_comp_descr.

    lo_struct_descr = cl_abap_structdescr=>create( p_components = lt_comp_descr ).
    lo_table_descr  = cl_abap_tabledescr=>create( p_line_type = lo_struct_descr ).

    CREATE DATA ls_data-table TYPE HANDLE lo_table_descr.

    lv_json = `{"str":"","table":[{"field1":"value1"},{"field1":"value2"}]}`.
    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case CHANGING data = ls_data ).

    cl_aunit_assert=>assert_bound( act = ls_data-table msg = 'Deserialize to known REF TO data for table fails!' ).

    ASSIGN ls_data-table->* TO <table>.
    cl_aunit_assert=>assert_subrc( act = sy-subrc msg = 'Deserialize to known REF TO data fails!' ).

    lv_lines = lines( <table> ).
    cl_aunit_assert=>assert_equals( act = lv_lines exp = 2 msg = 'Deserialize to known REF TO data for table fails!' ).

    " Test implicit generate on tables
    CLEAR ls_data.

    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case CHANGING data = ls_data ).

    cl_aunit_assert=>assert_bound( act = ls_data-table msg = 'Deserialize to unknown REF TO data for table fails!' ).

    ASSIGN ls_data-table->* TO <table>.
    cl_aunit_assert=>assert_subrc( act = sy-subrc msg = 'Deserialize to unknown REF TO data for table fails!' ).

    lv_lines = lines( <table> ).
    cl_aunit_assert=>assert_equals( act = lv_lines exp = 2 msg = 'Deserialize to unknown REF TO data for table fails!' ).

    " Test deserialize of REF TO DATA of known type for structure
    lv_json = `{"str":"","struct":{"field1":"value1"}}`.

    CREATE DATA ls_data-struct TYPE HANDLE lo_struct_descr.

    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case CHANGING data = ls_data ).
    cl_aunit_assert=>assert_bound( act = ls_data-struct msg = 'Deserialize to unknown REF TO data for struct fails!' ).

    ASSIGN ls_data-struct->* TO <struct>.
    cl_aunit_assert=>assert_subrc( act = sy-subrc msg = 'Deserialize to unknown REF TO data for struct fails!' ).

    ASSIGN COMPONENT `FIELD1` OF STRUCTURE <struct> TO <field>.
    cl_aunit_assert=>assert_subrc( act = sy-subrc msg = 'Deserialize to unknown REF TO data for struct fails!' ).
    cl_aunit_assert=>assert_equals( act = <field> exp = 'value1' msg = 'Deserialize to unknown REF TO data for struct fails!' ).

    " Test implicit generate on structures
    CLEAR ls_data.

    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case assoc_arrays = abap_true assoc_arrays_opt = abap_true CHANGING data = ls_data ).
    cl_aunit_assert=>assert_bound( act = ls_data-struct msg = 'Deserialize to unknown REF TO data for struct fails!' ).

    lo_data = z_ui2_data_access=>create( iv_data = ls_data-struct iv_component = 'field1' ).
    lo_data->value( IMPORTING ev_data = lv_value ).
    cl_aunit_assert=>assert_equals( act = lv_value exp = 'value1' msg = 'Deserialize to unknown REF TO data for struct fails!' ).

    TYPES: BEGIN OF struct,
             field TYPE i,
           END OF struct,
           tab TYPE STANDARD TABLE OF struct WITH DEFAULT KEY.
    DATA: BEGIN OF target_tab,
            tab      TYPE REF TO tab,
            abstract TYPE REF TO data,
            invalid  TYPE REF TO i,
          END OF target_tab.

    lv_json = `{"tab":[{"field":1}],"abstract":[{"field":1}],"invalid":[{"field":1}]}`.
    deserialize( EXPORTING json = lv_json CHANGING  data = target_tab ).
    cl_aunit_assert=>assert_not_initial( act = target_tab-tab msg = 'Generation of defined table fails!' ).
    cl_aunit_assert=>assert_not_initial( act = target_tab-abstract msg = 'Generation of abstract table fails!' ).
    cl_aunit_assert=>assert_initial( act = target_tab-invalid msg = 'Generation of invalid type into table fails!' ).

    DATA: BEGIN OF target_struct,
            substruct TYPE REF TO struct,
            abstract  TYPE REF TO data,
            invalid   TYPE REF TO i,
          END OF target_struct.

    lv_json = `{"substruct":{"field":1},"abstract":{"field":1},"invalid":{"field":1}}`.
    deserialize( EXPORTING json = lv_json CHANGING  data = target_struct ).
    cl_aunit_assert=>assert_not_initial( act = target_struct-substruct msg = 'Generation of defined structure fails!' ).
    cl_aunit_assert=>assert_not_initial( act = target_struct-abstract msg = 'Generation of abstract structure fails!' ).
    cl_aunit_assert=>assert_initial( act = target_struct-invalid msg = 'Generation of invalid type into structure fails!' ).

  ENDMETHOD.                    "deserialize_ref_to_data

  METHOD conversion_exits.

    TYPES:
      BEGIN OF ts_exp,
        date_s TYPE dats,
        date_x TYPE catstsdate,
      END OF ts_exp.

    DATA: lv_json TYPE json,
          ls_exp  TYPE ts_exp,
          ls_act  LIKE ls_exp,
          lt_act  TYPE STANDARD TABLE OF ts_exp WITH DEFAULT KEY.

    CONSTANTS: lc_ts TYPE timestamp VALUE '20160708123456',
               lc_tz TYPE tzonref-tzone VALUE 'UTC'.

    CONVERT TIME STAMP lc_ts TIME ZONE lc_tz INTO DATE ls_exp-date_s.
    ls_exp-date_x = ls_exp-date_s.

    DATA: lv_date_x TYPE c LENGTH 255.
    WRITE: ls_exp-date_x TO lv_date_x.

    lv_json = serialize( data = ls_exp ).

    FIND FIRST OCCURRENCE OF lv_date_x IN lv_json.
    cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 4 msg = 'Conversion exit used without been activated!' ).

    lv_json = serialize( data = ls_exp conversion_exits = abap_true ).

    FIND FIRST OCCURRENCE OF lv_date_x IN lv_json.
    cl_aunit_assert=>assert_subrc( act = sy-subrc msg = 'Conversion exit into external format fails!' ).

    deserialize( EXPORTING json = lv_json conversion_exits = abap_true CHANGING data = ls_act ).

    cl_aunit_assert=>assert_equals( act = ls_act exp = ls_exp msg = 'Conversion exit into internal format fails!' ).

    APPEND ls_exp TO lt_act.
    lv_json = serialize( data = lt_act conversion_exits = abap_true ).

    FIND FIRST OCCURRENCE OF lv_date_x IN lv_json.
    cl_aunit_assert=>assert_subrc( act = sy-subrc msg = 'Conversion exit into external format in table fails!' ).

  ENDMETHOD.                    "conversion_exits

  METHOD generate_special_attr_names.

    DATA: lv_json  TYPE json,
          lo_data  TYPE REF TO z_ui2_data_access,
          lr_data  TYPE REF TO data,
          lr_data2 TYPE REF TO data.

    lv_json = `{"system XYZ": {"S/I\D": "XYZ", ":#*~@#$%^&*()_+-|.,=><!?/'{}[]В§": "Ш§Щ„ШЄШ¬Ш§Ш±ЩЉ"}}`. "#EC NOTEXT

    lr_data = generate( json = lv_json ).

    cl_aunit_assert=>assert_bound( act = lr_data msg = 'Generattion of ABAP data for JSON with special attribute names fails!' ).

    lo_data = z_ui2_data_access=>create( ir_data = lr_data iv_component = 'SYSTEM_XYZ-S_I_D' ).
    lr_data2 = lo_data->ref( ).

    cl_aunit_assert=>assert_bound( act = lr_data2 msg = 'Generattion of ABAP data for JSON with special attribute names fails!' ).

  ENDMETHOD.                    "generate_special_attr_names

  METHOD escape_and_unescape.
    DATA:
      BEGIN OF ls_data,
        str TYPE string,
      END OF ls_data.

    DATA:
      lv_json TYPE json,
      lv_len  TYPE i.

    lv_json = `{"str":"Subject \u EN\u00a0\ud83e\udd14  \u4f1f\u5bb6  \u6210"}`.
    deserialize( EXPORTING json = lv_json CHANGING data = ls_data ).

    " we only support unescaping of unicode symbols, but not escaping
    lv_len = strlen( ls_data-str ).
    cl_aunit_assert=>assert_equals( act = lv_len exp = 22 msg = 'Unescapment of Unicode characters fails!' ).

    lv_json = `{"str":"\u6210"}`.
    deserialize( EXPORTING json = lv_json CHANGING data = ls_data ).

    lv_len = strlen( ls_data-str ).
    cl_aunit_assert=>assert_equals( act = lv_len exp = 1 msg = 'Unescapment of Unicode characters fails!' ).

    lv_json = `{"str":"\u4f1f\u5bb6"}`.
    deserialize( EXPORTING json = lv_json CHANGING data = ls_data ).

    lv_len = strlen( ls_data-str ).
    cl_aunit_assert=>assert_equals( act = lv_len exp = 2 msg = 'Unescapment of Unicode characters fails!' ).

    " we only support unescaping of unicode symbols, but not escaping
    "cl_aunit_assert=>assert_equals( act = ls_data-str exp = `Subject u ENu00a0ud83eudd14` msg = 'Unescapment of Unicode characters fails!' ).

  ENDMETHOD.                    "escape_and_unescape

  METHOD serialize_formatted.

    DATA: lt_fligt_exp TYPE STANDARD TABLE OF sflight WITH DEFAULT KEY,
          lt_fligt_act LIKE lt_fligt_exp,
          lv_json      TYPE json.

    SELECT * FROM sflight INTO TABLE lt_fligt_exp UP TO 2 ROWS ORDER BY PRIMARY KEY. "#EC CI_NOWHERE

    " serialize table lt_flight into JSON, skipping initial fields and converting ABAP field names into camelCase
    lv_json      = serialize( data = lt_fligt_exp compress = abap_true pretty_name = pretty_mode-camel_case format_output = abap_true ).
    cl_aunit_assert=>assert_not_initial( act = lv_json msg = 'Serialization with formatting fails!' ).

    deserialize( EXPORTING json = lv_json pretty_name = pretty_mode-camel_case CHANGING data = lt_fligt_act ).
    cl_aunit_assert=>assert_equals( act = lt_fligt_act exp = lt_fligt_exp msg = 'Deserialization with formatting fails!' ).

  ENDMETHOD. " serialize_formatted

  METHOD serialize_cycle_reference.

    DATA: dref1   TYPE REF TO data,
          dref2   TYPE REF TO data,
          lv_json TYPE json ##NEEDED.

    GET REFERENCE OF dref2 INTO dref1.
    GET REFERENCE OF dref1 INTO  dref2.

    lv_json = abap_to_json_simple_transform( dref1 ).
    lv_json = serialize(  data             = dref1
                          pretty_name      = pretty_mode-low_case
                          compress         = abap_false
                          hex_as_base64    = abap_false
                          format_output    = abap_true
                          assoc_arrays     = abap_true
                          assoc_arrays_opt = abap_true ).

  ENDMETHOD.                    "serialize_cycle_reference

  METHOD deserialize_strict_table_null.

    TYPES: BEGIN OF ty_test,
             tab1    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
             BEGIN OF struc1,
               field1 TYPE string,
             END OF struc1,
             string1 TYPE string,
           END OF ty_test.

    DATA: ls_test TYPE ty_test,
          lv_json TYPE json,
          lo_json TYPE t_json.

    CREATE OBJECT lo_json
      EXPORTING
        strict_mode = abap_true
        pretty_name = pretty_mode-camel_case.

    lv_json = '{"tab1":null,"struc1":{"field1":"hugo"},"string1":"u__u"}'.
    lo_json->deserialize_int( EXPORTING json = lv_json CHANGING data = ls_test ).

    cl_abap_unit_assert=>assert_initial( act = ls_test-tab1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'hugo' act = ls_test-struc1-field1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'u__u' act = ls_test-string1 ).

  ENDMETHOD.                    "deserialize_strict_table_null

  METHOD deserialize_strict_struct_null.

    TYPES: BEGIN OF ty_test,
             tab1    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
             BEGIN OF struc1,
               field1 TYPE string,
             END OF struc1,
             string1 TYPE string,
           END OF ty_test.

    DATA: ls_test TYPE ty_test,
          lv_val  TYPE string,
          lv_json TYPE json,
          lo_json TYPE t_json.

    CREATE OBJECT lo_json
      EXPORTING
        strict_mode = abap_true
        pretty_name = pretty_mode-camel_case.

    lv_json = '{"tab1":["hugo"],"struc1":null,"string1":"u__u"}'.
    lo_json->deserialize_int( EXPORTING json = lv_json CHANGING data = ls_test ).

    READ TABLE ls_test-tab1 INDEX 1 INTO lv_val.

    cl_abap_unit_assert=>assert_equals( exp = 'hugo' act = lv_val ).
    cl_abap_unit_assert=>assert_initial( act = ls_test-struc1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'u__u' act = ls_test-string1 ).

  ENDMETHOD.                    "deserialize_strict_struct_null

  METHOD deserialize_strict_string_null.

    TYPES: BEGIN OF ty_test,
             tab1    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
             BEGIN OF struc1,
               field1 TYPE string,
             END OF struc1,
             string1 TYPE string,
           END OF ty_test.

    DATA: ls_test TYPE ty_test,
          lv_val  TYPE string,
          lv_json TYPE json,
          lo_json TYPE t_json.

    CREATE OBJECT lo_json
      EXPORTING
        strict_mode = abap_true
        pretty_name = pretty_mode-camel_case.

    lv_json = '{"tab1":["hugo"],"struc1":{"field1":"hugo"},"string1":null}'.
    lo_json->deserialize_int( EXPORTING json = lv_json CHANGING data = ls_test ).

    READ TABLE ls_test-tab1 INDEX 1 INTO lv_val.

    cl_abap_unit_assert=>assert_equals( exp = 'hugo' act = lv_val ).
    cl_abap_unit_assert=>assert_equals( exp = 'hugo' act = ls_test-struc1-field1 ).
    cl_abap_unit_assert=>assert_initial( act = ls_test-string1 ).

  ENDMETHOD.                    "deserialize_strict_string_null

  METHOD deser_table_invalid_value.

    TYPES:
      BEGIN OF ty_test,
        int_p2 TYPE i,
      END OF ty_test,
      ty_tab   TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
      ty_s_tab TYPE STANDARD TABLE OF ty_test WITH DEFAULT KEY,
      BEGIN OF ty_test2,
        tab1    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
        BEGIN OF struc1,
          field1 TYPE string,
        END OF struc1,
        string1 TYPE string,
        tab2    TYPE STANDARD TABLE OF ty_test WITH DEFAULT KEY,
        tab3    TYPE STANDARD TABLE OF ty_tab WITH DEFAULT KEY,
        tab4    TYPE STANDARD TABLE OF ty_s_tab WITH DEFAULT KEY,
      END OF ty_test2.

    DATA: ls_test TYPE ty_test2,
          lo_json TYPE t_json,
          lx_move TYPE REF TO cx_sy_move_cast_error.

    CREATE OBJECT lo_json
      EXPORTING
        compress         = abap_true
        pretty_name      = pretty_mode-camel_case
        assoc_arrays     = abap_true
        assoc_arrays_opt = abap_true
        ts_as_iso8601    = abap_false
        expand_includes  = abap_true
        strict_mode      = abap_true " raise sometimes an exception in error case
        numc_as_string   = abap_false
        conversion_exits = abap_false.

    TRY.
        lo_json->deserialize_int( EXPORTING json = '{"tab1":true,"string1":"u__u"}' CHANGING data = ls_test ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_sy_move_cast_error INTO lx_move.
        cl_abap_unit_assert=>assert_equals( exp = `$.tab1` act = lx_move->source_typename ).
        cl_abap_unit_assert=>assert_equals( exp = `TABLE` act = lx_move->target_typename ).
        cl_abap_unit_assert=>assert_equals( exp = `Source type $.tab1 is not compatible, for the purposes of assignment, with target type TABLE` act = lx_move->get_text( ) ).
    ENDTRY.

    TRY.
        lo_json->deserialize_int( EXPORTING json = '{"tab1":"hugo","string1":"u__u"}' CHANGING data = ls_test ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_sy_move_cast_error INTO lx_move.
        cl_abap_unit_assert=>assert_equals( exp = `$.tab1` act = lx_move->source_typename ).
    ENDTRY.

    TRY.
        lo_json->deserialize_int( EXPORTING json = '{"tab1":["hugo",?]}' CHANGING data = ls_test ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_sy_move_cast_error INTO lx_move.
        cl_abap_unit_assert=>assert_equals( exp = `$.tab1[2]` act = lx_move->source_typename ).
    ENDTRY.

    TRY.
        lo_json->deserialize_int( EXPORTING json = '{"tab2":[{"intP2":1 },{"intP2":"illegal int"}]}' CHANGING data = ls_test ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_sy_move_cast_error INTO lx_move.
        cl_abap_unit_assert=>assert_equals( exp = `$.tab2[2].intP2` act = lx_move->source_typename ).
    ENDTRY.

    TRY.
        lo_json->deserialize_int( EXPORTING json = '{"tab3":[[1,2],["str1","str2"]]}' CHANGING data = ls_test ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_sy_move_cast_error INTO lx_move.
        cl_abap_unit_assert=>assert_equals( exp = `$.tab3[2][1]` act = lx_move->source_typename ).
    ENDTRY.

    TRY.
        lo_json->deserialize_int( EXPORTING json = '{"tab4":[[{"intP2":1}],[{"intP2":"illegal int"}]]}' CHANGING data = ls_test ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_sy_move_cast_error INTO lx_move.
        cl_abap_unit_assert=>assert_equals( exp = `$.tab4[2][1].intP2` act = lx_move->source_typename ).
    ENDTRY.

  ENDMETHOD.

  METHOD deser_structure_invalid_value.

    TYPES: BEGIN OF ty_test,
             tab1    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
             BEGIN OF struc1,
               field1 TYPE string,
             END OF struc1,
             string1 TYPE string,
           END OF ty_test.

    DATA: ls_test TYPE ty_test,
          lo_json TYPE t_json,
          lx_move TYPE REF TO cx_sy_move_cast_error.

    CREATE OBJECT lo_json
      EXPORTING
        compress         = abap_true
        pretty_name      = pretty_mode-camel_case
        assoc_arrays     = abap_true
        assoc_arrays_opt = abap_true
        ts_as_iso8601    = abap_false
        expand_includes  = abap_true
        strict_mode      = abap_true " raise sometimes an exception in error case
        numc_as_string   = abap_false
        conversion_exits = abap_false.

    TRY.
        lo_json->deserialize_int( EXPORTING json = '{"struc1":true,"string1":"u__u"}' CHANGING data = ls_test ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_sy_move_cast_error INTO lx_move.
        cl_abap_unit_assert=>assert_equals( exp = `$.struc1` act = lx_move->source_typename ).
        cl_abap_unit_assert=>assert_equals( exp = `STRUCTURE` act = lx_move->target_typename ).
        cl_abap_unit_assert=>assert_equals( exp = `Source type $.struc1 is not compatible, for the purposes of assignment, with target type STRUCTURE` act = lx_move->get_text( ) ).
    ENDTRY.

    TRY.
        lo_json->deserialize_int( EXPORTING json = '{"struc1":"hugo","string1":"u__u"}' CHANGING data = ls_test ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_sy_move_cast_error INTO lx_move.
        cl_abap_unit_assert=>assert_equals( exp = `$.struc1` act = lx_move->source_typename ).
    ENDTRY.
  ENDMETHOD.

  METHOD deser_field_invalid_value.

    TYPES: BEGIN OF ty_test,
             tab1    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
             BEGIN OF struc1,
               field1 TYPE string,
             END OF struc1,
             string1 TYPE string,
           END OF ty_test.

    DATA: ls_test TYPE ty_test,
          lo_json TYPE t_json,
          lx_move TYPE REF TO cx_sy_move_cast_error.

    CREATE OBJECT lo_json
      EXPORTING
        compress         = abap_true
        pretty_name      = pretty_mode-camel_case
        assoc_arrays     = abap_true
        assoc_arrays_opt = abap_true
        ts_as_iso8601    = abap_false
        expand_includes  = abap_true
        strict_mode      = abap_true " raise sometimes an exception in error case
        numc_as_string   = abap_false
        conversion_exits = abap_false.

    TRY.
        lo_json->deserialize_int( EXPORTING json = '{"struc1":{"field1":?}}' CHANGING data = ls_test ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_sy_move_cast_error INTO lx_move.
        cl_abap_unit_assert=>assert_equals( exp = `$.struc1.field1` act = lx_move->source_typename ).
        cl_abap_unit_assert=>assert_equals( exp = `STRING` act = lx_move->target_typename ).
    ENDTRY.


  ENDMETHOD.

  METHOD serialize_time_stamp.

    TYPES ty_tms TYPE xsddatetime_z.

    DATA: lv_xsd_tms  TYPE xsddatetime_z,
          lv_xsd_tms2 TYPE ty_tms,
          lv_act      TYPE json.

    lv_xsd_tms = '19370101120027' ##LITERAL.
    lv_act = serialize( ts_as_iso8601 = abap_true data = lv_xsd_tms ).
    cl_abap_unit_assert=>assert_equals( exp = '"1937-01-01T12:00:27Z"' act = lv_act ).

    deserialize( EXPORTING  json = '"1937-01-01T12:00:27"' CHANGING data = lv_xsd_tms ).
    cl_abap_unit_assert=>assert_equals( exp = '19370101120027' act = lv_xsd_tms ).

    lv_xsd_tms2 = '19370101120027' ##LITERAL.
    lv_act = serialize( ts_as_iso8601 = abap_true data = lv_xsd_tms2 ).
    cl_abap_unit_assert=>assert_equals( exp = '"1937-01-01T12:00:27Z"' act = lv_act ).

    deserialize( EXPORTING  json = '"1937-01-01T12:00:27"' CHANGING data = lv_xsd_tms2 ).
    cl_abap_unit_assert=>assert_equals( exp = '19370101120027' act = lv_xsd_tms2 ).

  ENDMETHOD.

ENDCLASS.       "abap_unit_testclass
