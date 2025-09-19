*"* local class implementation for public class
*"* use this source file for the implementation part of
*"* local helper classes

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
      so_regex_iso8601 TYPE REF TO cl_abap_regex,
      so_regex_edm_date_time TYPE REF TO cl_abap_regex.

    CLASS-METHODS:
      _escape IMPORTING in TYPE data EXPORTING out TYPE string,
      to_md5  IMPORTING iv_value TYPE string RETURNING VALUE(rv_result) TYPE string,
      read_string IMPORTING json TYPE string mark TYPE i CHANGING offset TYPE i DEFAULT 0 text TYPE string RAISING cx_sy_move_cast_error,
      read_iso8601 IMPORTING in TYPE string RETURNING VALUE(rv_tstm) TYPE timestampl,
      read_edm_datetime IMPORTING in TYPE string RETURNING VALUE(rv_tstm) TYPE timestampl,
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
    create_regexp so_regex_iso8601 '^(?:(\d{4})-?(\d{2})-?(\d{2}))?(?:T(\d{2}):?(\d{2})(?::?(\d{2}))?(?:[\.,](\d{0,9}))?(?:Z|(?:([+-])(\d{2})(?::?(\d{2}))?))?)?\s*$'.
    " support for Edm.DateTime => http://www.odata.org/documentation/odata-version-2-0/json-format/
    create_regexp so_regex_edm_date_time '^\/[Dd][Aa][Tt][Ee]\((-?\d+)(?:([+-])(\d{1,4}))?\)\/\s*$'.

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
    ELSEIF stimestmp+8(1) IS INITIAL. " date-only, default to 000000 time
      stimestmp+8(6) = '000000'.
    ELSEIF stimestmp(1) IS INITIAL. " time-only, default to current date
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

  METHOD read_edm_datetime.

    CONSTANTS: lc_epochs TYPE c LENGTH 15 VALUE '19700101000000.'.

    DATA: ticks       TYPE c LENGTH 21,
          offset_sign TYPE c,
          offset      TYPE c LENGTH 4,
          pticks      TYPE p,
          pseconds    TYPE p,
          psubsec     TYPE p,
          stimestmp   TYPE string.

    FIND FIRST OCCURRENCE OF REGEX so_regex_edm_date_time IN in SUBMATCHES ticks offset_sign offset.
    CHECK sy-subrc IS INITIAL.

    pticks     = ticks.
    pseconds   = pticks DIV 1000. " in seconds
    psubsec    = pticks MOD 1000. " in subsec

    stimestmp = psubsec.
    CONCATENATE lc_epochs stimestmp INTO stimestmp.
    rv_tstm = stimestmp.

    rv_tstm = cl_abap_tstmp=>add( tstmp = rv_tstm secs = pseconds ).

    IF offset_sign IS NOT INITIAL.
      pticks = offset * 60. "offset is in minutes
      IF offset_sign EQ '+'.
        rv_tstm = cl_abap_tstmp=>subtractsecs( tstmp = rv_tstm secs = pticks ).
      ELSE.
        rv_tstm = cl_abap_tstmp=>add( tstmp = rv_tstm secs = pticks ).
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

ENDCLASS.                    "lc_json_custom
