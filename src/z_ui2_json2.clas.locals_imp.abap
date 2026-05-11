*"* local class implementation for public class
*"* use this source file for the implementation part of
*"* local helper classes

*----------------------------------------------------------------------*
*       CLASS lcl_util DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

CLASS lcl_util DEFINITION FINAL FRIENDS Z_UI2_JSON2.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      detect_typekind
        IMPORTING
          !TYPE_DESCR      TYPE REF TO cl_abap_elemdescr
          !CONVEXIT        TYPE string
          !NUMC_AS_STRING  TYPE abap_bool OPTIONAL
          !BOOL_TYPES      TYPE string OPTIONAL
          !BOOL_3STATE     TYPE string OPTIONAL
        RETURNING
          VALUE(RV_TYPE)   TYPE abap_typekind,
      get_convexit_func
        IMPORTING
          !ELEM_DESCR TYPE REF TO cl_abap_elemdescr
          !INPUT      TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(RV_FUNC) TYPE string,
      read_json_to_string
        IMPORTING reader TYPE REF TO if_json_reader
        RETURNING VALUE(rv_json) TYPE string,
      read_iso8601 IMPORTING in TYPE string RETURNING VALUE(rv_tstm) TYPE timestampl,
      read_edm_datetime IMPORTING in TYPE string RETURNING VALUE(rv_tstm) TYPE timestampl,
      describe_type IMPORTING io_type_descr TYPE REF TO cl_abap_typedescr RETURNING VALUE(rv_typename) TYPE string.

    CLASS-DATA:
      so_regex_date                 TYPE REF TO cl_abap_regex,
      so_regex_time                 TYPE REF TO cl_abap_regex,
      so_regex_guid                 TYPE REF TO cl_abap_regex,
      so_regex_edm_date_time        TYPE REF TO cl_abap_regex,
      so_regex_edm_time             TYPE REF TO cl_abap_regex,
      so_regex_generate_normalize   TYPE REF TO cl_abap_regex,
      so_regex_generate_camel_case  TYPE REF TO cl_abap_regex,
      so_regex_iso8601              TYPE REF TO cl_abap_regex,
      so_type_s                     TYPE REF TO cl_abap_elemdescr,
      so_type_f                     TYPE REF TO cl_abap_elemdescr,
      so_type_p                     TYPE REF TO cl_abap_elemdescr,
      so_type_i                     TYPE REF TO cl_abap_elemdescr,
      so_type_b                     TYPE REF TO cl_abap_elemdescr,
      so_type_d                     TYPE REF TO cl_abap_elemdescr,
      so_type_t                     TYPE REF TO cl_abap_elemdescr,
      so_type_ts                    TYPE REF TO cl_abap_elemdescr,
      so_type_tsl                   TYPE REF TO cl_abap_elemdescr,
      so_type_reftab                TYPE REF TO cl_abap_tabledescr.

ENDCLASS.                    "lcl_util DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_util IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_util IMPLEMENTATION.

  METHOD class_constructor.

    so_regex_iso8601 = cl_abap_regex=>create_pcre( pattern = '^(?:(\d{4})-?(\d{2})-?(\d{2}))?(?:T(\d{2}):?(\d{2})(?::?(\d{2}))?(?:[\.,](\d{0,9}))?(?:Z|(?:([+-])(\d{2})(?::?(\d{2}))?))?)?\s*$' ) ##NO_TEXT.
    so_regex_edm_date_time = cl_abap_regex=>create_pcre( pattern = '(?i)^\/date\((-?\d+)(?:([+-])(\d{1,4}))?\)\/\s*$' ) ##NO_TEXT.

    so_regex_date      = cl_abap_regex=>create_pcre( pattern = '^(\d{4})-(\d{2})-(\d{2})(?:[^T]|$)' ) ##NO_TEXT.
    so_regex_time      = cl_abap_regex=>create_pcre( pattern = '^(\d{2}):(\d{2}):(\d{2})' ) ##NO_TEXT.
    so_regex_guid      = cl_abap_regex=>create_pcre( pattern = '(?i)^([0-9a-f]{8})-([0-9a-f]{4})-([0-9a-f]{4})-([0-9a-f]{4})-([0-9a-f]{12})\s*$' ) ##NO_TEXT.
    so_regex_edm_time  = cl_abap_regex=>create_pcre( pattern = '^-?P(?:(\d+)Y)?(?:(\d+)M)?(?:(\d+)D)?(?:T(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)(?:\.(\d+))?S)?)?\s*$' ) ##NO_TEXT.
    so_regex_generate_normalize   = cl_abap_regex=>create_pcre( pattern = '[^0-9a-zA-Z_]+' ) ##NO_TEXT.
    so_regex_generate_camel_case  = cl_abap_regex=>create_pcre( pattern = '([a-z])([A-Z])' ) ##NO_TEXT.

    so_type_s = cl_abap_elemdescr=>get_string( ).
    so_type_f = cl_abap_elemdescr=>get_f( ).
    so_type_p = cl_abap_elemdescr=>get_p( p_length = 16 p_decimals = 0 ).
    so_type_i = cl_abap_elemdescr=>get_i( ).
    so_type_d = cl_abap_elemdescr=>get_d( ).
    so_type_t = cl_abap_elemdescr=>get_t( ).
    so_type_ts  ?= cl_abap_typedescr=>describe_by_name( 'TIMESTAMP' ).
    so_type_tsl ?= cl_abap_typedescr=>describe_by_name( 'TIMESTAMPL' ).
    so_type_b   ?= cl_abap_typedescr=>describe_by_name( 'ABAP_BOOL' ).

    DATA(lo_type) = cl_abap_refdescr=>get_ref_to_data( ).
    so_type_reftab = cl_abap_tabledescr=>get( p_line_type = lo_type ).

  ENDMETHOD.

  METHOD describe_type.

    DATA: lv_kind_name  TYPE string,
          lv_pos        TYPE i.

    rv_typename = `?`.

    CHECK io_type_descr IS NOT INITIAL.

    FIND FIRST OCCURRENCE OF '\TYPE=' IN io_type_descr->absolute_name MATCH OFFSET lv_pos.
    IF sy-subrc IS INITIAL.
      lv_pos = lv_pos + 6.
      IF io_type_descr->absolute_name+lv_pos(1) <> '%'.
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
      rv_typename = |{ lv_kind_name }({ rv_typename })|.
      ELSE.
        rv_typename = lv_kind_name.
      ENDIF.
    ENDIF.

  ENDMETHOD.

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
      IF offset_sign = '+'.
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
    stimestmp = lc_epochs && stimestmp.
    rv_tstm = stimestmp.

    rv_tstm = cl_abap_tstmp=>add( tstmp = rv_tstm secs = pseconds ).

    IF offset_sign IS NOT INITIAL.
      pticks = offset * 60. "offset is in minutes
      IF offset_sign = '+'.
        rv_tstm = cl_abap_tstmp=>subtractsecs( tstmp = rv_tstm secs = pticks ).
      ELSE.
        rv_tstm = cl_abap_tstmp=>add( tstmp = rv_tstm secs = pticks ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD detect_typekind.

    DATA: domain_name     TYPE domname,
          inner_elemdescr TYPE REF TO cl_abap_elemdescr.

    IF convexit IS NOT INITIAL.
      rv_type = z_ui2_json2=>e_typekind-convexit.
    ELSE.
      rv_type = type_descr->type_kind.
      IF rv_type = cl_abap_typedescr=>typekind_packed.

        IF type_descr->help_id IS NOT INITIAL AND NOT contains( val = type_descr->absolute_name end = type_descr->help_id ).
          TRY.
              inner_elemdescr ?= cl_abap_elemdescr=>describe_by_name( type_descr->help_id ).
              IF inner_elemdescr->is_ddic_type( ) = abap_true.
                domain_name = inner_elemdescr->get_ddic_field( )-domname.
              ENDIF.
            CATCH cx_root.                               "#EC CATCH_ALL
              domain_name = ''.
          ENDTRY.
        ELSE.
          IF type_descr->is_ddic_type( ) = abap_true.
            domain_name = type_descr->get_ddic_field( )-domname.
          ENDIF.
        ENDIF.

        IF domain_name = 'TZNTSTMPS' OR domain_name = 'XSDDATETIME_Z'.
          rv_type = z_ui2_json2=>e_typekind-ts_iso8601.
        ELSEIF domain_name = 'TZNTSTMPL' OR domain_name = 'XSDDATETIME_LONG_Z'.
          rv_type = z_ui2_json2=>e_typekind-tsl_iso8601.
        ENDIF.

      ELSEIF rv_type = cl_abap_typedescr=>typekind_num AND numc_as_string = abap_true.
        rv_type = z_ui2_json2=>e_typekind-numc_string.
      ELSEIF rv_type = cl_abap_typedescr=>typekind_string AND type_descr->absolute_name = z_ui2_json2=>mc_json_type.
        rv_type = z_ui2_json2=>e_typekind-json.
      ELSEIF rv_type = cl_abap_typedescr=>typekind_char AND type_descr->output_length = 1 AND bool_types CS type_descr->absolute_name.
        IF bool_3state CS type_descr->absolute_name.
          rv_type = z_ui2_json2=>e_typekind-tribool.
        ELSE.
          rv_type = z_ui2_json2=>e_typekind-bool.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_convexit_func.

    DATA ls_dfies TYPE dfies.

    elem_descr->get_ddic_field(
      RECEIVING
        p_flddescr   = ls_dfies
      EXCEPTIONS
        not_found    = 1
        no_ddic_type = 2
        OTHERS       = 3
    ).
    IF sy-subrc IS INITIAL AND ls_dfies-convexit IS NOT INITIAL.
      IF input = abap_true.
        rv_func = 'CONVERSION_EXIT_' && ls_dfies-convexit && '_INPUT'.
      ELSE.
        rv_func = 'CONVERSION_EXIT_' && ls_dfies-convexit && '_OUTPUT'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD read_json_to_string.
    " Workaround: IF_JSON_READER=>skip_node( writer ) does not work correctly
    " on member positions mid-document. Replace with skip_node( writer ) once fixed.
    DATA(lo_writer) = cl_json_string_writer=>create( ).
    DATA(lv_depth) = 0.
    DATA lv_is_member TYPE c LENGTH 64.
    DO.
      CASE reader->node-type.
        WHEN if_json_node=>open_object.
          IF reader->node-name IS NOT INITIAL AND lv_depth > 0.
            lo_writer->open_member( reader->node-name ).
            lv_is_member+lv_depth(1) = 'X'.
          ELSE.
            lv_is_member+lv_depth(1) = ' '.
          ENDIF.
          lo_writer->open_object( ).
          lv_depth = lv_depth + 1.
        WHEN if_json_node=>close_object.
          lo_writer->close_object( ).
          lv_depth = lv_depth - 1.
          IF lv_is_member+lv_depth(1) = 'X'.
            lo_writer->close_member( ).
          ENDIF.
          IF lv_depth = 0. EXIT. ENDIF.
        WHEN if_json_node=>open_array.
          IF reader->node-name IS NOT INITIAL AND lv_depth > 0.
            lo_writer->open_member( reader->node-name ).
            lv_is_member+lv_depth(1) = 'X'.
          ELSE.
            lv_is_member+lv_depth(1) = ' '.
          ENDIF.
          lo_writer->open_array( ).
          lv_depth = lv_depth + 1.
        WHEN if_json_node=>close_array.
          lo_writer->close_array( ).
          lv_depth = lv_depth - 1.
          IF lv_is_member+lv_depth(1) = 'X'.
            lo_writer->close_member( ).
          ENDIF.
          IF lv_depth = 0. EXIT. ENDIF.
        WHEN if_json_node=>string.
          IF reader->node-name IS NOT INITIAL AND lv_depth > 0.
            lo_writer->open_member( reader->node-name ).
          ENDIF.
          lo_writer->write_string( reader->node-value ).
          IF reader->node-name IS NOT INITIAL AND lv_depth > 0.
            lo_writer->close_member( ).
          ENDIF.
        WHEN if_json_node=>number.
          IF reader->node-name IS NOT INITIAL AND lv_depth > 0.
            lo_writer->open_member( reader->node-name ).
          ENDIF.
          lo_writer->write_number( reader->node-value ).
          IF reader->node-name IS NOT INITIAL AND lv_depth > 0.
            lo_writer->close_member( ).
          ENDIF.
        WHEN if_json_node=>boolean.
          IF reader->node-name IS NOT INITIAL AND lv_depth > 0.
            lo_writer->open_member( reader->node-name ).
          ENDIF.
          lo_writer->write_boolean( reader->node-value ).
          IF reader->node-name IS NOT INITIAL AND lv_depth > 0.
            lo_writer->close_member( ).
          ENDIF.
        WHEN if_json_node=>null.
          IF reader->node-name IS NOT INITIAL AND lv_depth > 0.
            lo_writer->open_member( reader->node-name ).
          ENDIF.
          lo_writer->write_null( ).
          IF reader->node-name IS NOT INITIAL AND lv_depth > 0.
            lo_writer->close_member( ).
          ENDIF.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
      reader->next_node( ).
    ENDDO.
    rv_json = CAST cl_json_string_writer( lo_writer )->get_json( ).
  ENDMETHOD.

ENDCLASS.                    "lcl_util IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION FINAL FRIENDS Z_UI2_JSON2.

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
  ENDMETHOD.

ENDCLASS.                    "lcl_test IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lc_json_custom DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lc_json_custom DEFINITION FINAL INHERITING FROM Z_UI2_JSON2.
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
    DATA(lo_json) = NEW lc_json_custom(
      compress         = compress
      pretty_name      = pretty_name
      assoc_arrays     = abap_true
      assoc_arrays_opt = abap_true
      expand_includes  = abap_true
      numc_as_string   = abap_true
      bool_types       = `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL\TYPE=BOOLEAN\TYPE=/UI2/BOOLEAN`
      ts_as_iso8601    = abap_true ).
    r_json = lo_json->serialize_int( data = data ).
  ENDMETHOD.

  METHOD deserialize_ex.
    IF json IS NOT INITIAL.
      DATA(lo_json) = NEW lc_json_custom(
        pretty_name      = pretty_name
        assoc_arrays     = abap_true
        assoc_arrays_opt = abap_true ).
      TRY .
          lo_json->deserialize_int( EXPORTING json = json CHANGING data = data ).
        CATCH cx_sy_move_cast_error.                    "#EC NO_HANDLER
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD is_compressable.
    IF type_descr->absolute_name = `\TYPE=STRING` OR name = `INITIAL`.
      rv_compress = abap_false.
    ELSE.
      rv_compress = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD pretty_name_ex.
    out = super->pretty_name_ex( in ).
    out = out && 'Xxx'.
  ENDMETHOD.

  METHOD dump_type.

    DATA: is_ddic    TYPE abap_bool,
          ddic_field TYPE dfies.

    is_ddic = type_descr->is_ddic_type( ).
    IF is_ddic = abap_true.
      ddic_field = type_descr->get_ddic_field( ).
      IF mv_ts_as_iso8601 = c_bool-true AND ddic_field-domname = `TZNTSTMPL`.
        r_json = data.
        r_json = |"{ r_json(4) }-{ r_json+4(2) }-{ r_json+6(2) }T{ r_json+8(2) }:{ r_json+10(2) }:{ r_json+12(2) }.{ r_json+15(7) }Z"|.
        RETURN.
      ENDIF.
    ENDIF.
    IF mv_ts_as_iso8601 = c_bool-true AND type_descr->absolute_name = `\TYPE=LCM_CHANGED_ON`.
      r_json = data.
      r_json = |"{ r_json(4) }-{ r_json+4(2) }-{ r_json+6(2) }T{ r_json+8(2) }:{ r_json+10(2) }:{ r_json+12(2) }.{ r_json+15(7) }Z"|.
      RETURN.
    ENDIF.

    r_json = super->dump_type( data = data type_descr = type_descr convexit = convexit ).

  ENDMETHOD.

ENDCLASS.                    "lc_json_custom
