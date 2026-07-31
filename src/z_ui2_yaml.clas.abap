CLASS z_ui2_yaml DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES yaml TYPE string.
    TYPES pretty_name_mode TYPE c LENGTH 1.
    TYPES: BEGIN OF name_mapping,
             abap TYPE abap_compname,
             yaml TYPE string,
           END OF name_mapping.
    TYPES name_mappings TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY abap.

    CONSTANTS:
      BEGIN OF pretty_mode,
        none        TYPE c LENGTH 1 VALUE ``,
        low_case    TYPE c LENGTH 1 VALUE 'L',
        camel_case  TYPE c LENGTH 1 VALUE 'X',
        pascal_case TYPE c LENGTH 1 VALUE 'P',
        extended    TYPE c LENGTH 1 VALUE 'Y',
      END OF pretty_mode.

    CONSTANTS version TYPE i VALUE 1 ##NO_TEXT.

    METHODS constructor
      IMPORTING pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-none
                name_mappings    TYPE name_mappings    OPTIONAL
                strict_mode      TYPE abap_bool        DEFAULT abap_false
                assoc_arrays     TYPE abap_bool        DEFAULT abap_false
                indent           TYPE i                OPTIONAL
                emit_doc_markers TYPE abap_bool        DEFAULT abap_false
                quote_style      TYPE c                OPTIONAL
                flow_threshold   TYPE i                OPTIONAL.

    METHODS deserialize_int
      IMPORTING yaml  TYPE string  OPTIONAL
                yamlx TYPE xstring OPTIONAL
      CHANGING  data  TYPE data
      RAISING   cx_sy_move_cast_error.

    CLASS-METHODS deserialize
      IMPORTING yaml          TYPE string           OPTIONAL
                yamlx         TYPE xstring          OPTIONAL
                pretty_name   TYPE pretty_name_mode DEFAULT pretty_mode-none
                name_mappings TYPE name_mappings    OPTIONAL
      CHANGING  data          TYPE data.

    CLASS-METHODS generate
      IMPORTING yaml           TYPE string
      RETURNING VALUE(rr_data) TYPE REF TO data.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_pretty_name      TYPE pretty_name_mode.
    DATA mt_name_mappings    TYPE name_mappings.
    DATA mv_strict           TYPE abap_bool.
    DATA mv_assoc_arrays     TYPE abap_bool.
    DATA mv_indent           TYPE i.
    DATA mv_emit_doc_markers TYPE abap_bool.
    DATA mv_quote_style      TYPE c.
    DATA mv_flow_threshold   TYPE i.

ENDCLASS.

CLASS z_ui2_yaml IMPLEMENTATION.

  METHOD constructor.
    mv_pretty_name      = pretty_name.
    mt_name_mappings    = name_mappings.
    mv_strict           = strict_mode.
    mv_assoc_arrays     = assoc_arrays.
    mv_indent           = COND #( WHEN indent IS SUPPLIED THEN indent ELSE 2 ).
    mv_emit_doc_markers = emit_doc_markers.
    mv_quote_style      = COND #( WHEN quote_style IS SUPPLIED THEN quote_style ELSE 'P' ).
    mv_flow_threshold   = COND #( WHEN flow_threshold IS SUPPLIED THEN flow_threshold ELSE 0 ).
  ENDMETHOD.

  METHOD deserialize_int.
    DATA lv_yaml TYPE string.
    IF yamlx IS SUPPLIED AND yamlx IS NOT INITIAL.
      lv_yaml = cl_abap_codepage=>convert_from( source   = yamlx
                                                codepage = `UTF-8` ).
    ELSE.
      lv_yaml = yaml.
    ENDIF.
    DATA(root) = lcl_parser=>parse( lcl_scanner=>scan( lv_yaml ) ).
    lcl_typed_mapper=>map( EXPORTING node          = root
                                     pretty_name   = mv_pretty_name
                                     name_mappings = mt_name_mappings
                                     strict        = mv_strict
                           CHANGING  data          = data ).
  ENDMETHOD.

  METHOD deserialize.
    DATA(o) = NEW z_ui2_yaml( pretty_name   = pretty_name
                              name_mappings = name_mappings ).
    TRY.
        o->deserialize_int( EXPORTING yaml  = yaml
                                      yamlx = yamlx
                            CHANGING  data  = data ).
      CATCH cx_sy_move_cast_error.
        " static API is lenient — strict_mode defaults false, so unreachable by design
    ENDTRY.
  ENDMETHOD.

  METHOD generate.
    TRY.
        DATA(root) = lcl_parser=>parse( lcl_scanner=>scan( yaml ) ).
        rr_data = lcl_gen_mapper=>generate( root ).
      CATCH cx_sy_conversion_error.
        " parse error — return initial (null ref)
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
