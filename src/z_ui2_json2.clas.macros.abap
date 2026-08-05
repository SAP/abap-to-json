*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE is_compressable.
  IF mv_compress = abap_false.
    &3 = abap_false.
  ELSEIF mv_extended IS INITIAL.
    &3 = abap_true.
  ELSE.
    &3 = is_compressable( type_descr = &1 name = &2 ).
  ENDIF.
END-OF-DEFINITION.
DEFINE format_name.
  CASE &2.
    WHEN pretty_mode-camel_case.
      &3 = pretty_name( &1 ).
    WHEN pretty_mode-pascal_case.
      &3 = pretty_name( in = &1 pascal_case = c_bool-true ).
    WHEN pretty_mode-extended.
      &3 = pretty_name_ex( &1 ).
    WHEN pretty_mode-user_low_case.
      READ TABLE mt_name_mappings WITH TABLE KEY abap = &1 ASSIGNING <cache>. "#EC WARNOK
      IF sy-subrc IS INITIAL.
        &3 = <cache>-json.
      ELSE.
        &3 = &1.
        TRANSLATE &3 TO LOWER CASE.
      ENDIF.
    WHEN pretty_mode-user.
      READ TABLE mt_name_mappings WITH TABLE KEY abap = &1 ASSIGNING <cache>. "#EC WARNOK
      IF sy-subrc IS INITIAL.
        &3 = <cache>-json.
      ELSE.
        &3 = &1.
      ENDIF.
    WHEN pretty_mode-low_case.
      &3 = &1.
      TRANSLATE &3 TO LOWER CASE.
    WHEN OTHERS.
      &3 = &1.
  ENDCASE.
END-OF-DEFINITION.

DEFINE restore_dref.
  " &1 = reader, &2 = data, &3 = type_descr (REF type)
  ref_descr ?= &3.
  data_descr ?= ref_descr->get_referenced_type( ).
  IF &2 IS INITIAL.
    IF data_descr->type_kind = data_descr->typekind_data.
      generate_int_r( EXPORTING reader = &1 CHANGING data = &2 ).
      RETURN.
    ELSE.
      CREATE DATA &2 TYPE HANDLE data_descr.
    ENDIF.
  ENDIF.
  data_ref ?= &2.
  ASSIGN data_ref->* TO <data>.
  restore_type_int( EXPORTING reader = &1 type_descr = data_descr typekind = data_descr->type_kind CHANGING data = <data> ).
  RETURN.
END-OF-DEFINITION.

DEFINE restore_convexit.
  " &1 = convexit func, &2 = input string, &3 = output data
  TRY .
      CALL FUNCTION &1
        EXPORTING input  = &2
        IMPORTING output = &3
        EXCEPTIONS OTHERS = 1.
      IF sy-subrc IS NOT INITIAL.
        CLEAR &3.
      ENDIF.
      RETURN.
    CATCH cx_root INTO lo_exp.                           "#EC CATCH_ALL
      CLEAR &3.
      IF mv_strict_mode = abap_true.
        RAISE EXCEPTION TYPE cx_sy_move_cast_error EXPORTING previous = lo_exp.
      ELSE.
        RETURN.
      ENDIF.
  ENDTRY.
END-OF-DEFINITION.

DEFINE read_timestamp.
  " &1 = input string, &2 = output timestampl
  IF &1 IS NOT INITIAL.
    IF &1+0(1) CA '0123456789T'.
      &2 = lcl_util=>read_iso8601( &1 ).
    ELSE.
      CLEAR &2.
    ENDIF.
    IF &2 IS INITIAL AND &1+0(1) = '/'.
      &2 = lcl_util=>read_edm_datetime( &1 ).
    ENDIF.
  ELSE.
    CLEAR &2.
  ENDIF.
END-OF-DEFINITION.