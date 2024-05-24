*"* local class implementation for public class
*"* use this source file for the implementation part of
*"* local helper classes

DEFINE escape_json.
  " => 7.31
*  &2 = escape( val = &1 format = cl_abap_format=>e_json_string ).

  lcl_util=>_escape( EXPORTING in = &1 IMPORTING out = &2 ).

* <= 7.31 (not full scope of escaping is supported)
*  &2 = &1.
*
**  replace all occurrences of regex `[\\"]` in &1 with `\\$0`. <-- this is slower than 2 plain replaces
*  REPLACE ALL OCCURRENCES OF '\' IN &2 WITH '\\'.
*  REPLACE ALL OCCURRENCES OF '"' IN &2 WITH '\"'.
*
*  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf          IN &2 WITH '\r\n'.
*  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN &2 WITH '\n'.
*  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN &2 WITH '\t'.
END-OF-DEFINITION.

DEFINE is_compressable.
  IF mv_compress EQ abap_false.
    &3 = abap_false.
  ELSEIF mv_extended IS INITIAL.
    &3 = abap_true.
  ELSE.
    &3 = is_compressable( type_descr = &1 name = &2 ).
  ENDIF.
END-OF-DEFINITION.

DEFINE dump_type.
  IF mv_extended IS INITIAL.
    dump_type_int &1 &3 &4 &5.
  ELSE.
    &4 = dump_type( data = &1 type_descr = &2 typekind = &3 convexit = &5 ).
  ENDIF.
END-OF-DEFINITION.

DEFINE xstring_to_string_int.
  IF mv_hex_as_base64 IS INITIAL.
    MOVE &1 TO &2.
  ELSE.
    &2 = xstring_to_string( &1 ).
  ENDIF.
END-OF-DEFINITION.

DEFINE string_to_xstring_int.
  IF mv_hex_as_base64 IS INITIAL.
    MOVE &1 TO &2.
  ELSE.
    string_to_xstring( EXPORTING in = &1 CHANGING out = &2 ).
  ENDIF.
END-OF-DEFINITION.

DEFINE format_list_output.
  IF mv_format_output EQ abap_true AND &2 IS NOT INITIAL.
    CONCATENATE ',' lv_indent INTO lv_lb.
    CONCATENATE LINES OF &2 INTO &4 SEPARATED BY lv_lb.
    CONCATENATE &1 lv_indent &4 indent &3 INTO &4.
  ELSE.
    CONCATENATE LINES OF &2 INTO &4 SEPARATED BY ','.
    CONCATENATE &1 &4 &3 INTO &4.
  ENDIF.
END-OF-DEFINITION. " format_list_output

DEFINE dump_type_int.

  CASE &2.
    WHEN e_typekind-convexit.
      IF &1 IS INITIAL.
        &3 = `""`.
      ELSE.
        TRY.
            DATA: char128 TYPE c LENGTH 128.
            CALL FUNCTION &4
              EXPORTING
                input  = &1
              IMPORTING
                output = char128
              EXCEPTIONS
                OTHERS = 1.
            IF sy-subrc IS INITIAL.
              CONCATENATE '"' char128 '"' INTO &3.
            ENDIF.
          CATCH cx_root ##CATCH_ALL ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    WHEN e_typekind-utclong.
      IF &1 IS INITIAL.
        &3 = mv_initial_ts.
      ELSE.
        DATA: utcl TYPE c LENGTH 27.
        utcl = &1.
        CONCATENATE '"' utcl(10) 'T' utcl+11(16) 'Z"'  INTO &3.
      ENDIF.
    WHEN e_typekind-ts_iso8601.
      IF &1 IS INITIAL.
        &3 = mv_initial_ts.
      ELSE.
        DATA: ts TYPE c LENGTH 14.
        ts = &1.
        CONCATENATE '"' ts(4) '-' ts+4(2) '-' ts+6(2) 'T' ts+8(2) ':' ts+10(2) ':' ts+12(2) 'Z"'  INTO &3.
      ENDIF.
    WHEN e_typekind-tsl_iso8601.
      IF &1 IS INITIAL.
        &3 = mv_initial_ts.
      ELSE.
        DATA: tsl TYPE c LENGTH 22.
        tsl = &1.
        CONCATENATE '"' tsl(4) '-' tsl+4(2) '-' tsl+6(2) 'T' tsl+8(2) ':' tsl+10(2) ':' tsl+12(2) '.' tsl+15(7) 'Z"'  INTO &3.
      ENDIF.
    WHEN e_typekind-float.
      IF &1 IS INITIAL.
        &3 = `0`.
      ELSE.
        &3 = &1.
      ENDIF.
    WHEN e_typekind-int OR e_typekind-int1 OR e_typekind-int2 OR e_typekind-packed OR e_typekind-int8.
      IF &1 IS INITIAL.
        &3 = `0`.
      ELSE.
        &3 = &1.
        IF &1 LT 0.
          SHIFT &3 RIGHT CIRCULAR.
        ELSE.
          CONDENSE &3.
        ENDIF.
     ENDIF.
    WHEN e_typekind-numc_string.
      IF &1 IS INITIAL.
        &3 = `""`.
      ELSE.
        CONCATENATE '"' &1 '"' INTO &3.
      ENDIF.
    WHEN e_typekind-num.
      IF &1 IS INITIAL.
        &3 = `0`.
      ELSE.
        &3 = &1.
        SHIFT &3 LEFT DELETING LEADING '0'.
      ENDIF.
    WHEN e_typekind-json.
      &3 = &1.
    WHEN e_typekind-string OR e_typekind-csequence OR e_typekind-clike OR e_typekind-char.
      IF &1 IS INITIAL.
        &3 = `""`.
      ELSE.
        escape_json &1 &3.
        CONCATENATE '"' &3 '"' INTO &3.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
      IF &1 IS INITIAL.
        &3 = `""`.
      ELSE.
        xstring_to_string_int &1 &3.
        CONCATENATE '"' &3 '"' INTO &3.
      ENDIF.
    WHEN e_typekind-bool OR e_typekind-tribool.
      IF &1 EQ c_bool-true.
        &3 = `true`.                                        "#EC NOTEXT
      ELSEIF &1 IS INITIAL AND &2 EQ e_typekind-tribool.
        &3 = `null`.                                        "#EC NOTEXT
      ELSE.
        &3 = `false`.                                       "#EC NOTEXT
      ENDIF.
    WHEN e_typekind-date.
      IF &1 IS INITIAL.
        &3 = mv_initial_date.
      ELSE.
        CONCATENATE '"' &1(4) '-' &1+4(2) '-' &1+6(2) '"' INTO &3.
      ENDIF.
    WHEN e_typekind-time.
      IF &1 IS INITIAL.
        &3 = mv_initial_time.
      ELSE.
        CONCATENATE '"' &1(2) ':' &1+2(2) ':' &1+4(2) '"' INTO &3.
      ENDIF.
    WHEN e_typekind-enum.
      &3 = &1.
      CONCATENATE '"' &3 '"' INTO &3.
    WHEN OTHERS.
      IF &1 IS INITIAL.
        &3 = `null`.                                        "#EC NOTEXT
      ELSE.
        &3 = &1.
      ENDIF.
  ENDCASE.

END-OF-DEFINITION.

DEFINE format_name.
  CASE &2.
    WHEN pretty_mode-camel_case.
      &3 = pretty_name( &1 ).
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

DEFINE restore_reference.
  CREATE DATA data TYPE HANDLE &1.
  ASSIGN data->* TO <data>.
  restore_type( EXPORTING json = json length = length type_descr = &1 CHANGING offset = offset data = <data> ).
END-OF-DEFINITION.

DEFINE restore_reference_ex. " &1 - data, &2 - type_descr
  ref_descr ?= &2.
  data_descr ?= ref_descr->get_referenced_type( ).
  IF &1 IS INITIAL.
    IF data_descr->type_kind EQ data_descr->typekind_data. " REF TO DATA
      generate_int_ex( EXPORTING json = json length = length CHANGING offset = offset data = &1 ).
      RETURN.
    ELSE.
      CREATE DATA &1 TYPE HANDLE data_descr.
    ENDIF.
  ENDIF.
  data_ref ?= &1.
  ASSIGN data_ref->* TO <data>.
  restore_type( EXPORTING json = json length = length type_descr = data_descr CHANGING data = <data> offset = offset ).
END-OF-DEFINITION.

DEFINE throw_error.
  " RAISE EXCEPTION TYPE cx_sy_move_cast_error EXPORTING source_typename = |({ offset })|.
  RAISE EXCEPTION TYPE cx_sy_move_cast_error.
END-OF-DEFINITION.

DEFINE while_offset_cs.

*  >= 7.02 alternative
  offset = find_any_not_of( val = json sub = &1 off = offset )  ##NO_TEXT.
  IF offset EQ -1.
    offset = length.
  ENDIF.
*  >= 7.02

* < 7.02
*  WHILE offset < length.
*    FIND FIRST OCCURRENCE OF json+offset(1) IN &1.
*    IF sy-subrc IS NOT INITIAL.
*      EXIT.
*    ENDIF.
*    offset = offset + 1.
*  ENDWHILE.
* < 7.02

END-OF-DEFINITION.

DEFINE while_offset_not_cs.

*  >= 7.02 alternative
  offset = find_any_of( val = &2 sub = &1 off = offset ).
  IF offset EQ -1.
    offset = length.
  ENDIF.
*  >= 7.02

* < 7.02
*  WHILE offset < length.
*    FIND FIRST OCCURRENCE OF &2+offset(1) IN &1.
*    IF sy-subrc IS INITIAL.
*      EXIT.
*    ENDIF.
*    offset = offset + 1.
*  ENDWHILE.
* < 7.02

END-OF-DEFINITION.

DEFINE eat_white.
  while_offset_cs sv_white_space.
  IF offset GE length.
    throw_error.
  ENDIF.
END-OF-DEFINITION.

DEFINE eat_name.

  IF json+offset(1) NE '"'.
    throw_error.
  ENDIF.

  offset = mark = offset + 1.
  IF mark < length AND json+mark(1) EQ '"'.
    CLEAR &1.
  ELSE.
    FIND FIRST OCCURRENCE OF '"' IN SECTION OFFSET offset OF json MATCH OFFSET offset.
    IF sy-subrc IS NOT INITIAL.
      throw_error.
    ENDIF.
    pos = offset - 1.
    IF json+pos(1) EQ '\'. " escaped quotes inside -> fallback on advanced processing
      lcl_util=>read_string( EXPORTING json = json mark = mark CHANGING text = &1 offset = offset ).
    ELSE.
      match = offset - mark.
      &1 = json+mark(match).
    ENDIF.
  ENDIF.
  offset = offset + 1.

END-OF-DEFINITION.

DEFINE eat_number.
  mark   = offset.
  while_offset_cs '0123456789+-eE.'.
  match = offset - mark.
  &1 = json+mark(match).
END-OF-DEFINITION.

DEFINE eat_bool_string.
  mark   = offset.
  while_offset_cs 'aeflnrstu'.
  match = offset - mark.
END-OF-DEFINITION.

DEFINE eat_bool.
  eat_bool_string.
  IF json+mark(match) EQ 'true'  ##NO_TEXT.
    &1 = c_bool-true.
  ELSEIF json+mark(match) EQ 'false'  ##NO_TEXT.
    IF type_descr IS BOUND AND mv_bool_3state CS type_descr->absolute_name.
      &1 = c_tribool-false.
    ELSE.
      &1 = c_bool-false.
    ENDIF.
  ELSEIF json+mark(match) EQ 'null'  ##NO_TEXT.
    CLEAR &1.
  ENDIF.
END-OF-DEFINITION.

DEFINE eat_char.
  IF offset < length AND json+offset(1) EQ &1.
    offset = offset + 1.
  ELSE.
    throw_error.
  ENDIF.
END-OF-DEFINITION.

DEFINE create_regexp.
  " first try PCRE
  TRY.
      CALL METHOD cl_abap_regex=>('CREATE_PCRE')
        EXPORTING
          pattern = &2
        RECEIVING
          regex   = &1.                                     "#EC NOTEXT
    CATCH cx_sy_dyn_call_error.
      CREATE OBJECT &1
        EXPORTING
          pattern = &2 ##REGEX_POSIX ##NO_TEXT.             "#EC NOTEXT
  ENDTRY.
END-OF-DEFINITION.
