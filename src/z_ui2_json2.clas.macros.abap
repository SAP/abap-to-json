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

DEFINE dump_type.
" &1 = data, &2 = type_descr, &3 = typekind, &4 = writer, &5 = convexit, &6 = name
  IF mv_extended IS INITIAL.
    dump_type_int &1 &3 &4 &5 &6.
  ELSE.
    dump_type( data = &1 type_descr = &2 convexit = &5 typekind = &3 writer = &4 name = &6 ).
  ENDIF.
END-OF-DEFINITION.

DEFINE dump_type_int.
" &1 = data, &2 = typekind, &3 = writer, &4 = convexit, &5 = name

  CASE &2.
    WHEN e_typekind-convexit.
      IF &1 IS INITIAL.
        &3->write_string( name = &5 value = `` ).
      ELSE.
        TRY.
            DATA dump_type_int_val TYPE string.
            CALL FUNCTION &4
              EXPORTING
                input  = &1
              IMPORTING
                output = dump_type_int_val
              EXCEPTIONS
                OTHERS = 1.
            IF sy-subrc IS INITIAL.
              &3->write_string( name = &5 value = dump_type_int_val ).
            ELSE.
              &3->write_null( &5 ).
            ENDIF.
          CATCH cx_root ##CATCH_ALL ##NO_HANDLER.
            &3->write_null( &5 ).
        ENDTRY.
      ENDIF.
    WHEN e_typekind-utclong.
      IF &1 IS INITIAL.
        DATA dump_type_int_its TYPE string.
        dump_type_int_its = mv_initial_ts.
        &3->write_string( name = &5 value = substring( val = dump_type_int_its off = 1 len = strlen( dump_type_int_its ) - 2 ) ).
      ELSE.
        DATA dump_type_int_utcl TYPE c LENGTH 27.
        dump_type_int_utcl = &1.
        DATA dump_type_int_sv TYPE string.
        CONCATENATE dump_type_int_utcl(10) 'T' dump_type_int_utcl+11(16) 'Z' INTO dump_type_int_sv.
        &3->write_string( name = &5 value = dump_type_int_sv ).
      ENDIF.
    WHEN e_typekind-ts_iso8601.
      IF mv_ts_as_iso8601 = c_bool-true.
        IF &1 IS INITIAL.
          dump_type_int_its = mv_initial_ts.
          &3->write_string( name = &5 value = substring( val = dump_type_int_its off = 1 len = strlen( dump_type_int_its ) - 2 ) ).
        ELSE.
          DATA dump_type_int_ts TYPE c LENGTH 14.
          dump_type_int_ts = &1.
          CONCATENATE dump_type_int_ts(4) '-' dump_type_int_ts+4(2) '-' dump_type_int_ts+6(2) 'T' dump_type_int_ts+8(2) ':' dump_type_int_ts+10(2) ':' dump_type_int_ts+12(2) 'Z' INTO dump_type_int_sv.
          &3->write_string( name = &5 value = dump_type_int_sv ).
        ENDIF.
      ELSE.
        DATA dump_type_int_tsn TYPE string.
        dump_type_int_tsn = &1.
        CONDENSE dump_type_int_tsn.
        &3->write_number( name = &5 value = dump_type_int_tsn ).
      ENDIF.
    WHEN e_typekind-tsl_iso8601.
      IF mv_ts_as_iso8601 = c_bool-true.
        IF &1 IS INITIAL.
          dump_type_int_its = mv_initial_ts.
          &3->write_string( name = &5 value = substring( val = dump_type_int_its off = 1 len = strlen( dump_type_int_its ) - 2 ) ).
        ELSE.
          DATA dump_type_int_tsl TYPE c LENGTH 22.
          dump_type_int_tsl = &1.
          CONCATENATE dump_type_int_tsl(4) '-' dump_type_int_tsl+4(2) '-' dump_type_int_tsl+6(2) 'T' dump_type_int_tsl+8(2) ':' dump_type_int_tsl+10(2) ':' dump_type_int_tsl+12(2) '.' dump_type_int_tsl+15(7) 'Z' INTO dump_type_int_sv.
          &3->write_string( name = &5 value = dump_type_int_sv ).
        ENDIF.
      ELSE.
        DATA dump_type_int_tsln TYPE string.
        dump_type_int_tsln = &1.
        CONDENSE dump_type_int_tsln.
        &3->write_number( name = &5 value = dump_type_int_tsln ).
      ENDIF.
    WHEN e_typekind-float.
      IF &1 IS INITIAL.
        &3->write_number( name = &5 value = `0` ).
      ELSE.
        DATA dump_type_int_f TYPE string.
        dump_type_int_f = &1.
        &3->write_number( name = &5 value = dump_type_int_f ).
      ENDIF.
    WHEN e_typekind-int OR e_typekind-int1 OR e_typekind-int2 OR e_typekind-packed OR e_typekind-int8.
      IF &1 IS INITIAL.
        &3->write_number( name = &5 value = `0` ).
      ELSE.
        DATA dump_type_int_n TYPE string.
        dump_type_int_n = &1.
        IF &1 LT 0.
          SHIFT dump_type_int_n RIGHT CIRCULAR.
        ELSE.
          CONDENSE dump_type_int_n.
        ENDIF.
        &3->write_number( name = &5 value = dump_type_int_n ).
      ENDIF.
    WHEN e_typekind-numc_string.
      IF &1 IS INITIAL.
        &3->write_string( name = &5 value = `` ).
      ELSE.
        DATA dump_type_int_nc TYPE string.
        dump_type_int_nc = &1.
        &3->write_string( name = &5 value = dump_type_int_nc ).
      ENDIF.
    WHEN e_typekind-num.
      IF &1 IS INITIAL.
        &3->write_number( name = &5 value = `0` ).
      ELSE.
        DATA dump_type_int_nu TYPE string.
        dump_type_int_nu = &1.
        SHIFT dump_type_int_nu LEFT DELETING LEADING '0'.
        &3->write_number( name = &5 value = dump_type_int_nu ).
      ENDIF.
    WHEN e_typekind-json.
      " raw JSON — use intermediate reader to pipe to writer
      DATA dump_type_int_jr TYPE string.
      dump_type_int_jr = &1.
      IF dump_type_int_jr IS NOT INITIAL.
        IF &5 IS NOT INITIAL.
          &3->open_member( &5 ).
        ENDIF.
        DATA(dump_type_int_rdr) = cl_json_string_reader=>create( dump_type_int_jr ).
        dump_type_int_rdr->next_node( ).
        dump_type_int_rdr->skip_node( &3 ).
        IF &5 IS NOT INITIAL.
          &3->close_member( ).
        ENDIF.
      ELSE.
        &3->write_null( &5 ).
      ENDIF.
    WHEN e_typekind-string OR e_typekind-csequence OR e_typekind-clike OR e_typekind-char.
      IF &1 IS INITIAL.
        &3->write_string( name = &5 value = `` ).
      ELSE.
        DATA dump_type_int_s TYPE string.
        dump_type_int_s = &1.
        &3->write_string( name = &5 value = dump_type_int_s ).
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
      IF &1 IS INITIAL.
        &3->write_string( name = &5 value = `` ).
      ELSE.
        DATA dump_type_int_x TYPE string.
        IF mv_hex_as_base64 IS INITIAL.
          MOVE &1 TO dump_type_int_x.
        ELSE.
          DATA dump_type_int_xraw TYPE xstring.
          dump_type_int_xraw = &1.
          dump_type_int_x = cl_http_utility=>encode_x_base64( dump_type_int_xraw ).
        ENDIF.
        &3->write_string( name = &5 value = dump_type_int_x ).
      ENDIF.
    WHEN e_typekind-bool OR e_typekind-tribool.
      IF &1 = c_bool-true.
        &3->write_boolean( name = &5 value = `true` ) ##NO_TEXT.
      ELSEIF &1 IS INITIAL AND &2 = e_typekind-tribool.
        &3->write_null( &5 ).
      ELSE.
        &3->write_boolean( name = &5 value = `false` ) ##NO_TEXT.
      ENDIF.
    WHEN e_typekind-date.
      IF &1 IS INITIAL.
        DATA dump_type_int_id TYPE string.
        dump_type_int_id = mv_initial_date.
        &3->write_string( name = &5 value = substring( val = dump_type_int_id off = 1 len = strlen( dump_type_int_id ) - 2 ) ).
      ELSE.
        CONCATENATE &1(4) '-' &1+4(2) '-' &1+6(2) INTO dump_type_int_sv.
        &3->write_string( name = &5 value = dump_type_int_sv ).
      ENDIF.
    WHEN e_typekind-time.
      IF &1 IS INITIAL.
        DATA dump_type_int_it TYPE string.
        dump_type_int_it = mv_initial_time.
        &3->write_string( name = &5 value = substring( val = dump_type_int_it off = 1 len = strlen( dump_type_int_it ) - 2 ) ).
      ELSE.
        CONCATENATE &1(2) ':' &1+2(2) ':' &1+4(2) INTO dump_type_int_sv.
        &3->write_string( name = &5 value = dump_type_int_sv ).
      ENDIF.
    WHEN e_typekind-enum.
      DATA dump_type_int_e TYPE string.
      dump_type_int_e = &1.
      &3->write_string( name = &5 value = dump_type_int_e ).
    WHEN OTHERS.
      IF &1 IS INITIAL.
        &3->write_null( &5 ).
      ELSE.
        DATA dump_type_int_o TYPE string.
        dump_type_int_o = &1.
        &3->write_string( name = &5 value = dump_type_int_o ).
      ENDIF.
  ENDCASE.

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
    CATCH cx_root INTO lo_exp. "#EC CATCH_ALL
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

