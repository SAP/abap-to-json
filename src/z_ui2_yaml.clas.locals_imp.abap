*"* use this source file for the implementation part of
*"* local helper classes

CLASS c_node IMPLEMENTATION.
ENDCLASS.

CLASS lcl_node_ref IMPLEMENTATION.
ENDCLASS.

CLASS lcl_scanner IMPLEMENTATION.

  METHOD scan.
    SPLIT text AT |\n| INTO TABLE DATA(raw).
    LOOP AT raw INTO DATA(r).
      DATA(n) = sy-tabix.
      " count leading spaces; reject tab in leading whitespace
      DATA(off) = 0.
      WHILE off < strlen( r ).
        CASE r+off(1).
          WHEN ` `.
            off = off + 1.
          WHEN cl_abap_char_utilities=>horizontal_tab.
            " ponytail: cx_sy_conversion_no_number is the nearest concrete subclass
            " of cx_sy_conversion_error available on ER1; test catches the parent
            RAISE EXCEPTION TYPE cx_sy_conversion_no_number
              EXPORTING value = |Tab in indentation at line { n }|.
          WHEN OTHERS.
            EXIT.
        ENDCASE.
      ENDWHILE.
      DATA(indent) = off.
      DATA(body)   = r+off.
      body = strip_comment( body ).
      body = trim_right( body ).
      CHECK body IS NOT INITIAL AND body <> `---`.
      DATA(line) = VALUE ty_line( lineno = n indent = indent content = body ).
      IF body = `...`.
        line-doc_marker = abap_true.
      ENDIF.
      APPEND line TO lines.
    ENDLOOP.
  ENDMETHOD.

  METHOD strip_comment.
    " '#' at pos 0 or preceded by whitespace -- strip from there
    " Quoted-# awareness deferred to Task 5
    ##REGEX_POSIX
    FIND FIRST OCCURRENCE OF REGEX `(^|\s)#` IN body MATCH OFFSET DATA(mo).
    IF sy-subrc = 0.
      result = body(mo).
    ELSE.
      result = body.
    ENDIF.
  ENDMETHOD.

  METHOD trim_right.
    result = body.
    WHILE strlen( result ) > 0
      AND substring( val = result off = strlen( result ) - 1 len = 1 ) = ` `.
      result = substring( val = result len = strlen( result ) - 1 ).
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_parser IMPLEMENTATION.
ENDCLASS.

CLASS lcl_typed_mapper IMPLEMENTATION.
ENDCLASS.

CLASS lcl_gen_mapper IMPLEMENTATION.
ENDCLASS.

CLASS lcl_emitter IMPLEMENTATION.
ENDCLASS.
