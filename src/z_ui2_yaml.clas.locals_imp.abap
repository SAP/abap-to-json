*"* use this source file for the implementation part of
*"* local helper classes

CLASS c_node IMPLEMENTATION.
ENDCLASS.

CLASS lcl_node_ref IMPLEMENTATION.
ENDCLASS.

CLASS lcl_tree IMPLEMENTATION.
  METHOD new_scalar.
    node = NEW lcl_node_ref( ).
    node->node = VALUE ty_node( kind = c_node=>scalar value = value is_null = is_null ).
  ENDMETHOD.
  METHOD new_collection.
    node = NEW lcl_node_ref( ).
    node->node-kind = kind.
  ENDMETHOD.
  METHOD add_child.
    APPEND VALUE ty_child( key = key node = child ) TO node->children.
  ENDMETHOD.
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

  METHOD parse.
    DATA(idx) = 1.
    IF lines IS INITIAL.
      root = lcl_tree=>new_scalar( value = `` is_null = abap_true ).
      RETURN.
    ENDIF.
    root = parse_block( EXPORTING lines = lines CHANGING idx = idx ).
  ENDMETHOD.

  METHOD parse_block.
    DATA(own)   = lines[ idx ]-indent.
    DATA(first) = lines[ idx ]-content.
    IF first CP `- *` OR first = `-`.
      node = parse_sequence( EXPORTING lines = lines own_indent = own CHANGING idx = idx ).
    ELSE.
      DATA lv_is_mapping TYPE abap_bool.
      split_key_value( EXPORTING content = first lineno = lines[ idx ]-lineno
                       IMPORTING is_mapping = lv_is_mapping ).
      IF lv_is_mapping = abap_true.
        node = parse_mapping( EXPORTING lines = lines own_indent = own CHANGING idx = idx ).
      ELSE.
        node = lcl_tree=>new_scalar( value = first ).
        idx = idx + 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD parse_mapping.
    node = lcl_tree=>new_collection( c_node=>mapping ).
    DATA lv_key    TYPE string.
    DATA lv_inline TYPE string.
    DATA lv_has    TYPE abap_bool.
    WHILE idx <= lines( lines ) AND lines[ idx ]-indent = own_indent.
      DATA(cur) = lines[ idx ].
      IF cur-doc_marker = abap_true. idx = idx + 1. EXIT. ENDIF.
      IF cur-content CP `- *` OR cur-content = `-`. EXIT. ENDIF.
      split_key_value( EXPORTING content = cur-content lineno = cur-lineno
                       IMPORTING key = lv_key inline_value = lv_inline has_inline = lv_has ).
      idx = idx + 1.
      DATA(child) = value_or_block( EXPORTING lines = lines own_indent = own_indent
                                              has_inline = lv_has inline_value = lv_inline
                                    CHANGING  idx = idx ).
      lcl_tree=>add_child( node = node key = lv_key child = child ).
    ENDWHILE.
    " bad-dedent: next line is deeper than own but wasn't consumed
    IF idx <= lines( lines ) AND lines[ idx ]-indent > own_indent.
      RAISE EXCEPTION TYPE cx_sy_conversion_no_number
        EXPORTING value = |Bad indentation at line { lines[ idx ]-lineno }|.
    ENDIF.
  ENDMETHOD.

  METHOD parse_sequence.
    node = lcl_tree=>new_collection( c_node=>sequence ).
    WHILE idx <= lines( lines ) AND lines[ idx ]-indent = own_indent
          AND ( lines[ idx ]-content CP `- *` OR lines[ idx ]-content = `-` ).
      DATA(c)    = lines[ idx ]-content.
      DATA(rest) = COND string( WHEN c = `-` THEN ``
                                ELSE substring( val = c off = 2 ) ).
      DATA(child) = parse_seq_item( EXPORTING lines = lines own_indent = own_indent rest = rest
                                    CHANGING  idx = idx ).
      lcl_tree=>add_child( node = node child = child ).
    ENDWHILE.
    " bad-dedent: next line is deeper than own but wasn't consumed
    IF idx <= lines( lines ) AND lines[ idx ]-indent > own_indent.
      RAISE EXCEPTION TYPE cx_sy_conversion_no_number
        EXPORTING value = |Bad indentation at line { lines[ idx ]-lineno }|.
    ENDIF.
  ENDMETHOD.

  METHOD parse_seq_item.
    " advance past the dash line
    idx = idx + 1.
    " Build virtual sub-lines for this item:
    "   first entry: rest text at indent = own_indent + 2
    "   following:   continuation lines with indent > own_indent, copied as-is
    DATA sub TYPE ty_lines.
    IF rest IS NOT INITIAL.
      APPEND VALUE ty_line( lineno = 0 indent = own_indent + 2 content = rest ) TO sub.
    ENDIF.
    WHILE idx <= lines( lines ) AND lines[ idx ]-indent > own_indent.
      APPEND lines[ idx ] TO sub.
      idx = idx + 1.
    ENDWHILE.
    IF sub IS INITIAL.
      " bare `-` with nothing below → null scalar
      node = lcl_tree=>new_scalar( value = `` is_null = abap_true ).
      RETURN.
    ENDIF.
    DATA(sub_idx) = 1.
    node = parse_block( EXPORTING lines = sub CHANGING idx = sub_idx ).
  ENDMETHOD.

  METHOD split_key_value.
    DATA len TYPE i.
    DATA i   TYPE i.
    len = strlen( content ).
    WHILE i < len.
      DATA(ch) = substring( val = content off = i len = 1 ).
      IF ch = `:`.
        DATA(next_pos) = i + 1.
        IF next_pos >= len.
          " key: (nothing after colon)
          key          = substring( val = content len = i ).
          inline_value = ``.
          has_inline   = abap_false.
          is_mapping   = abap_true.
          RETURN.
        ENDIF.
        DATA(next_ch) = substring( val = content off = next_pos len = 1 ).
        IF next_ch = ` `.
          key = substring( val = content len = i ).
          DATA(val_off) = i + 2.
          IF val_off < len.
            inline_value = substring( val = content off = val_off ).
          ENDIF.
          has_inline = abap_true.
          is_mapping = abap_true.
          RETURN.
        ENDIF.
      ENDIF.
      i = i + 1.
    ENDWHILE.
    " no valid key separator found — plain scalar
    key        = content.
    has_inline = abap_false.
    is_mapping = abap_false.
  ENDMETHOD.

  METHOD value_or_block.
    IF has_inline = abap_true.
      node = lcl_tree=>new_scalar( value = inline_value ).
    ELSEIF idx <= lines( lines ) AND lines[ idx ]-indent > own_indent.
      node = parse_block( EXPORTING lines = lines CHANGING idx = idx ).
    ELSE.
      node = lcl_tree=>new_scalar( value = `` is_null = abap_true ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_typed_mapper IMPLEMENTATION.
ENDCLASS.

CLASS lcl_gen_mapper IMPLEMENTATION.
ENDCLASS.

CLASS lcl_emitter IMPLEMENTATION.
ENDCLASS.
