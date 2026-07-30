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
        DATA lv_bv TYPE string.
        DATA lv_bn TYPE abap_bool.
        resolve_scalar( EXPORTING raw = first IMPORTING value = lv_bv is_null = lv_bn ).
        node = lcl_tree=>new_scalar( value = lv_bv is_null = lv_bn ).
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
    " Quote-aware colon scan: skip ':' inside single/double quotes
    DATA len   TYPE i.
    DATA i     TYPE i.
    DATA in_sq TYPE abap_bool VALUE abap_false.
    DATA in_dq TYPE abap_bool VALUE abap_false.
    len = strlen( content ).
    WHILE i < len.
      DATA(ch) = substring( val = content off = i len = 1 ).
      IF in_sq = abap_true.
        " escaped '' inside single-quoted string
        IF ch = `'` AND i + 1 < len AND substring( val = content off = i + 1 len = 1 ) = `'`.
          i = i + 2.
          CONTINUE.
        ENDIF.
        IF ch = `'`. in_sq = abap_false. ENDIF.
      ELSEIF in_dq = abap_true.
        IF ch = `\` AND i + 1 < len. i = i + 2. CONTINUE. ENDIF.
        IF ch = `"`. in_dq = abap_false. ENDIF.
      ELSE.
        CASE ch.
          WHEN `'`. in_sq = abap_true.
          WHEN `"`. in_dq = abap_true.
          WHEN `:`.
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
        ENDCASE.
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
      DATA(trimmed) = condense( inline_value ).
      IF strlen( trimmed ) > 0 AND
         ( substring( val = trimmed off = 0 len = 1 ) = `{` OR
           substring( val = trimmed off = 0 len = 1 ) = `[` ).
        node = parse_flow( trimmed ).
      ELSE.
        DATA lv_val  TYPE string.
        DATA lv_null TYPE abap_bool.
        resolve_scalar( EXPORTING raw = trimmed IMPORTING value = lv_val is_null = lv_null ).
        node = lcl_tree=>new_scalar( value = lv_val is_null = lv_null ).
      ENDIF.
    ELSEIF idx <= lines( lines ) AND lines[ idx ]-indent > own_indent.
      node = parse_block( EXPORTING lines = lines CHANGING idx = idx ).
    ELSE.
      node = lcl_tree=>new_scalar( value = `` is_null = abap_true ).
    ENDIF.
  ENDMETHOD.

  METHOD resolve_scalar.
    is_null = abap_false.
    value   = ``.
    DATA(len) = strlen( raw ).
    IF len = 0 OR raw = `~`.
      is_null = abap_true.
      RETURN.
    ENDIF.
    DATA(first) = substring( val = raw off = 0 len = 1 ).
    IF first = `'`.
      " single-quoted: strip outer quotes, '' → '
      IF len < 2 OR substring( val = raw off = len - 1 len = 1 ) <> `'`.
        RAISE EXCEPTION TYPE cx_sy_conversion_no_number
          EXPORTING value = `Unterminated single-quoted scalar`.
      ENDIF.
      DATA(inner) = substring( val = raw off = 1 len = len - 2 ).
      REPLACE ALL OCCURRENCES OF `''` IN inner WITH `'`.
      value = inner.
    ELSEIF first = `"`.
      " double-quoted: strip outer quotes, process escape sequences
      IF len < 2 OR substring( val = raw off = len - 1 len = 1 ) <> `"`.
        RAISE EXCEPTION TYPE cx_sy_conversion_no_number
          EXPORTING value = `Unterminated double-quoted scalar`.
      ENDIF.
      DATA(src)  = substring( val = raw off = 1 len = len - 2 ).
      DATA(res)  = ``.
      DATA(i)    = 0.
      DATA(slen) = strlen( src ).
      WHILE i < slen.
        DATA(c) = substring( val = src off = i len = 1 ).
        IF c = `\` AND i + 1 < slen.
          DATA(esc) = substring( val = src off = i + 1 len = 1 ).
          CASE esc.
            WHEN `n`.  res = res && cl_abap_char_utilities=>newline.        i = i + 2.
            WHEN `t`.  res = res && cl_abap_char_utilities=>horizontal_tab. i = i + 2.
            WHEN `"`.  res = res && `"`.  i = i + 2.
            WHEN `\`.  res = res && `\`.  i = i + 2.
            WHEN `u`.
              IF i + 5 < slen.
                DATA(hex4) = substring( val = src off = i + 2 len = 4 ).
                res = res && cl_abap_conv_in_ce=>uccp( hex4 ).
                i = i + 6.
              ELSE.
                res = res && `\` && esc. i = i + 2.
              ENDIF.
            WHEN OTHERS. res = res && esc. i = i + 2.
          ENDCASE.
        ELSE.
          res = res && c.
          i = i + 1.
        ENDIF.
      ENDWHILE.
      value = res.
    ELSE.
      " plain scalar — as-is
      value = raw.
    ENDIF.
  ENDMETHOD.

  METHOD parse_flow.
    " Tokenize a flow collection {k:v,...} or [v,...] respecting nesting + quotes
    DATA(len) = strlen( raw ).
    IF len < 2.
      RAISE EXCEPTION TYPE cx_sy_conversion_no_number
        EXPORTING value = `Invalid flow collection`.
    ENDIF.
    DATA(opener) = substring( val = raw off = 0 len = 1 ).
    DATA(closer) = substring( val = raw off = len - 1 len = 1 ).
    DATA lv_is_map TYPE abap_bool.
    IF opener = `{` AND closer = `}`.
      lv_is_map = abap_true.
      node = lcl_tree=>new_collection( c_node=>mapping ).
    ELSEIF opener = `[` AND closer = `]`.
      node = lcl_tree=>new_collection( c_node=>sequence ).
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_conversion_no_number
        EXPORTING value = `Invalid flow collection`.
    ENDIF.

    " split inner body on ',' at depth 0, respecting quotes and nesting
    DATA(body) = substring( val = raw off = 1 len = len - 2 ).
    DATA tokens    TYPE string_table.
    DATA cur_tok   TYPE string VALUE ``.
    DATA depth     TYPE i VALUE 0.
    DATA in_sq     TYPE abap_bool VALUE abap_false.
    DATA in_dq     TYPE abap_bool VALUE abap_false.
    DATA(blen)     = strlen( body ).
    DATA j         TYPE i VALUE 0.
    WHILE j < blen.
      DATA(bc) = substring( val = body off = j len = 1 ).
      IF in_sq = abap_true.
        cur_tok = cur_tok && bc.
        IF bc = `'` AND j + 1 < blen AND substring( val = body off = j + 1 len = 1 ) = `'`.
          cur_tok = cur_tok && `'`.
          j = j + 2.
          CONTINUE.
        ENDIF.
        IF bc = `'`. in_sq = abap_false. ENDIF.
      ELSEIF in_dq = abap_true.
        IF bc = `\` AND j + 1 < blen.
          cur_tok = cur_tok && bc && substring( val = body off = j + 1 len = 1 ).
          j = j + 2.
          CONTINUE.
        ENDIF.
        cur_tok = cur_tok && bc.
        IF bc = `"`. in_dq = abap_false. ENDIF.
      ELSE.
        CASE bc.
          WHEN `'`. in_sq = abap_true. cur_tok = cur_tok && bc.
          WHEN `"`. in_dq = abap_true. cur_tok = cur_tok && bc.
          WHEN `{` OR `[`. depth = depth + 1. cur_tok = cur_tok && bc.
          WHEN `}` OR `]`. depth = depth - 1. cur_tok = cur_tok && bc.
          WHEN `,`.
            IF depth = 0.
              APPEND cur_tok TO tokens.
              CLEAR cur_tok.
            ELSE.
              cur_tok = cur_tok && bc.
            ENDIF.
          WHEN OTHERS.
            cur_tok = cur_tok && bc.
        ENDCASE.
      ENDIF.
      j = j + 1.
    ENDWHILE.
    APPEND cur_tok TO tokens.

    " process each token
    LOOP AT tokens INTO DATA(tok).
      DATA(trimmed) = condense( tok ).
      CHECK trimmed IS NOT INITIAL.
      IF lv_is_map = abap_true.
        " find ':' at depth 0, not in quotes
        DATA entry_key TYPE string VALUE ``.
        DATA entry_val TYPE string VALUE ``.
        DATA k         TYPE i VALUE 0.
        DATA(tlen)     = strlen( trimmed ).
        DATA fc        TYPE abap_bool VALUE abap_false.
        DATA tsq       TYPE abap_bool VALUE abap_false.
        DATA tdq       TYPE abap_bool VALUE abap_false.
        DATA tdepth    TYPE i VALUE 0.
        WHILE k < tlen.
          DATA(tc) = substring( val = trimmed off = k len = 1 ).
          IF tsq = abap_true.
            IF tc = `'` AND k + 1 < tlen AND substring( val = trimmed off = k + 1 len = 1 ) = `'`.
              k = k + 2. CONTINUE.
            ENDIF.
            IF tc = `'`. tsq = abap_false. ENDIF.
          ELSEIF tdq = abap_true.
            IF tc = `\` AND k + 1 < tlen. k = k + 2. CONTINUE. ENDIF.
            IF tc = `"`. tdq = abap_false. ENDIF.
          ELSE.
            CASE tc.
              WHEN `'`. tsq = abap_true.
              WHEN `"`. tdq = abap_true.
              WHEN `{` OR `[`. tdepth = tdepth + 1.
              WHEN `}` OR `]`. tdepth = tdepth - 1.
              WHEN `:`.
                IF tdepth = 0.
                  IF k + 1 >= tlen OR substring( val = trimmed off = k + 1 len = 1 ) = ` `.
                    entry_key = substring( val = trimmed len = k ).
                    DATA ks TYPE i.
                    ks = k + 2.
                    IF ks < tlen.
                      entry_val = substring( val = trimmed off = ks ).
                    ENDIF.
                    fc = abap_true.
                    EXIT.
                  ENDIF.
                ENDIF.
            ENDCASE.
          ENDIF.
          k = k + 1.
        ENDWHILE.
        IF fc = abap_false.
          entry_key = trimmed.
        ENDIF.
        DATA ev TYPE string.
        DATA en TYPE abap_bool.
        resolve_scalar( EXPORTING raw = condense( entry_val ) IMPORTING value = ev is_null = en ).
        lcl_tree=>add_child( node = node key = condense( entry_key )
                             child = lcl_tree=>new_scalar( value = ev is_null = en ) ).
      ELSE.
        " sequence item
        DATA(fc1) = substring( val = trimmed off = 0 len = 1 ).
        IF fc1 = `{` OR fc1 = `[`.
          lcl_tree=>add_child( node = node child = parse_flow( trimmed ) ).
        ELSE.
          DATA sv TYPE string.
          DATA sn TYPE abap_bool.
          resolve_scalar( EXPORTING raw = trimmed IMPORTING value = sv is_null = sn ).
          lcl_tree=>add_child( node = node child = lcl_tree=>new_scalar( value = sv is_null = sn ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_typed_mapper IMPLEMENTATION.
ENDCLASS.

CLASS lcl_gen_mapper IMPLEMENTATION.
ENDCLASS.

CLASS lcl_emitter IMPLEMENTATION.
ENDCLASS.
