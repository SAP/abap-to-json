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
    " Append sentinel so SPLIT preserves trailing empty lines
    " (ABAP SPLIT drops trailing empty strings without it)
    SPLIT text && `@` AT |\n| INTO TABLE DATA(raw).
    DATA(sentinel_idx) = lines( raw ).
    DATA(sentinel_val) = raw[ sentinel_idx ].
    DATA(slen0)        = strlen( sentinel_val ).
    raw[ sentinel_idx ] = COND string( WHEN slen0 > 0
                                       THEN substring( val = sentinel_val len = slen0 - 1 )
                                       ELSE `` ).
    DATA total    TYPE i.
    DATA n        TYPE i.
    DATA rr       TYPE string.
    DATA rlen     TYPE i.
    DATA off      TYPE i.
    DATA indent   TYPE i.
    DATA body     TYPE string.
    DATA blen     TYPE i.
    DATA blk_ind  TYPE string.
    DATA last2c   TYPE string.
    DATA last1c   TYPE string.
    DATA ind_len  TYPE i.
    DATA pre_char TYPE string.
    DATA blk_key0 TYPE string.
    DATA bk_len   TYPE i.
    DATA line     TYPE ty_line.

    " Block-body collection work vars
    DATA blk_body     TYPE string_table.
    DATA nn           TYPE i.
    DATA bbrr         TYPE string.
    DATA bb_off       TYPE i.
    DATA bb_len       TYPE i.
    DATA bb_blank     TYPE abap_bool.

    " Block indent detection
    DATA blk_ind_col   TYPE i.
    DATA blk_stripped  TYPE string_table.
    DATA bsl           TYPE string.
    DATA bsl_off       TYPE i.
    DATA bsl_len       TYPE i.
    DATA strip_from    TYPE i.
    DATA trailing_blanks TYPE i.
    DATA tot_stripped  TYPE i.
    DATA ti            TYPE i.
    DATA content_lines TYPE i.

    " Block value assembly
    DATA blk_style TYPE string.
    DATA blk_chomp TYPE string.
    DATA blk_val   TYPE string.
    DATA li        TYPE i.
    DATA fi        TYPE i.
    DATA fl        TYPE string.
    DATA prev_blank TYPE abap_bool.
    DATA ki        TYPE i.
    DATA bv_len    TYPE i.

    total = lines( raw ).
    n     = 1.
    WHILE n <= total.
      rr   = raw[ n ].
      rlen = strlen( rr ).
      off  = 0.
      " count leading spaces; reject tab in leading whitespace
      WHILE off < rlen.
        CASE substring( val = rr off = off len = 1 ).
          WHEN ` `.
            off = off + 1.
          WHEN cl_abap_char_utilities=>horizontal_tab.
            " ponytail: cx_sy_conversion_no_number nearest concrete subclass
            RAISE EXCEPTION TYPE cx_sy_conversion_no_number
              EXPORTING value = |Tab in indentation at line { n }|.
          WHEN OTHERS.
            EXIT.
        ENDCASE.
      ENDWHILE.
      indent = off.
      body   = substring( val = rr off = off ).
      body   = strip_comment( body ).
      body   = trim_right( body ).
      IF body IS INITIAL OR body = `---`.
        n = n + 1.
        CONTINUE.
      ENDIF.
      line = VALUE ty_line( lineno = n indent = indent content = body ).
      IF body = `...`.
        line-doc_marker = abap_true.
        APPEND line TO lines.
        n = n + 1.
        CONTINUE.
      ENDIF.

      " Detect block scalar indicator: |, >, |-, |+, >-, >+
      blen    = strlen( body ).
      blk_ind = ``.
      IF blen >= 2.
        last2c = substring( val = body off = blen - 2 len = 2 ).
        IF last2c = `|-` OR last2c = `|+` OR last2c = `>-` OR last2c = `>+`.
          blk_ind = last2c.
        ENDIF.
      ENDIF.
      IF blk_ind IS INITIAL AND blen >= 1.
        last1c = substring( val = body off = blen - 1 len = 1 ).
        IF last1c = `|` OR last1c = `>`.
          blk_ind = last1c.
        ENDIF.
      ENDIF.

      IF blk_ind IS NOT INITIAL.
        ind_len  = strlen( blk_ind ).
        " pre_char: the character just before the indicator
        pre_char = COND string( WHEN blen > ind_len
                                THEN substring( val = body off = blen - ind_len - 1 len = 1 )
                                ELSE `` ).
        blk_key0 = trim_right( substring( val = body len = blen - ind_len ) ).
        bk_len   = strlen( blk_key0 ).
        " Only a mapping block header: blk_key0 must end with ':' (and indicator preceded by space)
        IF ( blen = ind_len OR pre_char = ` ` )
           AND bk_len > 0
           AND substring( val = blk_key0 off = bk_len - 1 len = 1 ) = `:`.

          " Collect raw body lines: blank OR indent > header indent
          CLEAR blk_body.
          nn = n + 1.
          WHILE nn <= total.
            bbrr   = raw[ nn ].
            bb_len = strlen( bbrr ).
            bb_off = 0.
            WHILE bb_off < bb_len AND substring( val = bbrr off = bb_off len = 1 ) = ` `.
              bb_off = bb_off + 1.
            ENDWHILE.
            bb_blank = xsdbool( bb_off = bb_len ).
            IF bb_blank = abap_true.
              APPEND bbrr TO blk_body.
              nn = nn + 1.
            ELSEIF bb_off > indent.
              APPEND bbrr TO blk_body.
              nn = nn + 1.
            ELSE.
              EXIT.
            ENDIF.
          ENDWHILE.

          " Block indent = indent of first non-blank body line
          blk_ind_col = 0.
          LOOP AT blk_body INTO bsl.
            bsl_off = 0.
            bsl_len = strlen( bsl ).
            WHILE bsl_off < bsl_len AND substring( val = bsl off = bsl_off len = 1 ) = ` `.
              bsl_off = bsl_off + 1.
            ENDWHILE.
            IF bsl_off < bsl_len.
              blk_ind_col = bsl_off.
              EXIT.
            ENDIF.
          ENDLOOP.

          " Strip block indent from each body line
          CLEAR blk_stripped.
          LOOP AT blk_body INTO bsl.
            bsl_off = 0.
            bsl_len = strlen( bsl ).
            WHILE bsl_off < bsl_len AND substring( val = bsl off = bsl_off len = 1 ) = ` `.
              bsl_off = bsl_off + 1.
            ENDWHILE.
            IF bsl_off = bsl_len.
              APPEND `` TO blk_stripped.
            ELSE.
              strip_from = COND i( WHEN bsl_off >= blk_ind_col THEN blk_ind_col ELSE bsl_off ).
              APPEND substring( val = bsl off = strip_from ) TO blk_stripped.
            ENDIF.
          ENDLOOP.

          " Count trailing blank lines
          tot_stripped   = lines( blk_stripped ).
          trailing_blanks = 0.
          ti = tot_stripped.
          WHILE ti >= 1 AND blk_stripped[ ti ] IS INITIAL.
            trailing_blanks = trailing_blanks + 1.
            ti = ti - 1.
          ENDWHILE.
          content_lines = tot_stripped - trailing_blanks.

          " Fold/join body lines
          blk_style = substring( val = blk_ind off = 0 len = 1 ).
          blk_chomp = COND string( WHEN ind_len > 1
                                   THEN substring( val = blk_ind off = 1 len = 1 )
                                   ELSE `` ).
          blk_val = ``.
          IF blk_style = `|`.
            " LITERAL: each content line + LF
            li = 1.
            WHILE li <= content_lines.
              blk_val = blk_val && blk_stripped[ li ] && cl_abap_char_utilities=>newline.
              li = li + 1.
            ENDWHILE.
          ELSE.
            " FOLDED: consecutive non-blank lines joined by space; blank line → LF
            prev_blank = abap_false.
            fi = 1.
            WHILE fi <= content_lines.
              fl = blk_stripped[ fi ].
              IF fl IS INITIAL.
                blk_val    = blk_val && cl_abap_char_utilities=>newline.
                prev_blank = abap_true.
              ELSE.
                IF fi > 1 AND prev_blank = abap_false.
                  blk_val = blk_val && ` `.
                ENDIF.
                blk_val    = blk_val && fl.
                prev_blank = abap_false.
              ENDIF.
              fi = fi + 1.
            ENDWHILE.
            IF content_lines > 0.
              blk_val = blk_val && cl_abap_char_utilities=>newline.
            ENDIF.
          ENDIF.

          " Apply chomping
          CASE blk_chomp.
            WHEN `-`.
              " STRIP: remove trailing LF
              bv_len = strlen( blk_val ).
              IF bv_len > 0 AND
                 substring( val = blk_val off = bv_len - 1 len = 1 ) = cl_abap_char_utilities=>newline.
                blk_val = substring( val = blk_val len = bv_len - 1 ).
              ENDIF.
            WHEN `+`.
              " KEEP: append trailing blank lines as extra LFs
              ki = 1.
              WHILE ki <= trailing_blanks.
                blk_val = blk_val && cl_abap_char_utilities=>newline.
                ki = ki + 1.
              ENDWHILE.
            WHEN OTHERS.
              " CLIP (default): exactly one trailing LF already emitted — nothing to do
          ENDCASE.

          line-blk_scalar_hd = abap_true.
          line-blk_value     = blk_val.
          line-content       = blk_key0.
          APPEND line TO lines.
          n = nn.
          CONTINUE.
        ELSE.
          CLEAR blk_ind.  " not a mapping block header — treat as plain content
        ENDIF.
      ENDIF.

      APPEND line TO lines.
      n = n + 1.
    ENDWHILE.
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

  METHOD trim.
    " Leading/trailing whitespace only — does NOT collapse internal spaces
    DATA(len) = strlen( val ).
    DATA(l_off) = 0.
    WHILE l_off < len AND substring( val = val off = l_off len = 1 ) = ` `.
      l_off = l_off + 1.
    ENDWHILE.
    result = trim_right( substring( val = val off = l_off ) ).
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
    DATA lv_child  TYPE ty_node_ref.
    WHILE idx <= lines( lines ) AND lines[ idx ]-indent = own_indent.
      DATA(cur) = lines[ idx ].
      IF cur-doc_marker = abap_true. idx = idx + 1. EXIT. ENDIF.
      IF cur-content CP `- *` OR cur-content = `-`. EXIT. ENDIF.
      split_key_value( EXPORTING content = cur-content lineno = cur-lineno
                       IMPORTING key = lv_key inline_value = lv_inline has_inline = lv_has ).
      idx = idx + 1.
      IF cur-blk_scalar_hd = abap_true.
        lv_child = lcl_tree=>new_scalar( value = cur-blk_value ).
      ELSE.
        lv_child = value_or_block( EXPORTING lines = lines own_indent = own_indent
                                             has_inline = lv_has inline_value = lv_inline
                                   CHANGING  idx = idx ).
      ENDIF.
      lcl_tree=>add_child( node = node key = lv_key child = lv_child ).
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
      " trim leading/trailing only — preserve internal spaces in quoted scalars
      DATA(trimmed) = lcl_scanner=>trim( inline_value ).
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
      " ponytail: last-char termination check; full inner walk if strict mode needed
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
        DATA(cv) = substring( val = src off = i len = 1 ).
        IF cv = `\` AND i + 1 < slen.
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
          res = res && cv.
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
    DATA(body)  = substring( val = raw off = 1 len = len - 2 ).
    DATA tokens TYPE string_table.
    DATA cur_tok TYPE string.
    DATA depth   TYPE i.
    DATA in_sq   TYPE abap_bool.
    DATA in_dq   TYPE abap_bool.
    DATA(blen)   = strlen( body ).
    DATA j       TYPE i.

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

    " Declare map-branch work vars outside loop — ABAP DATA VALUE init is method-scope, not loop-scope
    DATA entry_key TYPE string.
    DATA entry_val TYPE string.
    DATA k         TYPE i.
    DATA fc        TYPE abap_bool.
    DATA tsq       TYPE abap_bool.
    DATA tdq       TYPE abap_bool.
    DATA tdepth    TYPE i.

    " process each token
    LOOP AT tokens INTO DATA(tok).
      DATA(trimmed) = lcl_scanner=>trim( tok ).
      CHECK trimmed IS NOT INITIAL.
      IF lv_is_map = abap_true.
        " reset per-entry work vars each iteration — DATA VALUE is method-scope, not loop-scope
        CLEAR: entry_key, entry_val, fc, tsq, tdq, tdepth.
        k = 0.
        " find ':' at depth 0, not in quotes
        DATA(tlen) = strlen( trimmed ).
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
                    DATA(ks) = k + 2.
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
        " dispatch map value: nested collection or scalar
        DATA(ev_trim) = lcl_scanner=>trim( entry_val ).
        DATA child_node TYPE ty_node_ref.
        IF strlen( ev_trim ) > 0 AND
           ( substring( val = ev_trim off = 0 len = 1 ) = `{` OR
             substring( val = ev_trim off = 0 len = 1 ) = `[` ).
          child_node = parse_flow( ev_trim ).
        ELSE.
          DATA ev TYPE string.
          DATA en TYPE abap_bool.
          resolve_scalar( EXPORTING raw = ev_trim IMPORTING value = ev is_null = en ).
          child_node = lcl_tree=>new_scalar( value = ev is_null = en ).
        ENDIF.
        lcl_tree=>add_child( node = node key = lcl_scanner=>trim( entry_key ) child = child_node ).
      ELSE.
        " sequence item: nested collection or scalar
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
