*"* use this source file for your ABAP unit test classes

CLASS ltc_smoke DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS version_is_one FOR TESTING.
ENDCLASS.

CLASS ltc_smoke IMPLEMENTATION.
  METHOD version_is_one.
    cl_abap_unit_assert=>assert_equals( act = z_ui2_yaml=>version exp = 1 ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_scanner DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS counts_indent          FOR TESTING.
    METHODS strips_trailing_comment FOR TESTING.
    METHODS keeps_hash_in_value    FOR TESTING.
    METHODS drops_blank_and_comment FOR TESTING.
    METHODS rejects_tab_indent     FOR TESTING.
ENDCLASS.

CLASS ltc_scanner IMPLEMENTATION.
  METHOD counts_indent.
    DATA(l) = lcl_scanner=>scan( |a: 1\n  b: 2| ).
    cl_abap_unit_assert=>assert_equals( act = l[ 1 ]-indent exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = l[ 2 ]-indent exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = l[ 2 ]-content exp = `b: 2` ).
  ENDMETHOD.
  METHOD strips_trailing_comment.
    DATA(l) = lcl_scanner=>scan( |a: 1  # note| ).
    cl_abap_unit_assert=>assert_equals( act = l[ 1 ]-content exp = `a: 1` ).
  ENDMETHOD.
  METHOD keeps_hash_in_value.
    DATA(l) = lcl_scanner=>scan( |a: v#alue| ).
    cl_abap_unit_assert=>assert_equals( act = l[ 1 ]-content exp = `a: v#alue` ).
  ENDMETHOD.
  METHOD drops_blank_and_comment.
    DATA(l) = lcl_scanner=>scan( |a: 1\n\n# just a comment\nb: 2| ).
    cl_abap_unit_assert=>assert_equals( act = lines( l ) exp = 2 ).
  ENDMETHOD.
  METHOD rejects_tab_indent.
    TRY.
        lcl_scanner=>scan( |a:\n\tb: 2| ).
        cl_abap_unit_assert=>fail( `expected tab rejection` ).
      CATCH cx_sy_conversion_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_tree DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS build_mapping FOR TESTING.
ENDCLASS.
CLASS ltc_tree IMPLEMENTATION.
  METHOD build_mapping.
    DATA(m) = lcl_tree=>new_collection( c_node=>mapping ).
    lcl_tree=>add_child( node = m key = `a` child = lcl_tree=>new_scalar( `1` ) ).
    lcl_tree=>add_child( node = m key = `b` child = lcl_tree=>new_scalar( `2` ) ).
    cl_abap_unit_assert=>assert_equals( act = m->node-kind exp = c_node=>mapping ).
    cl_abap_unit_assert=>assert_equals( act = lines( m->children ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = m->children[ 1 ]-key exp = `a` ).
    cl_abap_unit_assert=>assert_equals( act = m->children[ 1 ]-node->node-value exp = `1` ).
    cl_abap_unit_assert=>assert_equals( act = m->children[ 2 ]-node->node-value exp = `2` ).
    " a scalar built with default is_null must be false
    DATA(s) = lcl_tree=>new_scalar( `x` ).
    cl_abap_unit_assert=>assert_equals( act = s->node-is_null exp = abap_false ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_scalar DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS single_quote       FOR TESTING RAISING cx_sy_conversion_error.
    METHODS double_escapes     FOR TESTING RAISING cx_sy_conversion_error.
    METHODS tilde_is_null      FOR TESTING RAISING cx_sy_conversion_error.
    METHODS flow_map           FOR TESTING RAISING cx_sy_conversion_error.
    METHODS flow_seq           FOR TESTING RAISING cx_sy_conversion_error.
    METHODS colon_in_quotes    FOR TESTING RAISING cx_sy_conversion_error.
    METHODS unterminated_fails FOR TESTING.
    METHODS flow_map_varlen    FOR TESTING RAISING cx_sy_conversion_error.
    METHODS flow_nested        FOR TESTING RAISING cx_sy_conversion_error.
ENDCLASS.
CLASS ltc_scalar IMPLEMENTATION.
  METHOD single_quote.
    lcl_parser=>resolve_scalar( EXPORTING raw = `'it''s'` IMPORTING value = DATA(v) is_null = DATA(n) ).
    cl_abap_unit_assert=>assert_equals( act = v exp = `it's` ).
  ENDMETHOD.
  METHOD double_escapes.
    lcl_parser=>resolve_scalar( EXPORTING raw = `"a\tb"` IMPORTING value = DATA(v) is_null = DATA(n) ).
    cl_abap_unit_assert=>assert_equals( act = v exp = |a\tb| ).
  ENDMETHOD.
  METHOD tilde_is_null.
    lcl_parser=>resolve_scalar( EXPORTING raw = `~` IMPORTING value = DATA(v) is_null = DATA(n) ).
    cl_abap_unit_assert=>assert_equals( act = n exp = abap_true ).
  ENDMETHOD.
  METHOD flow_map.
    DATA(r) = lcl_parser=>parse_flow( `{a: 1, b: 2}` ).
    cl_abap_unit_assert=>assert_equals( act = r->node-kind exp = c_node=>mapping ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 2 ]-key exp = `b` ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 2 ]-node->node-value exp = `2` ).
  ENDMETHOD.
  METHOD flow_seq.
    DATA(r) = lcl_parser=>parse_flow( `[x, y, z]` ).
    cl_abap_unit_assert=>assert_equals( act = r->node-kind exp = c_node=>sequence ).
    cl_abap_unit_assert=>assert_equals( act = lines( r->children ) exp = 3 ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 3 ]-node->node-value exp = `z` ).
  ENDMETHOD.
  METHOD colon_in_quotes.
    DATA(r) = lcl_parser=>parse( lcl_scanner=>scan( |a: "x: y"| ) ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node->node-value exp = `x: y` ).
  ENDMETHOD.
  METHOD unterminated_fails.
    TRY.
        lcl_parser=>resolve_scalar( EXPORTING raw = `"oops` IMPORTING value = DATA(v) is_null = DATA(n) ).
        cl_abap_unit_assert=>fail( `expected unterminated` ).
      CATCH cx_sy_conversion_error.
    ENDTRY.
  ENDMETHOD.
  METHOD flow_map_varlen.
    DATA(r) = lcl_parser=>parse_flow( `{abc: 1, b: 2}` ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-key exp = `abc` ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node->node-value exp = `1` ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 2 ]-key exp = `b` ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 2 ]-node->node-value exp = `2` ).
  ENDMETHOD.
  METHOD flow_nested.
    DATA(r) = lcl_parser=>parse_flow( `{a: [1, 2]}` ).
    DATA(av) = r->children[ 1 ]-node.
    cl_abap_unit_assert=>assert_equals( act = av->node-kind exp = c_node=>sequence ).
    cl_abap_unit_assert=>assert_equals( act = lines( av->children ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = av->children[ 2 ]-node->node-value exp = `2` ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_parser DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS flat_mapping       FOR TESTING RAISING cx_sy_conversion_error.
    METHODS nested_mapping     FOR TESTING RAISING cx_sy_conversion_error.
    METHODS block_sequence     FOR TESTING RAISING cx_sy_conversion_error.
    METHODS seq_of_mappings    FOR TESTING RAISING cx_sy_conversion_error.
    METHODS key_null_value     FOR TESTING RAISING cx_sy_conversion_error.
    METHODS bad_dedent_fails   FOR TESTING.
    METHODS scalar_with_colon  FOR TESTING RAISING cx_sy_conversion_error.
    METHODS p IMPORTING t TYPE string RETURNING VALUE(r) TYPE ty_node_ref RAISING cx_sy_conversion_error.
ENDCLASS.
CLASS ltc_parser IMPLEMENTATION.
  METHOD p.
    r = lcl_parser=>parse( lcl_scanner=>scan( t ) ).
  ENDMETHOD.
  METHOD flat_mapping.
    DATA(r) = p( |a: 1\nb: 2| ).
    cl_abap_unit_assert=>assert_equals( act = r->node-kind exp = c_node=>mapping ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-key exp = `a` ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node->node-value exp = `1` ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 2 ]-node->node-value exp = `2` ).
  ENDMETHOD.
  METHOD nested_mapping.
    DATA(r) = p( |parent:\n  child: v| ).
    DATA(pn) = r->children[ 1 ]-node.
    cl_abap_unit_assert=>assert_equals( act = pn->node-kind exp = c_node=>mapping ).
    cl_abap_unit_assert=>assert_equals( act = pn->children[ 1 ]-key exp = `child` ).
    cl_abap_unit_assert=>assert_equals( act = pn->children[ 1 ]-node->node-value exp = `v` ).
  ENDMETHOD.
  METHOD block_sequence.
    DATA(r) = p( |- x\n- y| ).
    cl_abap_unit_assert=>assert_equals( act = r->node-kind exp = c_node=>sequence ).
    cl_abap_unit_assert=>assert_equals( act = lines( r->children ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 2 ]-node->node-value exp = `y` ).
  ENDMETHOD.
  METHOD seq_of_mappings.
    DATA(r) = p( |- name: a\n  port: 1\n- name: b\n  port: 2| ).
    cl_abap_unit_assert=>assert_equals( act = lines( r->children ) exp = 2 ).
    DATA(first) = r->children[ 1 ]-node.
    cl_abap_unit_assert=>assert_equals( act = first->node-kind exp = c_node=>mapping ).
    cl_abap_unit_assert=>assert_equals( act = first->children[ 1 ]-node->node-value exp = `a` ).
    cl_abap_unit_assert=>assert_equals( act = first->children[ 2 ]-key exp = `port` ).
  ENDMETHOD.
  METHOD key_null_value.
    DATA(r) = p( |a:\nb: 2| ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node->node-is_null exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 2 ]-node->node-value exp = `2` ).
  ENDMETHOD.
  METHOD bad_dedent_fails.
    TRY.
        lcl_parser=>parse( lcl_scanner=>scan( |a:\n    b: 1\n   c: 2| ) ).
        cl_abap_unit_assert=>fail( `expected bad dedent` ).
      CATCH cx_sy_conversion_error.
    ENDTRY.
  ENDMETHOD.
  METHOD scalar_with_colon.
    " a sequence item that is a URL must stay a scalar, not become a mapping
    DATA(r) = p( |- http://example.com\n- plain| ).
    cl_abap_unit_assert=>assert_equals( act = r->node-kind exp = c_node=>sequence ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node->node-kind exp = c_node=>scalar ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node->node-value exp = `http://example.com` ).
  ENDMETHOD.
ENDCLASS.
