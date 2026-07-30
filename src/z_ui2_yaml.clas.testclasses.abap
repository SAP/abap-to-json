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
