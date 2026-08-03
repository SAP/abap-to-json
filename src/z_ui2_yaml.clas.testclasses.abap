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
    METHODS hash_in_quotes     FOR TESTING RAISING cx_sy_conversion_error.
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
  METHOD hash_in_quotes.
    DATA(r) = lcl_parser=>parse( lcl_scanner=>scan( |msg: "hello # world"| ) ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node->node-value exp = `hello # world` ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_parser DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS flat_mapping          FOR TESTING RAISING cx_sy_conversion_error.
    METHODS nested_mapping        FOR TESTING RAISING cx_sy_conversion_error.
    METHODS block_sequence        FOR TESTING RAISING cx_sy_conversion_error.
    METHODS seq_of_mappings       FOR TESTING RAISING cx_sy_conversion_error.
    METHODS key_null_value        FOR TESTING RAISING cx_sy_conversion_error.
    METHODS bad_dedent_fails      FOR TESTING.
    METHODS scalar_with_colon     FOR TESTING RAISING cx_sy_conversion_error.
    METHODS flush_seq_under_key   FOR TESTING RAISING cx_sy_conversion_error.
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
  METHOD flush_seq_under_key.
    " flush style: sequence items at SAME indent as the key
    DATA(r) = p( |servers:\n- host: a\n  port: 1\n- host: b\n  port: 2| ).
    DATA(servers) = r->children[ 1 ]-node.
    cl_abap_unit_assert=>assert_equals( act = servers->node-kind exp = c_node=>sequence ).
    cl_abap_unit_assert=>assert_equals( act = lines( servers->children ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = servers->children[ 2 ]-node->children[ 1 ]-node->node-value exp = `b` ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_anchor DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS scalar_alias          FOR TESTING RAISING cx_sy_conversion_error.
    METHODS mapping_alias         FOR TESTING RAISING cx_sy_conversion_error.
    METHODS undefined_alias_fails FOR TESTING.
    METHODS glob_is_not_alias     FOR TESTING RAISING cx_sy_conversion_error.
    METHODS block_header_anchor   FOR TESTING RAISING cx_sy_conversion_error.
ENDCLASS.
CLASS ltc_anchor IMPLEMENTATION.
  METHOD scalar_alias.
    DATA(r) = lcl_parser=>parse( lcl_scanner=>scan( |a: &v hello\nb: *v| ) ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node->node-value exp = `hello` ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 2 ]-node->node-value exp = `hello` ).
  ENDMETHOD.
  METHOD mapping_alias.
    DATA(r) = lcl_parser=>parse( lcl_scanner=>scan( |base: &d\n  timeout: 30\nother: *d| ) ).
    DATA(other) = r->children[ 2 ]-node.
    cl_abap_unit_assert=>assert_equals( act = other->node-kind exp = c_node=>mapping ).
    cl_abap_unit_assert=>assert_equals( act = other->children[ 1 ]-node->node-value exp = `30` ).
    " shared-reference contract: both children must point to the same lcl_node_ref instance
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node exp = r->children[ 2 ]-node ).
  ENDMETHOD.
  METHOD undefined_alias_fails.
    TRY.
        lcl_parser=>parse( lcl_scanner=>scan( |a: *missing| ) ).
        cl_abap_unit_assert=>fail( `expected undefined alias` ).
      CATCH cx_sy_conversion_error.
    ENDTRY.
  ENDMETHOD.
  METHOD glob_is_not_alias.
    " a value starting with * that isn't a valid alias name stays a scalar
    DATA(r) = lcl_parser=>parse( lcl_scanner=>scan( |pattern: '*.txt'| ) ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node->node-value exp = `*.txt` ).
    " unquoted bare-ish glob also must not raise as an undefined alias
    DATA(r2) = lcl_parser=>parse( lcl_scanner=>scan( |g: *.log| ) ).
    cl_abap_unit_assert=>assert_equals( act = r2->children[ 1 ]-node->node-value exp = `*.log` ).
  ENDMETHOD.
  METHOD block_header_anchor.
    DATA(r) = lcl_parser=>parse( lcl_scanner=>scan( |a: &blk \|\n  line1\n  line2\nb: *blk| ) ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node->node-value exp = |line1\nline2\n| ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 2 ]-node->node-value exp = |line1\nline2\n| ).
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node exp = r->children[ 2 ]-node ).
  ENDMETHOD.
ENDCLASS.
CLASS ltc_block_scalar DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS literal_keeps_newlines FOR TESTING RAISING cx_sy_conversion_error.
    METHODS folded_joins_lines     FOR TESTING RAISING cx_sy_conversion_error.
    METHODS strip_chomp            FOR TESTING RAISING cx_sy_conversion_error.
    METHODS keep_chomp             FOR TESTING RAISING cx_sy_conversion_error.
    METHODS crlf_block_scalar      FOR TESTING RAISING cx_sy_conversion_error.
    METHODS v IMPORTING t TYPE string RETURNING VALUE(r) TYPE string RAISING cx_sy_conversion_error.
ENDCLASS.
CLASS ltc_block_scalar IMPLEMENTATION.
  METHOD v.
    DATA(root) = lcl_parser=>parse( lcl_scanner=>scan( t ) ).
    r = root->children[ 1 ]-node->node-value.
  ENDMETHOD.
  METHOD literal_keeps_newlines.
    cl_abap_unit_assert=>assert_equals( act = v( |k: \|\n  line1\n  line2| ) exp = |line1\nline2\n| ).
  ENDMETHOD.
  METHOD folded_joins_lines.
    cl_abap_unit_assert=>assert_equals( act = v( |k: >\n  line1\n  line2| ) exp = |line1 line2\n| ).
  ENDMETHOD.
  METHOD strip_chomp.
    cl_abap_unit_assert=>assert_equals( act = v( |k: \|-\n  line1| ) exp = `line1` ).
  ENDMETHOD.
  METHOD keep_chomp.
    " |+ keeps trailing blank line -> "line1\n\n"
    cl_abap_unit_assert=>assert_equals( act = v( |k: \|+\n  line1\n| ) exp = |line1\n\n| ).
  ENDMETHOD.
  METHOD crlf_block_scalar.
    " CRLF input: block-scalar body lines must have CR stripped, no early truncation
    DATA(crlf) = cl_abap_char_utilities=>cr_lf.
    DATA(y) = |k: \|{ crlf }  line1{ crlf }{ crlf }  line3{ crlf }next: x|.
    DATA(r) = lcl_parser=>parse( lcl_scanner=>scan( y ) ).
    " literal block: line1 + blank line + line3 + clip newline
    cl_abap_unit_assert=>assert_equals( act = r->children[ 1 ]-node->node-value
                                        exp = |line1\n\nline3\n| ).
    " sibling key must be present (no early truncation)
    cl_abap_unit_assert=>assert_equals( act = r->children[ 2 ]-key exp = `next` ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_deser DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS flat_struct          FOR TESTING.
    METHODS nested_struct        FOR TESTING.
    METHODS table_of_struct      FOR TESTING.
    METHODS strict_bad_number    FOR TESTING.
    METHODS lenient_bad_number   FOR TESTING.
    METHODS camel_inverse_deser  FOR TESTING.
    METHODS pascal_inverse_deser FOR TESTING.
    METHODS camel_round_trip     FOR TESTING.
ENDCLASS.
CLASS ltc_deser IMPLEMENTATION.
  METHOD flat_struct.
    TYPES: BEGIN OF ty, name TYPE string, port TYPE i, END OF ty.
    DATA out TYPE ty.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |name: web\nport: 8080| CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out-name exp = `web` ).
    cl_abap_unit_assert=>assert_equals( act = out-port exp = 8080 ).
  ENDMETHOD.
  METHOD nested_struct.
    TYPES: BEGIN OF ty_i, host TYPE string, port TYPE i, END OF ty_i.
    TYPES: BEGIN OF ty, name TYPE string, inner TYPE ty_i, END OF ty.
    DATA out TYPE ty.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |name: x\ninner:\n  host: h\n  port: 5| CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out-inner-host exp = `h` ).
    cl_abap_unit_assert=>assert_equals( act = out-inner-port exp = 5 ).
  ENDMETHOD.
  METHOD table_of_struct.
    TYPES: BEGIN OF ty, name TYPE string, port TYPE i, END OF ty.
    DATA out TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |- name: a\n  port: 1\n- name: b\n  port: 2| CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = lines( out ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = out[ 2 ]-port exp = 2 ).
  ENDMETHOD.
  METHOD strict_bad_number.
    TYPES: BEGIN OF ty, port TYPE i, END OF ty.
    DATA out TYPE ty.
    DATA(o) = NEW z_ui2_yaml( strict_mode = abap_true ).
    TRY.
        o->deserialize_int( EXPORTING yaml = |port: notanumber| CHANGING data = out ).
        cl_abap_unit_assert=>fail( `expected cast error` ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.
  METHOD lenient_bad_number.
    TYPES: BEGIN OF ty, port TYPE i, END OF ty.
    DATA out TYPE ty.
    " lenient (static): bad number left initial, no raise
    z_ui2_yaml=>deserialize( EXPORTING yaml = |port: notanumber| CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out-port exp = 0 ).
  ENDMETHOD.
  METHOD camel_inverse_deser.
    TYPES: BEGIN OF ty, my_field TYPE string, another_one TYPE i, END OF ty.
    DATA out TYPE ty.
    DATA(o) = NEW z_ui2_yaml( pretty_name = z_ui2_yaml=>pretty_mode-camel_case ).
    o->deserialize_int( EXPORTING yaml = |myField: hello\nanotherOne: 5| CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out-my_field exp = `hello` ).
    cl_abap_unit_assert=>assert_equals( act = out-another_one exp = 5 ).
  ENDMETHOD.
  METHOD pascal_inverse_deser.
    TYPES: BEGIN OF ty, my_field TYPE string, END OF ty.
    DATA out TYPE ty.
    DATA(o) = NEW z_ui2_yaml( pretty_name = z_ui2_yaml=>pretty_mode-pascal_case ).
    o->deserialize_int( EXPORTING yaml = |MyField: hi| CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out-my_field exp = `hi` ).
  ENDMETHOD.
  METHOD camel_round_trip.
    TYPES: BEGIN OF ty, my_field TYPE string, port_num TYPE i, END OF ty.
    DATA(in) = VALUE ty( my_field = `x` port_num = 9 ).
    DATA out TYPE ty.
    DATA(o) = NEW z_ui2_yaml( pretty_name = z_ui2_yaml=>pretty_mode-camel_case ).
    o->deserialize_int( EXPORTING yaml = o->serialize_int( in ) CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out exp = in ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_gen DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS gen_mapping_field           FOR TESTING.
    METHODS gen_typed_int               FOR TESTING.
    METHODS gen_string_field            FOR TESTING.
    METHODS gen_sequence                FOR TESTING.
    METHODS gen_dup_sanitized_keys      FOR TESTING.
    METHODS gen_structural_error_fatal  FOR TESTING.
    METHODS gen_nonuniform_seq_keeps_all FOR TESTING.
    METHODS gen_mixed_elem_keeps_values FOR TESTING.
    METHODS gen_seq_of_mappings         FOR TESTING.
    METHODS gen_seq_of_mappings_typed   FOR TESTING.
ENDCLASS.
CLASS ltc_gen IMPLEMENTATION.
  METHOD gen_mapping_field.
    DATA(r) = z_ui2_yaml=>generate( |host: localhost\nport: 5432| ).
    FIELD-SYMBOLS <s> TYPE any. ASSIGN r->* TO <s>.
    FIELD-SYMBOLS <f> TYPE any. ASSIGN COMPONENT `HOST` OF STRUCTURE <s> TO <f>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = <f> exp = `localhost` ).
  ENDMETHOD.
  METHOD gen_typed_int.
    DATA(r) = z_ui2_yaml=>generate( |port: 5432| ).
    FIELD-SYMBOLS <s> TYPE any. ASSIGN r->* TO <s>.
    FIELD-SYMBOLS <f> TYPE any. ASSIGN COMPONENT `PORT` OF STRUCTURE <s> TO <f>.
    DATA(td) = cl_abap_typedescr=>describe_by_data( <f> ).
    cl_abap_unit_assert=>assert_differs( act = td->type_kind exp = cl_abap_typedescr=>typekind_string ).
    cl_abap_unit_assert=>assert_equals( act = <f> exp = 5432 ).
  ENDMETHOD.
  METHOD gen_string_field.
    DATA(r) = z_ui2_yaml=>generate( |name: web-01| ).
    FIELD-SYMBOLS <s> TYPE any. ASSIGN r->* TO <s>.
    FIELD-SYMBOLS <f> TYPE any. ASSIGN COMPONENT `NAME` OF STRUCTURE <s> TO <f>.
    cl_abap_unit_assert=>assert_equals( act = <f> exp = `web-01` ).
  ENDMETHOD.
  METHOD gen_sequence.
    DATA(r) = z_ui2_yaml=>generate( |- 1\n- 2\n- 3| ).
    FIELD-SYMBOLS <t> TYPE ANY TABLE. ASSIGN r->* TO <t>.
    cl_abap_unit_assert=>assert_equals( act = lines( <t> ) exp = 3 ).
    " element type must be typed (not string) — integers 1/2/3 → TYPE i
    FIELD-SYMBOLS <st> TYPE STANDARD TABLE. ASSIGN r->* TO <st>.
    FIELD-SYMBOLS <e> TYPE any. READ TABLE <st> INDEX 1 ASSIGNING <e>.
    DATA(etd) = cl_abap_typedescr=>describe_by_data( <e> ).
    cl_abap_unit_assert=>assert_differs( act = etd->type_kind exp = cl_abap_typedescr=>typekind_string ).
  ENDMETHOD.
  METHOD gen_dup_sanitized_keys.
    " my-key and my_key both sanitize to MY_KEY — must not dump, must produce 2 distinct components
    DATA(r) = z_ui2_yaml=>generate( |my-key: 1\nmy_key: 2| ).
    cl_abap_unit_assert=>assert_bound( r ).
    FIELD-SYMBOLS <s> TYPE any. ASSIGN r->* TO <s>.
    DATA(td) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <s> ) ).
    cl_abap_unit_assert=>assert_equals( act = lines( td->components ) exp = 2 ).
  ENDMETHOD.
  METHOD gen_structural_error_fatal.
    TRY.
        DATA(r) = z_ui2_yaml=>generate( |a:\n\tb: 1| ).  " tab in indentation = structural error
        cl_abap_unit_assert=>fail( `expected structural error to propagate` ).
      CATCH cx_sy_conversion_error.
    ENDTRY.
  ENDMETHOD.
  METHOD gen_nonuniform_seq_keeps_all.
    " non-uniform sequence (int/bool/int/int): all 4 elements must survive (regression for #6 fix)
    DATA(r) = z_ui2_yaml=>generate( |- 1\n- true\n- 2\n- 3| ).
    FIELD-SYMBOLS <t> TYPE ANY TABLE.
    ASSIGN r->* TO <t>.
    cl_abap_unit_assert=>assert_equals( act = lines( <t> ) exp = 4 ).
  ENDMETHOD.
  METHOD gen_mixed_elem_keeps_values.
    " mixed elementary types (int + decimal + int) → string-table fallback preserves all values
    " Under kind-only uniformity check this was TABLE OF I and truncated 2.5 → 2
    DATA(r) = z_ui2_yaml=>generate( |- 1\n- 2.5\n- 3| ).
    FIELD-SYMBOLS <t> TYPE STANDARD TABLE. ASSIGN r->* TO <t>.
    cl_abap_unit_assert=>assert_equals( act = lines( <t> ) exp = 3 ).
    FIELD-SYMBOLS <e> TYPE any. READ TABLE <t> INDEX 2 ASSIGNING <e>.
    cl_abap_unit_assert=>assert_char_cp( act = |{ <e> }| exp = `*2.5*` ).
  ENDMETHOD.
  METHOD gen_seq_of_mappings.
    " list of uniform objects (dominant config shape) must generate a table of 3, no dump
    DATA(r) = z_ui2_yaml=>generate( |- host: a\n  port: 1\n- host: b\n  port: 2\n- host: c\n  port: 3| ).
    FIELD-SYMBOLS <t> TYPE ANY TABLE. ASSIGN r->* TO <t>.
    cl_abap_unit_assert=>assert_equals( act = lines( <t> ) exp = 3 ).
  ENDMETHOD.
  METHOD gen_seq_of_mappings_typed.
    " values inside the objects survive: read row 2's host + port
    DATA(r) = z_ui2_yaml=>generate( |- host: a\n  port: 1\n- host: b\n  port: 2| ).
    FIELD-SYMBOLS <t> TYPE STANDARD TABLE. ASSIGN r->* TO <t>.
    FIELD-SYMBOLS <row> TYPE any. READ TABLE <t> INDEX 2 ASSIGNING <row>.
    FIELD-SYMBOLS <h> TYPE any. ASSIGN COMPONENT `HOST` OF STRUCTURE <row> TO <h>.
    cl_abap_unit_assert=>assert_equals( act = <h> exp = `b` ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_ser DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS flat_struct              FOR TESTING.
    METHODS quotes_colon             FOR TESTING.
    METHODS quotes_numeric_string    FOR TESTING.
    METHODS table_seq                FOR TESTING.
    METHODS bool_emits_true_false    FOR TESTING.
    METHODS bool_round_trip          FOR TESTING.
    METHODS round_trip               FOR TESTING.
ENDCLASS.
CLASS ltc_ser IMPLEMENTATION.
  METHOD flat_struct.
    TYPES: BEGIN OF ty, name TYPE string, port TYPE i, END OF ty.
    DATA(in) = VALUE ty( name = `web` port = 8080 ).
    cl_abap_unit_assert=>assert_equals( act = z_ui2_yaml=>serialize( in ) exp = |NAME: web\nPORT: 8080| ).
  ENDMETHOD.
  METHOD quotes_colon.
    TYPES: BEGIN OF ty, t TYPE string, END OF ty.
    DATA(in) = VALUE ty( t = `x: y` ).
    cl_abap_unit_assert=>assert_equals( act = z_ui2_yaml=>serialize( in ) exp = |T: "x: y"| ).
  ENDMETHOD.
  METHOD quotes_numeric_string.
    TYPES: BEGIN OF ty, code TYPE string, END OF ty.
    DATA(in) = VALUE ty( code = `007` ).
    cl_abap_unit_assert=>assert_equals( act = z_ui2_yaml=>serialize( in ) exp = |CODE: "007"| ).
  ENDMETHOD.
  METHOD table_seq.
    TYPES: BEGIN OF ty, name TYPE string, port TYPE i, END OF ty.
    DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
    tab = VALUE #( ( name = `a` port = 1 ) ( name = `b` port = 2 ) ).
    DATA(y) = z_ui2_yaml=>serialize( tab ).
    DATA out TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
    z_ui2_yaml=>deserialize( EXPORTING yaml = y CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = lines( out ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = out[ 2 ]-name exp = `b` ).
  ENDMETHOD.
  METHOD bool_emits_true_false.
    TYPES: BEGIN OF ty, enabled TYPE abap_bool, disabled TYPE abap_bool, END OF ty.
    DATA(in) = VALUE ty( enabled = abap_true disabled = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = z_ui2_yaml=>serialize( in ) exp = |ENABLED: true\nDISABLED: false| ).
  ENDMETHOD.
  METHOD bool_round_trip.
    TYPES: BEGIN OF ty, enabled TYPE abap_bool, disabled TYPE abap_bool, END OF ty.
    DATA(in) = VALUE ty( enabled = abap_true disabled = abap_false ).
    DATA out TYPE ty.
    z_ui2_yaml=>deserialize( EXPORTING yaml = z_ui2_yaml=>serialize( in ) CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out-enabled exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = out-disabled exp = abap_false ).
  ENDMETHOD.
  METHOD round_trip.
    TYPES: BEGIN OF ty, name TYPE string, port TYPE i, END OF ty.
    DATA(in) = VALUE ty( name = `a: b` port = 7 ).
    DATA out TYPE ty.
    z_ui2_yaml=>deserialize( EXPORTING yaml = z_ui2_yaml=>serialize( in ) CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out exp = in ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_fixtures DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS servers_list        FOR TESTING.
    METHODS app_config          FOR TESTING.
    METHODS flush_servers_typed FOR TESTING.
ENDCLASS.
CLASS ltc_fixtures IMPLEMENTATION.
  METHOD servers_list.
    TYPES: BEGIN OF ty_s, host TYPE string, port TYPE i, END OF ty_s.
    TYPES: BEGIN OF ty_w, servers TYPE STANDARD TABLE OF ty_s WITH DEFAULT KEY, END OF ty_w.
    DATA w TYPE ty_w.
    DATA(yaml) = |servers:\n  - host: a\n    port: 1\n  - host: b\n    port: 2|.
    z_ui2_yaml=>deserialize( EXPORTING yaml = yaml CHANGING data = w ).
    cl_abap_unit_assert=>assert_equals( act = lines( w-servers ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = w-servers[ 2 ]-host exp = `b` ).
  ENDMETHOD.
  METHOD app_config.
    TYPES: BEGIN OF ty, name TYPE string, enabled TYPE abap_bool, replicas TYPE i, END OF ty.
    DATA cfg TYPE ty.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |name: web\nenabled: true\nreplicas: 3| CHANGING data = cfg ).
    cl_abap_unit_assert=>assert_equals( act = cfg-name exp = `web` ).
    cl_abap_unit_assert=>assert_equals( act = cfg-enabled exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = cfg-replicas exp = 3 ).
  ENDMETHOD.
  METHOD flush_servers_typed.
    TYPES: BEGIN OF ty_s, host TYPE string, port TYPE i, END OF ty_s.
    TYPES: BEGIN OF ty_w, servers TYPE STANDARD TABLE OF ty_s WITH DEFAULT KEY, END OF ty_w.
    DATA w TYPE ty_w.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |servers:\n- host: a\n  port: 1\n- host: b\n  port: 2| CHANGING data = w ).
    cl_abap_unit_assert=>assert_equals( act = lines( w-servers ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = w-servers[ 2 ]-host exp = `b` ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_multidoc DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS generate_all_three    FOR TESTING.
    METHODS deserialize_all_typed FOR TESTING.
    METHODS single_doc_degenerate FOR TESTING.
    METHODS legacy_first_doc_only FOR TESTING.
ENDCLASS.
CLASS ltc_multidoc IMPLEMENTATION.
  METHOD generate_all_three.
    DATA(rt) = z_ui2_yaml=>generate_all( |a: 1\n---\na: 2\n---\na: 3| ).
    cl_abap_unit_assert=>assert_equals( act = lines( rt ) exp = 3 ).
    FIELD-SYMBOLS <s> TYPE any. ASSIGN rt[ 2 ]->* TO <s>.
    FIELD-SYMBOLS <f> TYPE any. ASSIGN COMPONENT `A` OF STRUCTURE <s> TO <f>.
    cl_abap_unit_assert=>assert_equals( act = <f> exp = 2 ).
  ENDMETHOD.
  METHOD deserialize_all_typed.
    TYPES: BEGIN OF ty, name TYPE string, port TYPE i, END OF ty.
    DATA results TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
    z_ui2_yaml=>deserialize_all( EXPORTING yaml = |name: a\nport: 1\n---\nname: b\nport: 2|
                                 CHANGING  results = results ).
    cl_abap_unit_assert=>assert_equals( act = lines( results ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = results[ 2 ]-name exp = `b` ).
    cl_abap_unit_assert=>assert_equals( act = results[ 2 ]-port exp = 2 ).
  ENDMETHOD.
  METHOD single_doc_degenerate.
    DATA(rt) = z_ui2_yaml=>generate_all( |a: 1| ).
    cl_abap_unit_assert=>assert_equals( act = lines( rt ) exp = 1 ).
  ENDMETHOD.
  METHOD legacy_first_doc_only.
    TYPES: BEGIN OF ty, a TYPE i, END OF ty.
    DATA out TYPE ty.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |a: 1\n---\na: 2| CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out-a exp = 1 ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_phaseb DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    " C1 — static DESERIALIZE must not dump on structural errors
    METHODS c1_tab_indent_lenient     FOR TESTING.
    METHODS c1_bad_dedent_lenient     FOR TESTING.
    METHODS c1_unterminated_lenient   FOR TESTING.
    " C2 — CRLF input
    METHODS c2_crlf_numeric           FOR TESTING.
    METHODS c2_crlf_string            FOR TESTING.
    " C3 — negative packed decimal trailing sign
    METHODS c3_negative_packed        FOR TESTING.
    " C4 — null/Null/NULL keyword
    METHODS c4_null_lower             FOR TESTING.
    METHODS c4_null_mixed             FOR TESTING.
    METHODS c4_null_upper             FOR TESTING.
    METHODS c4_quoted_null_is_string  FOR TESTING.
    " C5 — indent_step respected for multi-line table rows
    METHODS c5_indent_step_roundtrip  FOR TESTING.
ENDCLASS.

CLASS ltc_phaseb IMPLEMENTATION.

  METHOD c1_tab_indent_lenient.
    TYPES: BEGIN OF ty, a TYPE string, b TYPE string, END OF ty.
    DATA out TYPE ty.
    " tab in indentation → structural error; static API must absorb it, not dump
    z_ui2_yaml=>deserialize( EXPORTING yaml = |a: hello\n\tb: world| CHANGING data = out ).
    " out-a stays initial because structural error aborted before any mapping
    cl_abap_unit_assert=>assert_not_initial( act = abap_true ).  " reaching here = no dump = pass
  ENDMETHOD.

  METHOD c1_bad_dedent_lenient.
    TYPES: BEGIN OF ty, a TYPE string, END OF ty.
    DATA out TYPE ty.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |a:\n    b: 1\n   c: 2| CHANGING data = out ).
    cl_abap_unit_assert=>assert_not_initial( act = abap_true ).  " no dump = pass
  ENDMETHOD.

  METHOD c1_unterminated_lenient.
    TYPES: BEGIN OF ty, k TYPE string, END OF ty.
    DATA out TYPE ty.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |k: "oops| CHANGING data = out ).
    cl_abap_unit_assert=>assert_not_initial( act = abap_true ).  " no dump = pass
  ENDMETHOD.

  METHOD c2_crlf_numeric.
    TYPES: BEGIN OF ty, port TYPE i, name TYPE string, END OF ty.
    DATA out TYPE ty.
    DATA(crlf) = cl_abap_char_utilities=>cr_lf.
    DATA(yaml) = |port: 8080| && crlf && |name: web|.
    z_ui2_yaml=>deserialize( EXPORTING yaml = yaml CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out-port exp = 8080 ).
  ENDMETHOD.

  METHOD c2_crlf_string.
    TYPES: BEGIN OF ty, port TYPE i, name TYPE string, END OF ty.
    DATA out TYPE ty.
    DATA(crlf) = cl_abap_char_utilities=>cr_lf.
    DATA(yaml) = |port: 8080| && crlf && |name: web|.
    z_ui2_yaml=>deserialize( EXPORTING yaml = yaml CHANGING data = out ).
    " name must be exactly "web", no trailing CR
    cl_abap_unit_assert=>assert_equals( act = out-name exp = `web` ).
  ENDMETHOD.

  METHOD c3_negative_packed.
    TYPES: BEGIN OF ty, val TYPE p LENGTH 4 DECIMALS 2, END OF ty.
    DATA(in) = VALUE ty( val = '-3.14' ).
    DATA(yaml) = z_ui2_yaml=>serialize( in ).
    " must contain "-3.14", not "3.14-"
    cl_abap_unit_assert=>assert_char_cp( act = yaml exp = `*-3.14*` ).
  ENDMETHOD.

  METHOD c4_null_lower.
    TYPES: BEGIN OF ty, a TYPE string, END OF ty.
    DATA out TYPE ty.
    out-a = `prior`.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |a: null| CHANGING data = out ).
    cl_abap_unit_assert=>assert_initial( act = out-a ).
  ENDMETHOD.

  METHOD c4_null_mixed.
    TYPES: BEGIN OF ty, a TYPE string, END OF ty.
    DATA out TYPE ty.
    out-a = `prior`.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |a: Null| CHANGING data = out ).
    cl_abap_unit_assert=>assert_initial( act = out-a ).
  ENDMETHOD.

  METHOD c4_null_upper.
    TYPES: BEGIN OF ty, a TYPE string, END OF ty.
    DATA out TYPE ty.
    out-a = `prior`.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |a: NULL| CHANGING data = out ).
    cl_abap_unit_assert=>assert_initial( act = out-a ).
  ENDMETHOD.

  METHOD c4_quoted_null_is_string.
    TYPES: BEGIN OF ty, a TYPE string, END OF ty.
    DATA out TYPE ty.
    z_ui2_yaml=>deserialize( EXPORTING yaml = |a: "null"| CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = out-a exp = `null` ).
  ENDMETHOD.

  METHOD c5_indent_step_roundtrip.
    TYPES: BEGIN OF ty, name TYPE string, port TYPE i, END OF ty.
    DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
    tab = VALUE #( ( name = `a` port = 1 ) ( name = `b` port = 2 ) ).
    DATA out TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
    " indent=4: serialize then deserialize — round-trip must survive any indent width
    DATA(o) = NEW z_ui2_yaml( indent = 4 ).
    DATA(yaml) = o->serialize_int( tab ).
    o->deserialize_int( EXPORTING yaml = yaml CHANGING data = out ).
    cl_abap_unit_assert=>assert_equals( act = lines( out ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = out[ 2 ]-name exp = `b` ).
    cl_abap_unit_assert=>assert_equals( act = out[ 2 ]-port exp = 2 ).
  ENDMETHOD.

ENDCLASS.
