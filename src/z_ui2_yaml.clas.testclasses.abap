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
