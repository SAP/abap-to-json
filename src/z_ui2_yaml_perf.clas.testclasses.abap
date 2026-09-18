*"* use this source file for your ABAP unit test classes
CLASS ltc_perf DEFINITION FOR TESTING DURATION LONG RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS baseline FOR TESTING.
ENDCLASS.
CLASS ltc_perf IMPLEMENTATION.
  METHOD baseline.
    " Intentionally fails to expose baseline numbers — NOT a regression gate.
    DATA(rt) = z_ui2_yaml_perf=>run( ).
    DATA lv_msg TYPE string.
    LOOP AT rt INTO DATA(row).
      lv_msg = lv_msg && row-name && `: ` && row-usec && ` µs avg\n`.
    ENDLOOP.
    cl_abap_unit_assert=>fail( lv_msg ).
  ENDMETHOD.
ENDCLASS.
