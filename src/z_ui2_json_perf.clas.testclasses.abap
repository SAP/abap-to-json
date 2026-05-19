CLASS ltcl_perf_test DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION LONG
  FINAL.

  PUBLIC SECTION.
    METHODS run_and_report FOR TESTING.

ENDCLASS.

CLASS ltcl_perf_test IMPLEMENTATION.

  METHOD run_and_report.
    DATA(lt_result) = z_ui2_json_perf=>run( ).

    DATA lv_msg TYPE string.
    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<row>).
      lv_msg = lv_msg && |{ <row>-name }: V23={ <row>-old } V1={ <row>-new } diff={ <row>-diff } ({ <row>-percent }%)\n|.
    ENDLOOP.

    cl_abap_unit_assert=>fail( msg = lv_msg ).
  ENDMETHOD.

ENDCLASS.
