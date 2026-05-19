*&---------------------------------------------------------------------*
*& Report Z_UI2_JSON_PERF_TEST
*&---------------------------------------------------------------------*
*& Performance comparison: Z_UI2_JSON (V23) vs Z_UI2_JSON2 (V1)
*&---------------------------------------------------------------------*
REPORT z_ui2_json_perf_test.

START-OF-SELECTION.

  DATA(lt_result) = z_ui2_json_perf=>run( ).

  SKIP.
  ULINE AT /1(127).

  WRITE: / sy-vline,
        (50) 'Test' COLOR COL_HEADING, sy-vline,
        (15) 'V23 (µs)' CENTERED COLOR COL_HEADING, sy-vline,
        (15) 'V1 (µs)' CENTERED COLOR COL_HEADING, sy-vline,
        (15) 'Diff (µs)' CENTERED COLOR COL_HEADING, sy-vline,
        (16) 'Diff (%)' CENTERED COLOR COL_HEADING, sy-vline.

  ULINE AT /1(127).

  LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<row>).
    WRITE: / sy-vline,
        (50) <row>-name COLOR COL_HEADING, sy-vline,
        (15) <row>-old RIGHT-JUSTIFIED, sy-vline,
        (15) <row>-new RIGHT-JUSTIFIED, sy-vline,
        (15) <row>-diff RIGHT-JUSTIFIED, sy-vline.
    IF <row>-percent LE -3.
      WRITE: (15) <row>-percent RIGHT-JUSTIFIED NO-GAP COLOR COL_NEGATIVE, '%' COLOR COL_NEGATIVE.
    ELSEIF <row>-percent GE 3.
      WRITE: (15) <row>-percent RIGHT-JUSTIFIED NO-GAP COLOR COL_POSITIVE, '%' COLOR COL_POSITIVE.
    ELSE.
      WRITE: (15) <row>-percent RIGHT-JUSTIFIED NO-GAP, '%'.
    ENDIF.
    WRITE: sy-vline.
  ENDLOOP.

  ULINE AT /1(127).
