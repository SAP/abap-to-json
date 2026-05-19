*&---------------------------------------------------------------------*
*& Report Z_UI2_JSON_PERF_TEST
*&---------------------------------------------------------------------*
*& Performance comparison: Z_UI2_JSON (V23) vs Z_UI2_JSON2 (V1)
*&---------------------------------------------------------------------*
REPORT z_ui2_json_perf_test.

CLASS z_json_perf DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF runtime,
             name       TYPE string,
             old        TYPE i,
             new        TYPE i,
             diff       TYPE i,
             percent(5) TYPE p DECIMALS 2,
           END OF runtime.

    DATA runtimes TYPE STANDARD TABLE OF runtime WITH DEFAULT KEY READ-ONLY.

    METHODS perform_tests.

  PRIVATE SECTION.
    METHODS start_measurement RETURNING VALUE(runtime) TYPE i.
    METHODS end_measurement IMPORTING start TYPE i RETURNING VALUE(duration) TYPE i.
    METHODS store_runtime IMPORTING old TYPE i new TYPE i name TYPE string.
    METHODS perf_timestamp.
    METHODS perf_sbook.
    METHODS perf_all_types.
    METHODS perf_strings.
    METHODS perf_deep_structure.
    METHODS perf_generate.

ENDCLASS.

DATA: lo_perf_test TYPE REF TO z_json_perf.

START-OF-SELECTION.

  CREATE OBJECT lo_perf_test.
  lo_perf_test->perform_tests( ).

  SKIP.
  ULINE AT /1(127).

  WRITE: / sy-vline,
        (50) 'Test' COLOR COL_HEADING, sy-vline,
        (15) 'V23 (µs)' CENTERED COLOR COL_HEADING, sy-vline,
        (15) 'V1 (µs)' CENTERED COLOR COL_HEADING, sy-vline,
        (15) 'Diff (µs)' CENTERED COLOR COL_HEADING, sy-vline,
        (16) 'Diff (%)' CENTERED COLOR COL_HEADING, sy-vline.

  ULINE AT /1(127).

  LOOP AT lo_perf_test->runtimes ASSIGNING FIELD-SYMBOL(<runtime>).
    WRITE: / sy-vline,
        (50) <runtime>-name COLOR COL_HEADING, sy-vline,
        (15) <runtime>-old RIGHT-JUSTIFIED, sy-vline,
        (15) <runtime>-new RIGHT-JUSTIFIED, sy-vline,
        (15) <runtime>-diff RIGHT-JUSTIFIED, sy-vline.
    IF <runtime>-percent LE -3.
      WRITE: (15) <runtime>-percent RIGHT-JUSTIFIED NO-GAP COLOR COL_NEGATIVE, '%' COLOR COL_NEGATIVE.
    ELSEIF <runtime>-percent GE 3.
      WRITE: (15) <runtime>-percent RIGHT-JUSTIFIED NO-GAP COLOR COL_POSITIVE, '%' COLOR COL_POSITIVE.
    ELSE.
      WRITE: (15) <runtime>-percent RIGHT-JUSTIFIED NO-GAP, '%'.
    ENDIF.
    WRITE: sy-vline.
  ENDLOOP.

  ULINE AT /1(127).


CLASS z_json_perf IMPLEMENTATION.

  METHOD perform_tests.
    perf_timestamp( ).
    perf_sbook( ).
    perf_all_types( ).
    perf_strings( ).
    perf_deep_structure( ).
    perf_generate( ).
  ENDMETHOD.

  METHOD start_measurement.
    GET RUN TIME FIELD runtime.
  ENDMETHOD.

  METHOD end_measurement.
    DATA(now) = 0.
    GET RUN TIME FIELD now.
    duration = now - start.
  ENDMETHOD.

  METHOD store_runtime.
    DATA(ls_rt) = VALUE runtime(
      name = name old = old new = new
      diff = old - new ).
    IF old > 0.
      ls_rt-percent = ( old - new ) / old * 100.
    ENDIF.
    APPEND ls_rt TO runtimes.
  ENDMETHOD.

  METHOD perf_timestamp.

    DATA: BEGIN OF timestamp_line,
            ts TYPE timestamp,
            tl TYPE timestampl,
          END OF timestamp_line,
          timestamps  LIKE STANDARD TABLE OF timestamp_line,
          timestamps2 LIKE timestamps.

    DATA: start TYPE i,
          old   TYPE i,
          new   TYPE i.

    DO 100000 TIMES.
      GET TIME STAMP FIELD timestamp_line-ts.
      GET TIME STAMP FIELD timestamp_line-tl.
      APPEND timestamp_line TO timestamps.
    ENDDO.

    DATA(times) = 5.

    " --- Serialize ---
    CLEAR: new, old.
    DO times TIMES.
      start = start_measurement( ).
      DATA(json1) = z_ui2_json=>serialize( data = timestamps ts_as_iso8601 = abap_true ).
      old += end_measurement( start ).

      start = start_measurement( ).
      DATA(json2) = z_ui2_json2=>serialize( data = timestamps ts_as_iso8601 = abap_true ).
      new += end_measurement( start ).
    ENDDO.
    store_runtime( name = |Serialize Timestamps 100k ({ times }x)| old = old / times new = new / times ).
    ASSERT json1 = json2.

    " --- Deserialize ---
    CLEAR: new, old.
    DO times TIMES.
      CLEAR timestamps2.
      start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = json1 CHANGING data = timestamps2 ).
      old += end_measurement( start ).
      ASSERT timestamps = timestamps2.

      CLEAR timestamps2.
      start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = json2 CHANGING data = timestamps2 ).
      new += end_measurement( start ).
      ASSERT timestamps = timestamps2.
    ENDDO.
    store_runtime( name = |Deserialize Timestamps 100k ({ times }x)| old = old / times new = new / times ).

  ENDMETHOD.

  METHOD perf_sbook.

    DATA: start TYPE i,
          old   TYPE i,
          new   TYPE i.

    SELECT * FROM sbook UP TO 20000 ROWS INTO TABLE @DATA(sbook) ORDER BY PRIMARY KEY.

    DATA(times) = 5.

    " --- Serialize ---
    CLEAR: new, old.
    DO times TIMES.
      start = start_measurement( ).
      DATA(json1) = z_ui2_json=>serialize( data = sbook ).
      old += end_measurement( start ).

      start = start_measurement( ).
      DATA(json2) = z_ui2_json2=>serialize( data = sbook ).
      new += end_measurement( start ).
    ENDDO.
    store_runtime( name = |Serialize SBOOK { lines( sbook ) } lines ({ times }x)| old = old / times new = new / times ).
    ASSERT json1 = json2.

    " --- Deserialize ---
    DATA sbook2 LIKE sbook.
    CLEAR: new, old.
    DO times TIMES.
      CLEAR sbook2.
      start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = json1 CHANGING data = sbook2 ).
      old += end_measurement( start ).
      ASSERT sbook = sbook2.

      CLEAR sbook2.
      start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = json2 CHANGING data = sbook2 ).
      new += end_measurement( start ).
      ASSERT sbook = sbook2.
    ENDDO.
    store_runtime( name = |Deserialize SBOOK { lines( sbook ) } lines ({ times }x)| old = old / times new = new / times ).

    " --- Serialize compressed + camelCase ---
    CLEAR: new, old.
    DO times TIMES.
      start = start_measurement( ).
      json1 = z_ui2_json=>serialize( data = sbook compress = abap_true pretty_name = z_ui2_json=>pretty_mode-camel_case ).
      old += end_measurement( start ).

      start = start_measurement( ).
      json2 = z_ui2_json2=>serialize( data = sbook compress = abap_true pretty_name = z_ui2_json2=>pretty_mode-camel_case ).
      new += end_measurement( start ).
    ENDDO.
    store_runtime( name = |Serialize SBOOK compressed+camelCase ({ times }x)| old = old / times new = new / times ).
    ASSERT json1 = json2.

    " --- Deserialize camelCase ---
    CLEAR: new, old.
    DO times TIMES.
      CLEAR sbook2.
      start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = json1 pretty_name = z_ui2_json=>pretty_mode-camel_case CHANGING data = sbook2 ).
      old += end_measurement( start ).

      CLEAR sbook2.
      start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = json2 pretty_name = z_ui2_json2=>pretty_mode-camel_case CHANGING data = sbook2 ).
      new += end_measurement( start ).
    ENDDO.
    store_runtime( name = |Deserialize SBOOK camelCase ({ times }x)| old = old / times new = new / times ).

  ENDMETHOD.

  METHOD perf_all_types.

    DATA: BEGIN OF test,
            id         TYPE i,
            timestamp  TYPE timestamp,
            timestampl TYPE timestampl,
            int1       TYPE int1,
            int2       TYPE int2,
            int4       TYPE int4,
            int8       TYPE int8,
            packed(8)  TYPE p DECIMALS 2,
            fp         TYPE f,
            char(10)   TYPE c,
            n(8)       TYPE n,
            string     TYPE string,
            xstring    TYPE xstring,
            date       TYPE d,
            time       TYPE t,
            bool       TYPE abap_bool,
          END OF test,
          tests  LIKE STANDARD TABLE OF test,
          tests2 LIKE tests.

    DATA: start TYPE i,
          old   TYPE i,
          new   TYPE i.

    DO 10000 TIMES.
      IF sy-index MOD 20 = 0.
        INSERT VALUE #( id = sy-index ) INTO TABLE tests.
      ELSE.
        test-id = sy-index.
        GET TIME STAMP FIELD test-timestamp.
        GET TIME STAMP FIELD test-timestampl.
        test-int1 = sy-index MOD 10 + 1.
        test-int2 = sy-index MOD 5 + sy-index + 2.
        test-int4 = sy-index + 4.
        test-int8 = sy-index + 8.
        test-packed = sy-index + '123.45'.
        test-fp = sy-index + '234.56'.
        test-char = 'TestiTest'.
        test-n = '00000123'.
        test-string = `Testi Test`.
        test-xstring = cl_abap_codepage=>convert_to( `Testi Test` ).
        test-date = sy-datum.
        test-time = sy-timlo.
        test-bool = abap_true.
        INSERT test INTO TABLE tests.
      ENDIF.
    ENDDO.

    DATA(times) = 5.

    " --- Serialize ---
    CLEAR: new, old.
    DO times TIMES.
      start = start_measurement( ).
      DATA(json1) = z_ui2_json=>serialize( data = tests ts_as_iso8601 = abap_true ).
      old += end_measurement( start ).

      start = start_measurement( ).
      DATA(json2) = z_ui2_json2=>serialize( data = tests ts_as_iso8601 = abap_true ).
      new += end_measurement( start ).
    ENDDO.
    store_runtime( name = |Serialize AllTypes 10k ({ times }x)| old = old / times new = new / times ).
    ASSERT json1 = json2.

    " --- Deserialize ---
    CLEAR: new, old.
    DO times TIMES.
      CLEAR tests2.
      start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = json1 CHANGING data = tests2 ).
      old += end_measurement( start ).
      ASSERT tests = tests2.

      CLEAR tests2.
      start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = json2 CHANGING data = tests2 ).
      new += end_measurement( start ).
      ASSERT tests = tests2.
    ENDDO.
    store_runtime( name = |Deserialize AllTypes 10k ({ times }x)| old = old / times new = new / times ).

  ENDMETHOD.

  METHOD perf_strings.

    DATA: BEGIN OF str_line,
            short  TYPE string,
            medium TYPE string,
            long   TYPE string,
            escape TYPE string,
          END OF str_line,
          strings  LIKE STANDARD TABLE OF str_line,
          strings2 LIKE strings.

    DATA: start TYPE i,
          old   TYPE i,
          new   TYPE i.

    DO 10000 TIMES.
      str_line-short = |val{ sy-index }|.
      str_line-medium = |This is a medium length string number { sy-index } with some content|.
      str_line-long = |{ repeat( val = `abcdefghij` occ = 50 ) }{ sy-index }|.
      str_line-escape = |Line1\r\nLine2\tTabbed "quoted" back\\slash { sy-index }|.
      APPEND str_line TO strings.
    ENDDO.

    DATA(times) = 5.

    " --- Serialize ---
    CLEAR: new, old.
    DO times TIMES.
      start = start_measurement( ).
      DATA(json1) = z_ui2_json=>serialize( data = strings ).
      old += end_measurement( start ).

      start = start_measurement( ).
      DATA(json2) = z_ui2_json2=>serialize( data = strings ).
      new += end_measurement( start ).
    ENDDO.
    store_runtime( name = |Serialize Strings 10k ({ times }x)| old = old / times new = new / times ).
    ASSERT json1 = json2.

    " --- Deserialize ---
    CLEAR: new, old.
    DO times TIMES.
      CLEAR strings2.
      start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = json1 CHANGING data = strings2 ).
      old += end_measurement( start ).
      ASSERT strings = strings2.

      CLEAR strings2.
      start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = json2 CHANGING data = strings2 ).
      new += end_measurement( start ).
      ASSERT strings = strings2.
    ENDDO.
    store_runtime( name = |Deserialize Strings 10k ({ times }x)| old = old / times new = new / times ).

  ENDMETHOD.

  METHOD perf_deep_structure.

    TYPES: BEGIN OF ty_inner,
             field1 TYPE string,
             field2 TYPE i,
             field3 TYPE d,
           END OF ty_inner,
           BEGIN OF ty_outer,
             id    TYPE i,
             name  TYPE string,
             inner TYPE STANDARD TABLE OF ty_inner WITH DEFAULT KEY,
           END OF ty_outer.

    DATA: deep  TYPE STANDARD TABLE OF ty_outer WITH DEFAULT KEY,
          deep2 LIKE deep,
          outer LIKE LINE OF deep,
          inner LIKE LINE OF outer-inner.

    DATA: start TYPE i,
          old   TYPE i,
          new   TYPE i.

    DO 1000 TIMES.
      outer-id = sy-index.
      outer-name = |Object { sy-index }|.
      CLEAR outer-inner.
      DO 10 TIMES.
        inner-field1 = |Inner { sy-index }|.
        inner-field2 = sy-index * 100.
        inner-field3 = sy-datum.
        APPEND inner TO outer-inner.
      ENDDO.
      APPEND outer TO deep.
    ENDDO.

    DATA(times) = 5.

    " --- Serialize ---
    CLEAR: new, old.
    DO times TIMES.
      start = start_measurement( ).
      DATA(json1) = z_ui2_json=>serialize( data = deep compress = abap_true pretty_name = z_ui2_json=>pretty_mode-camel_case ).
      old += end_measurement( start ).

      start = start_measurement( ).
      DATA(json2) = z_ui2_json2=>serialize( data = deep compress = abap_true pretty_name = z_ui2_json2=>pretty_mode-camel_case ).
      new += end_measurement( start ).
    ENDDO.
    store_runtime( name = |Serialize Deep 1k*10 ({ times }x)| old = old / times new = new / times ).
    ASSERT json1 = json2.

    " --- Deserialize ---
    CLEAR: new, old.
    DO times TIMES.
      CLEAR deep2.
      start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = json1 pretty_name = z_ui2_json=>pretty_mode-camel_case CHANGING data = deep2 ).
      old += end_measurement( start ).
      ASSERT deep = deep2.

      CLEAR deep2.
      start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = json2 pretty_name = z_ui2_json2=>pretty_mode-camel_case CHANGING data = deep2 ).
      new += end_measurement( start ).
      ASSERT deep = deep2.
    ENDDO.
    store_runtime( name = |Deserialize Deep 1k*10 ({ times }x)| old = old / times new = new / times ).

  ENDMETHOD.

  METHOD perf_generate.

    DATA: start TYPE i,
          old   TYPE i,
          new   TYPE i.

    DATA: lo_data1 TYPE REF TO data,
          lo_data2 TYPE REF TO data.

    " Use SBOOK data for generate test
    SELECT * FROM sbook UP TO 5000 ROWS INTO TABLE @DATA(sbook) ORDER BY PRIMARY KEY.
    DATA(json) = z_ui2_json=>serialize( data = sbook ).

    DATA(times) = 3.

    " --- Generate (REF TO data) ---
    CLEAR: new, old.
    DO times TIMES.
      CLEAR lo_data1.
      start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = json gen_optimize = abap_true CHANGING data = lo_data1 ).
      old += end_measurement( start ).

      CLEAR lo_data2.
      start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = json CHANGING data = lo_data2 ).
      new += end_measurement( start ).
    ENDDO.
    store_runtime( name = |Generate SBOOK { lines( sbook ) } lines ({ times }x)| old = old / times new = new / times ).

    " Verify both produce same JSON when re-serialized
    DATA(json1) = z_ui2_json=>serialize( data = lo_data1 ).
    DATA(json2) = z_ui2_json2=>serialize( data = lo_data2 ).
    ASSERT json1 = json2.

  ENDMETHOD.

ENDCLASS.