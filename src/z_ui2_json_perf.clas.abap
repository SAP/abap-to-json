CLASS z_ui2_json_perf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_runtime,
             name       TYPE string,
             old        TYPE i,
             new        TYPE i,
             diff       TYPE i,
             percent(5) TYPE p DECIMALS 2,
           END OF ty_runtime.

    TYPES tt_runtime TYPE STANDARD TABLE OF ty_runtime WITH DEFAULT KEY.

    CLASS-METHODS run
      RETURNING VALUE(rt_result) TYPE tt_runtime.

  PRIVATE SECTION.

    CLASS-METHODS start_measurement
      RETURNING VALUE(rv_start) TYPE i.

    CLASS-METHODS end_measurement
      IMPORTING iv_start          TYPE i
      RETURNING VALUE(rv_duration) TYPE i.

    CLASS-METHODS make_runtime
      IMPORTING iv_name           TYPE string
                iv_old            TYPE i
                iv_new            TYPE i
      RETURNING VALUE(rs_runtime) TYPE ty_runtime.

    CLASS-METHODS perf_timestamp
      RETURNING VALUE(rt_result) TYPE tt_runtime.

    CLASS-METHODS perf_sbook
      RETURNING VALUE(rt_result) TYPE tt_runtime.

    CLASS-METHODS perf_all_types
      RETURNING VALUE(rt_result) TYPE tt_runtime.

    CLASS-METHODS perf_strings
      RETURNING VALUE(rt_result) TYPE tt_runtime.

    CLASS-METHODS perf_deep_structure
      RETURNING VALUE(rt_result) TYPE tt_runtime.

    CLASS-METHODS perf_generate
      RETURNING VALUE(rt_result) TYPE tt_runtime.

ENDCLASS.


CLASS z_ui2_json_perf IMPLEMENTATION.

  METHOD run.
    APPEND LINES OF perf_timestamp( ) TO rt_result.
    APPEND LINES OF perf_sbook( ) TO rt_result.
    APPEND LINES OF perf_all_types( ) TO rt_result.
    APPEND LINES OF perf_strings( ) TO rt_result.
    APPEND LINES OF perf_deep_structure( ) TO rt_result.
    APPEND LINES OF perf_generate( ) TO rt_result.
  ENDMETHOD.

  METHOD start_measurement.
    GET RUN TIME FIELD rv_start.
  ENDMETHOD.

  METHOD end_measurement.
    DATA(lv_now) = 0.
    GET RUN TIME FIELD lv_now.
    rv_duration = lv_now - iv_start.
  ENDMETHOD.

  METHOD make_runtime.
    rs_runtime = VALUE #( name = iv_name old = iv_old new = iv_new diff = iv_old - iv_new ).
    IF iv_old > 0.
      rs_runtime-percent = ( iv_old - iv_new ) / iv_old * 100.
    ENDIF.
  ENDMETHOD.

  METHOD perf_timestamp.

    DATA: BEGIN OF timestamp_line,
            ts TYPE timestamp,
            tl TYPE timestampl,
          END OF timestamp_line,
          timestamps  LIKE STANDARD TABLE OF timestamp_line,
          timestamps2 LIKE timestamps.

    DATA: lv_start TYPE i,
          lv_old   TYPE i,
          lv_new   TYPE i.

    DO 100000 TIMES.
      GET TIME STAMP FIELD timestamp_line-ts.
      GET TIME STAMP FIELD timestamp_line-tl.
      APPEND timestamp_line TO timestamps.
    ENDDO.

    DATA(lv_times) = 5.

    " --- Serialize ---
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      lv_start = start_measurement( ).
      DATA(lv_json1) = z_ui2_json=>serialize( data = timestamps ts_as_iso8601 = abap_true ).
      lv_old += end_measurement( lv_start ).

      lv_start = start_measurement( ).
      DATA(lv_json2) = z_ui2_json2=>serialize( data = timestamps ts_as_iso8601 = abap_true ).
      lv_new += end_measurement( lv_start ).
    ENDDO.
    APPEND make_runtime( iv_name = |Serialize Timestamps 100k ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.
    ASSERT lv_json1 = lv_json2.

    " --- Deserialize ---
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      CLEAR timestamps2.
      lv_start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = lv_json1 CHANGING data = timestamps2 ).
      lv_old += end_measurement( lv_start ).
      ASSERT timestamps = timestamps2.

      CLEAR timestamps2.
      lv_start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = lv_json2 CHANGING data = timestamps2 ).
      lv_new += end_measurement( lv_start ).
      ASSERT timestamps = timestamps2.
    ENDDO.
    APPEND make_runtime( iv_name = |Deserialize Timestamps 100k ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.

  ENDMETHOD.

  METHOD perf_sbook.

    DATA: lv_start TYPE i,
          lv_old   TYPE i,
          lv_new   TYPE i.

    SELECT * FROM sbook UP TO 20000 ROWS INTO TABLE @DATA(lt_sbook) ORDER BY PRIMARY KEY.

    DATA(lv_times) = 5.

    " --- Serialize ---
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      lv_start = start_measurement( ).
      DATA(lv_json1) = z_ui2_json=>serialize( data = lt_sbook ).
      lv_old += end_measurement( lv_start ).

      lv_start = start_measurement( ).
      DATA(lv_json2) = z_ui2_json2=>serialize( data = lt_sbook ).
      lv_new += end_measurement( lv_start ).
    ENDDO.
    APPEND make_runtime( iv_name = |Serialize SBOOK { lines( lt_sbook ) } lines ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.
    ASSERT lv_json1 = lv_json2.

    " --- Deserialize ---
    DATA lt_sbook2 LIKE lt_sbook.
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      CLEAR lt_sbook2.
      lv_start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = lv_json1 CHANGING data = lt_sbook2 ).
      lv_old += end_measurement( lv_start ).
      ASSERT lt_sbook = lt_sbook2.

      CLEAR lt_sbook2.
      lv_start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = lv_json2 CHANGING data = lt_sbook2 ).
      lv_new += end_measurement( lv_start ).
      ASSERT lt_sbook = lt_sbook2.
    ENDDO.
    APPEND make_runtime( iv_name = |Deserialize SBOOK { lines( lt_sbook ) } lines ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.

    " --- Serialize compressed + camelCase ---
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      lv_start = start_measurement( ).
      lv_json1 = z_ui2_json=>serialize(
        data = lt_sbook compress = abap_true pretty_name = z_ui2_json=>pretty_mode-camel_case ).
      lv_old += end_measurement( lv_start ).

      lv_start = start_measurement( ).
      lv_json2 = z_ui2_json2=>serialize(
        data = lt_sbook compress = abap_true pretty_name = z_ui2_json2=>pretty_mode-camel_case ).
      lv_new += end_measurement( lv_start ).
    ENDDO.
    APPEND make_runtime( iv_name = |Serialize SBOOK compressed+camelCase ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.
    ASSERT lv_json1 = lv_json2.

    " --- Deserialize camelCase ---
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      CLEAR lt_sbook2.
      lv_start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = lv_json1
        pretty_name = z_ui2_json=>pretty_mode-camel_case CHANGING data = lt_sbook2 ).
      lv_old += end_measurement( lv_start ).

      CLEAR lt_sbook2.
      lv_start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = lv_json2
        pretty_name = z_ui2_json2=>pretty_mode-camel_case CHANGING data = lt_sbook2 ).
      lv_new += end_measurement( lv_start ).
    ENDDO.
    APPEND make_runtime( iv_name = |Deserialize SBOOK camelCase ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.

  ENDMETHOD.

  METHOD perf_all_types.

    DATA: BEGIN OF ls_test,
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
          END OF ls_test,
          lt_tests  LIKE STANDARD TABLE OF ls_test,
          lt_tests2 LIKE lt_tests.

    DATA: lv_start TYPE i,
          lv_old   TYPE i,
          lv_new   TYPE i.

    DO 10000 TIMES.
      IF sy-index MOD 20 = 0.
        INSERT VALUE #( id = sy-index ) INTO TABLE lt_tests.
      ELSE.
        ls_test-id = sy-index.
        GET TIME STAMP FIELD ls_test-timestamp.
        GET TIME STAMP FIELD ls_test-timestampl.
        ls_test-int1 = sy-index MOD 10 + 1.
        ls_test-int2 = sy-index MOD 5 + sy-index + 2.
        ls_test-int4 = sy-index + 4.
        ls_test-int8 = sy-index + 8.
        ls_test-packed = sy-index + '123.45'.
        ls_test-fp = sy-index + '234.56'.
        ls_test-char = 'TestiTest'.
        ls_test-n = '00000123'.
        ls_test-string = `Testi Test`.
        ls_test-xstring = cl_abap_codepage=>convert_to( `Testi Test` ).
        ls_test-date = sy-datum.
        ls_test-time = sy-timlo.
        ls_test-bool = abap_true.
        INSERT ls_test INTO TABLE lt_tests.
      ENDIF.
    ENDDO.

    DATA(lv_times) = 5.

    " --- Serialize ---
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      lv_start = start_measurement( ).
      DATA(lv_json1) = z_ui2_json=>serialize( data = lt_tests ts_as_iso8601 = abap_true ).
      lv_old += end_measurement( lv_start ).

      lv_start = start_measurement( ).
      DATA(lv_json2) = z_ui2_json2=>serialize( data = lt_tests ts_as_iso8601 = abap_true ).
      lv_new += end_measurement( lv_start ).
    ENDDO.
    APPEND make_runtime( iv_name = |Serialize AllTypes 10k ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.
    ASSERT lv_json1 = lv_json2.

    " --- Deserialize ---
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      CLEAR lt_tests2.
      lv_start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = lv_json1 CHANGING data = lt_tests2 ).
      lv_old += end_measurement( lv_start ).
      ASSERT lt_tests = lt_tests2.

      CLEAR lt_tests2.
      lv_start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = lv_json2 CHANGING data = lt_tests2 ).
      lv_new += end_measurement( lv_start ).
      ASSERT lt_tests = lt_tests2.
    ENDDO.
    APPEND make_runtime( iv_name = |Deserialize AllTypes 10k ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.

  ENDMETHOD.

  METHOD perf_strings.

    DATA: BEGIN OF ls_str,
            short  TYPE string,
            medium TYPE string,
            long   TYPE string,
            escape TYPE string,
          END OF ls_str,
          lt_strings  LIKE STANDARD TABLE OF ls_str,
          lt_strings2 LIKE lt_strings.

    DATA: lv_start TYPE i,
          lv_old   TYPE i,
          lv_new   TYPE i.

    DO 10000 TIMES.
      ls_str-short = |val{ sy-index }|.
      ls_str-medium = |This is a medium length string number { sy-index } with some content|.
      ls_str-long = |{ repeat( val = `abcdefghij` occ = 50 ) }{ sy-index }|.
      ls_str-escape = |Line1\r\nLine2\tTabbed "quoted" back\\slash { sy-index }|.
      APPEND ls_str TO lt_strings.
    ENDDO.

    DATA(lv_times) = 5.

    " --- Serialize ---
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      lv_start = start_measurement( ).
      DATA(lv_json1) = z_ui2_json=>serialize( data = lt_strings ).
      lv_old += end_measurement( lv_start ).

      lv_start = start_measurement( ).
      DATA(lv_json2) = z_ui2_json2=>serialize( data = lt_strings ).
      lv_new += end_measurement( lv_start ).
    ENDDO.
    APPEND make_runtime( iv_name = |Serialize Strings 10k ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.
    ASSERT lv_json1 = lv_json2.

    " --- Deserialize ---
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      CLEAR lt_strings2.
      lv_start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = lv_json1 CHANGING data = lt_strings2 ).
      lv_old += end_measurement( lv_start ).
      ASSERT lt_strings = lt_strings2.

      CLEAR lt_strings2.
      lv_start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = lv_json2 CHANGING data = lt_strings2 ).
      lv_new += end_measurement( lv_start ).
      ASSERT lt_strings = lt_strings2.
    ENDDO.
    APPEND make_runtime( iv_name = |Deserialize Strings 10k ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.

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

    DATA: lt_deep  TYPE STANDARD TABLE OF ty_outer WITH DEFAULT KEY,
          lt_deep2 LIKE lt_deep,
          ls_outer LIKE LINE OF lt_deep,
          ls_inner LIKE LINE OF ls_outer-inner.

    DATA: lv_start TYPE i,
          lv_old   TYPE i,
          lv_new   TYPE i.

    DO 1000 TIMES.
      ls_outer-id = sy-index.
      ls_outer-name = |Object { sy-index }|.
      CLEAR ls_outer-inner.
      DO 10 TIMES.
        ls_inner-field1 = |Inner { sy-index }|.
        ls_inner-field2 = sy-index * 100.
        ls_inner-field3 = sy-datum.
        APPEND ls_inner TO ls_outer-inner.
      ENDDO.
      APPEND ls_outer TO lt_deep.
    ENDDO.

    DATA(lv_times) = 5.

    " --- Serialize ---
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      lv_start = start_measurement( ).
      DATA(lv_json1) = z_ui2_json=>serialize(
        data = lt_deep compress = abap_true pretty_name = z_ui2_json=>pretty_mode-camel_case ).
      lv_old += end_measurement( lv_start ).

      lv_start = start_measurement( ).
      DATA(lv_json2) = z_ui2_json2=>serialize(
        data = lt_deep compress = abap_true pretty_name = z_ui2_json2=>pretty_mode-camel_case ).
      lv_new += end_measurement( lv_start ).
    ENDDO.
    APPEND make_runtime( iv_name = |Serialize Deep 1k*10 ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.
    ASSERT lv_json1 = lv_json2.

    " --- Deserialize ---
    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      CLEAR lt_deep2.
      lv_start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = lv_json1
        pretty_name = z_ui2_json=>pretty_mode-camel_case CHANGING data = lt_deep2 ).
      lv_old += end_measurement( lv_start ).
      ASSERT lt_deep = lt_deep2.

      CLEAR lt_deep2.
      lv_start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = lv_json2
        pretty_name = z_ui2_json2=>pretty_mode-camel_case CHANGING data = lt_deep2 ).
      lv_new += end_measurement( lv_start ).
      ASSERT lt_deep = lt_deep2.
    ENDDO.
    APPEND make_runtime( iv_name = |Deserialize Deep 1k*10 ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.

  ENDMETHOD.

  METHOD perf_generate.

    DATA: lv_start TYPE i,
          lv_old   TYPE i,
          lv_new   TYPE i.

    DATA: lo_data1 TYPE REF TO data,
          lo_data2 TYPE REF TO data.

    SELECT * FROM sbook UP TO 5000 ROWS INTO TABLE @DATA(lt_sbook) ORDER BY PRIMARY KEY.
    DATA(lv_json) = z_ui2_json=>serialize( data = lt_sbook ).

    DATA(lv_times) = 3.

    CLEAR: lv_new, lv_old.
    DO lv_times TIMES.
      CLEAR lo_data1.
      lv_start = start_measurement( ).
      z_ui2_json=>deserialize( EXPORTING json = lv_json gen_optimize = abap_true CHANGING data = lo_data1 ).
      lv_old += end_measurement( lv_start ).

      CLEAR lo_data2.
      lv_start = start_measurement( ).
      z_ui2_json2=>deserialize( EXPORTING json = lv_json CHANGING data = lo_data2 ).
      lv_new += end_measurement( lv_start ).
    ENDDO.
    APPEND make_runtime( iv_name = |Generate SBOOK { lines( lt_sbook ) } lines ({ lv_times }x)|
      iv_old = lv_old / lv_times iv_new = lv_new / lv_times ) TO rt_result.

    DATA(lv_json1) = z_ui2_json=>serialize( data = lo_data1 ).
    DATA(lv_json2) = z_ui2_json2=>serialize( data = lo_data2 ).
    ASSERT lv_json1 = lv_json2.

  ENDMETHOD.

ENDCLASS.
