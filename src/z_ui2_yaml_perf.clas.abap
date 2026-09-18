CLASS z_ui2_yaml_perf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_runtime,
             name       TYPE string,
             usec       TYPE i,
           END OF ty_runtime.

    TYPES tt_runtime TYPE STANDARD TABLE OF ty_runtime WITH DEFAULT KEY.

    CLASS-METHODS run
      RETURNING VALUE(rt_result) TYPE tt_runtime.

  PRIVATE SECTION.

    CLASS-METHODS start_measurement
      RETURNING VALUE(rv_start) TYPE i.

    CLASS-METHODS end_measurement
      IMPORTING iv_start           TYPE i
      RETURNING VALUE(rv_duration) TYPE i.

    CLASS-METHODS perf_serialize
      RETURNING VALUE(rt_result) TYPE tt_runtime.

    CLASS-METHODS perf_deserialize
      RETURNING VALUE(rt_result) TYPE tt_runtime.

    CLASS-METHODS perf_generate
      RETURNING VALUE(rt_result) TYPE tt_runtime.

    CLASS-METHODS perf_deserialize_large
      RETURNING VALUE(rt_result) TYPE tt_runtime.

    CLASS-METHODS perf_generate_large
      RETURNING VALUE(rt_result) TYPE tt_runtime.

    CLASS-METHODS build_large_yaml
      IMPORTING iv_rows        TYPE i
      RETURNING VALUE(rv_yaml) TYPE string.

ENDCLASS.


CLASS z_ui2_yaml_perf IMPLEMENTATION.

  METHOD run.
    APPEND LINES OF perf_serialize( )         TO rt_result.
    APPEND LINES OF perf_deserialize( )       TO rt_result.
    APPEND LINES OF perf_generate( )          TO rt_result.
    APPEND LINES OF perf_deserialize_large( ) TO rt_result.
    APPEND LINES OF perf_generate_large( )    TO rt_result.
  ENDMETHOD.

  METHOD start_measurement.
    GET RUN TIME FIELD rv_start.
  ENDMETHOD.

  METHOD end_measurement.
    DATA(lv_now) = 0.
    GET RUN TIME FIELD lv_now.
    rv_duration = lv_now - iv_start.
  ENDMETHOD.

  METHOD perf_serialize.
    TYPES: BEGIN OF ty_s, host TYPE string, port TYPE i, enabled TYPE abap_bool, END OF ty_s.
    TYPES: BEGIN OF ty_w, name TYPE string, replicas TYPE i, servers TYPE STANDARD TABLE OF ty_s
                                                              WITH DEFAULT KEY, END OF ty_w.

    DATA(cfg) = VALUE ty_w(
      name     = `production`
      replicas = 3
      servers  = VALUE #(
        ( host = `db1.example.com` port = 5432 enabled = abap_true )
        ( host = `db2.example.com` port = 5432 enabled = abap_true )
        ( host = `db3.example.com` port = 5432 enabled = abap_false )
      )
    ).

    DATA lv_start TYPE i.
    DATA lv_total TYPE i.
    DATA(lv_times) = 1000.

    CLEAR lv_total.
    DO lv_times TIMES.
      lv_start = start_measurement( ).
      DATA(lv_yaml) = z_ui2_yaml=>serialize( data = cfg pretty_name = z_ui2_yaml=>pretty_mode-low_case ).
      lv_total += end_measurement( lv_start ).
    ENDDO.
    APPEND VALUE #( name = |Serialize small config ({ lv_times }x)| usec = lv_total / lv_times )
           TO rt_result.
  ENDMETHOD.

  METHOD perf_deserialize.
    DATA(lv_yaml) = |name: production\nreplicas: 3\nservers:\n|
                 && |  - host: db1.example.com\n    port: 5432\n    enabled: true\n|
                 && |  - host: db2.example.com\n    port: 5432\n    enabled: true\n|
                 && |  - host: db3.example.com\n    port: 5432\n    enabled: false|.

    TYPES: BEGIN OF ty_s, host TYPE string, port TYPE i, enabled TYPE abap_bool, END OF ty_s.
    TYPES: BEGIN OF ty_w, name TYPE string, replicas TYPE i, servers TYPE STANDARD TABLE OF ty_s
                                                              WITH DEFAULT KEY, END OF ty_w.
    DATA lv_start TYPE i.
    DATA lv_total TYPE i.
    DATA(lv_times) = 1000.

    CLEAR lv_total.
    DO lv_times TIMES.
      DATA cfg TYPE ty_w.
      CLEAR cfg.
      lv_start = start_measurement( ).
      z_ui2_yaml=>deserialize( EXPORTING yaml = lv_yaml CHANGING data = cfg ).
      lv_total += end_measurement( lv_start ).
    ENDDO.
    APPEND VALUE #( name = |Deserialize small config ({ lv_times }x)| usec = lv_total / lv_times )
           TO rt_result.
  ENDMETHOD.

  METHOD perf_generate.
    DATA(lv_yaml) = |name: production\nreplicas: 3\nservers:\n|
                 && |  - host: db1.example.com\n    port: 5432\n|
                 && |  - host: db2.example.com\n    port: 5432\n|
                 && |  - host: db3.example.com\n    port: 5432|.

    DATA lv_start TYPE i.
    DATA lv_total TYPE i.
    DATA(lv_times) = 1000.

    CLEAR lv_total.
    DO lv_times TIMES.
      lv_start = start_measurement( ).
      DATA(lv_ref) = z_ui2_yaml=>generate( lv_yaml ).
      lv_total += end_measurement( lv_start ).
    ENDDO.
    APPEND VALUE #( name = |Generate small config ({ lv_times }x)| usec = lv_total / lv_times )
           TO rt_result.
  ENDMETHOD.

  METHOD build_large_yaml.
    " Build top-level sequence: rows of { host, port, enabled }.
    " Uses string_table + CONCATENATE LINES OF for efficient O(n) construction.
    DATA lt_lines TYPE string_table.
    DO iv_rows TIMES.
      APPEND |- host: h{ sy-index }| TO lt_lines.
      APPEND |  port: 5432| TO lt_lines.
      APPEND |  enabled: true| TO lt_lines.
    ENDDO.
    CONCATENATE LINES OF lt_lines INTO rv_yaml SEPARATED BY cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD perf_deserialize_large.
    TYPES: BEGIN OF ty_s, host TYPE string, port TYPE i, enabled TYPE abap_bool, END OF ty_s.
    DATA lt_out TYPE STANDARD TABLE OF ty_s WITH DEFAULT KEY.
    DATA lv_start TYPE i.
    DATA lv_total TYPE i.

    " 10k rows, 3 iterations
    DATA(lv_yaml_10k)  = build_large_yaml( 10000 ).
    DATA(lv_iters_10k) = 3.
    CLEAR lv_total.
    DO lv_iters_10k TIMES.
      CLEAR lt_out.
      lv_start = start_measurement( ).
      z_ui2_yaml=>deserialize( EXPORTING yaml = lv_yaml_10k CHANGING data = lt_out ).
      lv_total += end_measurement( lv_start ).
    ENDDO.
    APPEND VALUE #( name = |Deserialize 10k rows ({ lv_iters_10k }x)| usec = lv_total / lv_iters_10k )
           TO rt_result.

    " 100k rows, 1 iteration (single-op wall-clock)
    DATA(lv_yaml_100k) = build_large_yaml( 100000 ).
    CLEAR lt_out.
    lv_start = start_measurement( ).
    z_ui2_yaml=>deserialize( EXPORTING yaml = lv_yaml_100k CHANGING data = lt_out ).
    APPEND VALUE #( name = `Deserialize 100k rows (1x)` usec = end_measurement( lv_start ) )
           TO rt_result.
  ENDMETHOD.

  METHOD perf_generate_large.
    DATA lv_start TYPE i.
    DATA lv_total TYPE i.
    DATA lv_result TYPE REF TO data.

    " 10k rows, 2 iterations
    DATA(lv_yaml_10k)  = build_large_yaml( 10000 ).
    DATA(lv_iters_10k) = 2.
    CLEAR lv_total.
    DO lv_iters_10k TIMES.
      lv_start = start_measurement( ).
      lv_result = z_ui2_yaml=>generate( lv_yaml_10k ).
      lv_total += end_measurement( lv_start ).
    ENDDO.
    APPEND VALUE #( name = |Generate 10k rows ({ lv_iters_10k }x)| usec = lv_total / lv_iters_10k )
           TO rt_result.

    " 100k rows, 1 iteration
    DATA(lv_yaml_100k) = build_large_yaml( 100000 ).
    lv_start = start_measurement( ).
    lv_result = z_ui2_yaml=>generate( lv_yaml_100k ).
    APPEND VALUE #( name = `Generate 100k rows (1x)` usec = end_measurement( lv_start ) )
           TO rt_result.
  ENDMETHOD.

ENDCLASS.
