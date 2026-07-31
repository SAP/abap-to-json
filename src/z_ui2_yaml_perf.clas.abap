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

ENDCLASS.


CLASS z_ui2_yaml_perf IMPLEMENTATION.

  METHOD run.
    APPEND LINES OF perf_serialize( )   TO rt_result.
    APPEND LINES OF perf_deserialize( ) TO rt_result.
    APPEND LINES OF perf_generate( )    TO rt_result.
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

ENDCLASS.
