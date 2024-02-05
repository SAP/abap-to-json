"! Helper Class for /UI2/CL_JSON
"! Usage examples and documentation can be found on https://github.com/SAP/abap-to-json
class Z_UI2_DATA_ACCESS definition
  public
  create public .

public section.
  type-pools ABAP .

  class-methods CREATE
    importing
      !IR_DATA type ref to DATA optional
      !IV_DATA type DATA optional
      !IV_COMPONENT type STRING optional
    returning
      value(RO_REF) type ref to Z_UI2_DATA_ACCESS .
  methods CONSTRUCTOR
    importing
      !IR_DATA type ref to DATA optional
      !IV_DATA type DATA optional .
  methods AT
    importing
      !IV_COMPONENT type STRING optional
    returning
      value(RO_REF) type ref to Z_UI2_DATA_ACCESS .
  methods EMPTY
    returning
      value(RV_VAL) type ABAP_BOOL .
  methods REF
    returning
      value(RV_DATA) type ref to DATA .
  methods VALUE
    importing
      !IV_DEFAULT type DATA optional
    exporting
      !EV_DATA type DATA .
  methods SET
    importing
      !IV_DATA type DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods COUNT
    returning
      value(RV_LINES) type I .
protected section.

  data MR_DATA type ref to DATA .

  class-methods DEREF
    importing
      !IR_DATA type ref to DATA
    returning
      value(RR_DATA) type ref to DATA .
  methods AT_INT
    importing
      !IV_COMPONENT type STRING optional
      !IV_INDEX type I optional
      !IV_KEYS type STRING optional
    returning
      value(RO_REF) type ref to Z_UI2_DATA_ACCESS .
ENDCLASS.



CLASS Z_UI2_DATA_ACCESS IMPLEMENTATION.


  METHOD AT.

    DATA:
      lv_component TYPE string,
      lv_sindex    TYPE string,
      lv_keys      TYPE string,
      lv_index     TYPE i,
      lt_hier      TYPE match_result_tab.

    FIELD-SYMBOLS:
      <component> LIKE LINE OF lt_hier,
      <sub_match> TYPE LINE OF submatch_result_tab.

    " (?:(\w+)|^)(?:\[(?:(\d+)|([^\]]+))\])? - no check for separators
    FIND ALL OCCURRENCES OF REGEX `(?:([\w\/]+)|^)(?:\[\s*(?:(\d+)|([^\]]+))\s*\])?(?:(?:-\>)|(?:-)|(?:=>)|$)` IN iv_component RESULTS lt_hier ##REGEX_POSIX.

    ro_ref = me.

    LOOP AT lt_hier ASSIGNING <component>.
      CHECK ro_ref->empty( ) EQ abap_false.
      READ TABLE <component>-submatches INDEX 1 ASSIGNING <sub_match>.
      IF <sub_match>-length IS INITIAL.
        CLEAR lv_component.
      ELSE.
        lv_component = iv_component+<sub_match>-offset(<sub_match>-length).
        TRANSLATE lv_component TO UPPER CASE.
      ENDIF.
      READ TABLE <component>-submatches INDEX 2 ASSIGNING <sub_match>.
      IF <sub_match>-length IS INITIAL.
        CLEAR lv_index.
      ELSE.
        lv_index = lv_sindex = iv_component+<sub_match>-offset(<sub_match>-length).
      ENDIF.
      READ TABLE <component>-submatches INDEX 3 ASSIGNING <sub_match>.
      IF <sub_match>-length IS INITIAL.
        CLEAR lv_keys.
      ELSE.
        lv_keys = iv_component+<sub_match>-offset(<sub_match>-length).
      ENDIF.
      ro_ref = ro_ref->at_int( iv_component = lv_component iv_index = lv_index iv_keys = lv_keys ).
    ENDLOOP.

  ENDMETHOD.                    "at_int


METHOD AT_INT.

  DATA: lv_key   TYPE string,
        lv_value TYPE string,
        lt_keys  TYPE match_result_tab,
        lr_res   TYPE REF TO data,
        lr_data  TYPE REF TO data,
        lo_type  TYPE REF TO cl_abap_typedescr.

  FIELD-SYMBOLS: <data>      TYPE data,
                 <comp>      TYPE data,
                 <key>       LIKE LINE OF lt_keys,
                 <sub_match> TYPE LINE OF submatch_result_tab,
                 <table>     TYPE ANY TABLE,
                 <idx_table> TYPE INDEX TABLE.

  IF mr_data IS BOUND.
    IF iv_component IS NOT INITIAL.
      ASSIGN mr_data->* TO <data>.
      ASSIGN COMPONENT iv_component OF STRUCTURE <data> TO <comp>.
      IF <comp> IS ASSIGNED.
        GET REFERENCE OF <comp> INTO lr_data.
        lr_res = deref( lr_data ).
      ENDIF.
    ELSE.
      lr_res = mr_data.
    ENDIF.
  ENDIF.

  IF lr_res IS NOT INITIAL AND ( iv_index IS NOT INITIAL OR iv_keys IS NOT INITIAL ).
    lo_type = cl_abap_typedescr=>describe_by_data_ref( lr_res ).
    IF lo_type->kind EQ cl_abap_typedescr=>kind_table.
      " check for table index access
      IF iv_index IS NOT INITIAL.
        ASSIGN lr_res->* TO <idx_table>.
        IF sy-subrc IS INITIAL.
          READ TABLE <idx_table> INDEX iv_index REFERENCE INTO lr_res.
          IF sy-subrc IS NOT INITIAL.
            CLEAR lr_res.
          ENDIF.
        ELSE.
          CLEAR lr_res.
        ENDIF.
      ELSE. " iv_keys IS NOT INITIAL
        ASSIGN lr_res->* TO <table>.
        IF sy-subrc IS INITIAL.
          CREATE DATA lr_data LIKE LINE OF <table>.
          ASSIGN lr_data->* TO <data>.
          FIND ALL OCCURRENCES OF REGEX `(\w+)\s*=\s*([^,]*),?` IN iv_keys RESULTS lt_keys ##REGEX_POSIX.
          IF sy-subrc IS INITIAL.
            LOOP AT lt_keys ASSIGNING <key>.
              READ TABLE <key>-submatches INDEX 1 ASSIGNING <sub_match>.
              lv_key = iv_keys+<sub_match>-offset(<sub_match>-length).
              TRANSLATE lv_key TO UPPER CASE.
              READ TABLE <key>-submatches INDEX 2 ASSIGNING <sub_match>.
              lv_value = iv_keys+<sub_match>-offset(<sub_match>-length).
              ASSIGN COMPONENT lv_key OF STRUCTURE <data> TO <comp>.
              CHECK sy-subrc IS INITIAL.
              <comp> = lv_value.
            ENDLOOP.
          ELSE.
            <data> = lv_key.
          ENDIF.
          READ TABLE <table> FROM <data> REFERENCE INTO lr_res.
          IF sy-subrc IS NOT INITIAL.
            CLEAR lr_res.
          ENDIF.
        ELSE.
          CLEAR lr_res.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CREATE OBJECT ro_ref
    EXPORTING
      ir_data = lr_res.

ENDMETHOD.                    "at_int


  METHOD CONSTRUCTOR.
    IF ir_data IS NOT INITIAL.
      mr_data = ir_data.
    ELSEIF iv_data IS SUPPLIED.
      GET REFERENCE OF iv_data INTO mr_data.
    ENDIF.
    mr_data = deref( mr_data ).
  ENDMETHOD.                    "constructor


METHOD COUNT.

  FIELD-SYMBOLS: <tab> TYPE ANY TABLE.

  IF mr_data IS BOUND.
    ASSIGN mr_data->* TO <tab>.
    IF sy-subrc IS INITIAL.
      rv_lines = lines( <tab> ).
    ENDIF.
  ENDIF.

ENDMETHOD.


  METHOD CREATE.
    IF iv_data IS SUPPLIED.
      CREATE OBJECT ro_ref
        EXPORTING
          iv_data = iv_data.
    ELSE.
      CREATE OBJECT ro_ref
        EXPORTING
          ir_data = ir_data.
    ENDIF.

    IF iv_component IS NOT INITIAL.
      ro_ref = ro_ref->at( iv_component ).
    ENDIF.
  ENDMETHOD.                    "create


  METHOD DEREF.

    DATA: lo_type TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <data> TYPE data.

    rr_data = ir_data.
    IF rr_data IS NOT INITIAL.
      lo_type = cl_abap_typedescr=>describe_by_data_ref( ir_data ).
      IF lo_type->kind EQ cl_abap_typedescr=>kind_ref.
        ASSIGN ir_data->* TO <data>.
        rr_data = deref( <data> ).
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "deref


  METHOD EMPTY.
    IF mr_data IS INITIAL.
      rv_val = abap_true.
    ENDIF.
  ENDMETHOD.                    "empty


  METHOD REF.
    rv_data = mr_data.
  ENDMETHOD.                    "ref


  METHOD SET.

    FIELD-SYMBOLS: <data> TYPE data.

    IF mr_data IS BOUND.
      ASSIGN mr_data->* TO <data>.
      <data> = iv_data.
      rv_success = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD VALUE.

    DATA: lo_type_out TYPE REF TO cl_abap_typedescr,
          lo_type_in  TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <data> TYPE data.

    CLEAR ev_data.

    IF mr_data IS BOUND.
      ASSIGN mr_data->* TO <data>.
      lo_type_out = cl_abap_typedescr=>describe_by_data( ev_data ).
      lo_type_in = cl_abap_typedescr=>describe_by_data( <data> ).
      IF lo_type_out->kind EQ lo_type_in->kind.
        ev_data = <data>.
      ENDIF.
    ELSEIF iv_default IS SUPPLIED.
      ev_data = iv_default.
    ENDIF.

  ENDMETHOD.                    "value
ENDCLASS.
