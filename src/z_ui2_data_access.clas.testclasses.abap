*"* use this source file for your ABAP unit test classes
CLASS lc_ut DEFINITION FOR TESTING FINAL "#AU Duration Medium
  "#AU Risk_Level Harmless
INHERITING FROM z_ui2_data_access.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_properties,
        enabled     TYPE abap_bool,
        length      TYPE i,
        description TYPE c LENGTH 20,
      END OF t_properties,
      BEGIN OF t_data,
        id         TYPE i,
        properties TYPE t_properties,
        content    TYPE string,
      END OF t_data,
      BEGIN OF t_name_value,
        name  TYPE string,
        value TYPE string,
      END OF t_name_value,
      BEGIN OF t_example,
        flag        TYPE abap_bool,
        /bic/z_year TYPE i,
        props       TYPE t_data,
        params      TYPE SORTED TABLE OF t_name_value WITH UNIQUE KEY name,
        params2     TYPE SORTED TABLE OF t_name_value WITH UNIQUE KEY name value,
      END OF t_example.

    METHODS: test_basic_actions FOR TESTING.
    METHODS: test_table_access FOR TESTING.
    METHODS: test_table_access2 FOR TESTING.
    METHODS: test_table_access3 FOR TESTING.

ENDCLASS.       "lc_ut


CLASS lc_ut IMPLEMENTATION.

  METHOD test_basic_actions.

    DATA: ls_data TYPE t_example,
          lr_data TYPE REF TO data,
          lr_ref  TYPE REF TO data,
          lv_int  TYPE i,
          lo_data TYPE REF TO z_ui2_data_access,
          lo_tmp  LIKE lo_data.

    FIELD-SYMBOLS: <data> TYPE data.

    ls_data-props-id                     = 12345.
    ls_data-/bic/z_year                  = 1978.
    ls_data-props-content                = `Some Content`.
    ls_data-props-properties-enabled     = abap_false.
    ls_data-props-properties-length      = 10.
    ls_data-props-properties-description = `My description`.

    GET REFERENCE OF ls_data INTO lr_data.

    CREATE OBJECT lo_data EXPORTING ir_data = lr_data.

    " standard way (does not work on SAP_BASIS 700)
    lo_tmp = lo_data->at( `PROPS` ).
    lo_tmp = lo_tmp->at( `id` ).
    lr_ref = lo_tmp->ref( ).
    " lr_ref = lo_data->at(`PROPS`)->at(`id`)->ref( ).    => 700
    cl_aunit_assert=>assert_bound( act = lr_ref msg = `Dynamic access to existing property fails!` ).

    ASSIGN lr_ref->* TO <data>.
    cl_aunit_assert=>assert_equals( act = <data> exp = ls_data-props-id msg = `Dynamic access to existing property fails!` ).

    lo_tmp = lo_data->at( `PROPS` ).
    lo_tmp = lo_tmp->at( `key` ).
    lr_ref = lo_tmp->ref( ).
    " lr_ref = lo_data->at(`PROPS`)->at(`key`)->ref( ). => 700
    cl_aunit_assert=>assert_not_bound( act = lr_ref msg = `Dynamic access to NOT existing property fails!` ).

    " XPath like
    lo_tmp = lo_data->at( `PROPS-ID` ).
    lr_ref = lo_tmp->ref( ).
    " lr_ref = lo_data->at(`PROPS-ID`)->ref( ). => 700
    cl_aunit_assert=>assert_bound( act = lr_ref msg = `Dynamic access to existing property fails!` ).

    ASSIGN lr_ref->* TO <data>.
    cl_aunit_assert=>assert_equals( act = <data> exp = ls_data-props-id msg = `Dynamic access to existing property fails!` ).

    " Alternative component separator
    lo_tmp = lo_data->at( `PROPS->ID` ).
    lr_ref = lo_tmp->ref( ).
    " lr_ref = lo_data->at(`PROPS->ID`)->ref( ). => 700
    cl_aunit_assert=>assert_bound( act = lr_ref msg = `Dynamic access to existing property fails!` ).

    ASSIGN lr_ref->* TO <data>.
    cl_aunit_assert=>assert_equals( act = <data> exp = ls_data-props-id msg = `Dynamic access to existing property fails!` ).

    " access for attributes with special characters
    lo_tmp = lo_data->at( `/BIC/Z_YEAR` ).
    lr_ref = lo_tmp->ref( ).
    " lr_ref = lo_data->at(`PROPS-ID`)->ref( ). => 700
    cl_aunit_assert=>assert_bound( act = lr_ref msg = `Dynamic access to existing property with special characters fails!` ).

    ASSIGN lr_ref->* TO <data>.
    cl_aunit_assert=>assert_equals( act = <data> exp = ls_data-/bic/z_year msg = `Dynamic access to existing property with special characters fails!` ).

    " using helper method for creation
    lo_tmp = create( ir_data = lr_data iv_component = `PROPS-ID` ).
    lr_ref = lo_tmp->ref( ).
    " lr_ref = create( ir_data = lr_data iv_component = `PROPS-ID`)->ref( ). => 700
    cl_aunit_assert=>assert_bound( act = lr_ref msg = `Dynamic access to existing property fails!` ).

    ASSIGN lr_ref->* TO <data>.
    cl_aunit_assert=>assert_equals( act = <data> exp = ls_data-props-id msg = `Dynamic access to existing property fails!` ).

    " reading value
    lo_tmp = lo_data->at( `props-properties-length` ).
    lo_tmp->value( IMPORTING ev_data = lv_int ).
    " lo_data->at(`props-properties-length`)->value( IMPORTING ev_data = lv_int ). => 700
    cl_aunit_assert=>assert_equals( act = lv_int exp = ls_data-props-properties-length msg = `Dynamic read of existing the property fails!` ).

    lv_int = 25.
    lo_tmp = lo_data->at( `props-properties-len` ).
    lo_tmp->value( IMPORTING ev_data = lv_int ).
    " lo_data->at(`props-properties-len`)->value( IMPORTING ev_data = lv_int ). => 700
    cl_aunit_assert=>assert_initial( act = lv_int msg = `Dynamic read of the NOT existing property fails!` ).

    " modifing value
    lv_int = 25.
    lo_tmp = lo_data->at( `props-properties-length` ).
    lr_ref = lo_tmp->ref( ).
    " lr_ref = lo_data->at(`props-properties-length`)->ref( ). => 700
    ASSIGN lr_ref->* TO <data>.
    <data> = lv_int.
    cl_aunit_assert=>assert_equals( act = lv_int exp = ls_data-props-properties-length msg = `Dynamic modification of the existing property fails!` ).

    lv_int = 15.
    lo_tmp = lo_data->at( `props-properties-length` ).
    lo_tmp->set( lv_int ).
    " lo_data->at(`props-properties-length`)->set( lv_int ). => 700
    cl_aunit_assert=>assert_equals( act = lv_int exp = ls_data-props-properties-length msg = `Dynamic modification of the existing property fails!` ).

    " degenerated example
    lv_int = 15.
    lo_tmp = create( iv_data = lv_int ).
    lr_ref = lo_tmp->ref( ).
    " lr_ref = create( iv_data = lv_int )->ref( ). => 700
    ASSIGN lr_ref->* TO <data>.
    cl_aunit_assert=>assert_equals( act = <data> exp = lv_int msg = `Dynamic access of existing the property fails!` ).

  ENDMETHOD.

  METHOD test_table_access.

    DATA:
      ls_data  TYPE t_example,
      ls_line  LIKE LINE OF ls_data-params,
      lv_value TYPE string,
      lo_data  TYPE REF TO z_ui2_data_access.

    ls_line-name = `KEY1`.
    ls_line-value = `Value1`.
    INSERT ls_line INTO TABLE ls_data-params.
    INSERT ls_line INTO TABLE ls_data-params2.

    ls_line-name = `KEY2`.
    ls_line-value = `Value1`.
    INSERT ls_line INTO TABLE ls_data-params.
    INSERT ls_line INTO TABLE ls_data-params2.

    ls_line-name = `KEY2`.
    ls_line-value = `Value2`.
    INSERT ls_line INTO TABLE ls_data-params.
    INSERT ls_line INTO TABLE ls_data-params2.

    ls_line-name = `KEY3`.
    ls_line-value = `Value1, Value2`.
    INSERT ls_line INTO TABLE ls_data-params.
    INSERT ls_line INTO TABLE ls_data-params2.

    lo_data = create( iv_data = ls_data iv_component = `params[2]-name` ).
    lo_data->value( IMPORTING ev_data = lv_value ).

    cl_aunit_assert=>assert_equals( act = lv_value exp = `KEY2` msg = `Dynamic read to table item by index fails!` ).

    lo_data = create( iv_data = ls_data iv_component = `params[name=KEY1]-value` ).
    lo_data->value( IMPORTING ev_data = lv_value ).

    cl_aunit_assert=>assert_equals( act = lv_value exp = `Value1` msg = `Dynamic read to table item by key index fails!` ).

    lo_data = create( iv_data = ls_data iv_component = `params` ).
    lo_data = lo_data->at( `[name=KEY1]-value` ).
    lo_data->value( IMPORTING ev_data = lv_value ).

    cl_aunit_assert=>assert_equals( act = lv_value exp = `Value1` msg = `Dynamic read to table item by key index fails!` ).

    lo_data = create( iv_data = ls_data iv_component = `params2[name=KEY2, value=Value2]-value` ).
    lo_data->value( IMPORTING ev_data = lv_value ).

    cl_aunit_assert=>assert_equals( act = lv_value exp = `Value2` msg = `Dynamic read to table item by key index fails!` ).

    " we do not support usage usage of "," or "]" as part of the key :(
    lo_data = create( iv_data = ls_data iv_component = `params2[name=KEY3, value=Value1, Value2]-value` ).
    lo_data->value( IMPORTING ev_data = lv_value ).

    cl_aunit_assert=>assert_equals( act = lv_value exp = `` msg = `Dynamic read to table item by unescaped key works!` ).

  ENDMETHOD.

  METHOD test_table_access2.

    DATA: lr_data TYPE REF TO data,
          lr_ref  TYPE REF TO data,
          lo_data TYPE REF TO z_ui2_data_access,
          lv_json TYPE z_ui2_json=>json.

    lv_json = `{"type": "UPDATE","organization": {"duns": "429773946"},"elements": [{"element": "organization","current": null,"timestamp": "2019-05-01T19:56:20Z"}]}`.
    lr_data = z_ui2_json=>generate( json = lv_json ).
    lo_data = create( ir_data = lr_data iv_component =  'elements[1]-current[1]-value' ).
    lr_ref  = lo_data->ref( ).

    cl_aunit_assert=>assert_not_bound( act = lr_ref msg = `Access to null array fails!` ).

  ENDMETHOD.

  METHOD test_table_access3.

    DATA: access    TYPE REF TO z_ui2_data_access,
          itab      TYPE REF TO z_ui2_data_access,
          value     TYPE REF TO z_ui2_data_access,
          lr_data   TYPE REF TO data,
          index_act TYPE string,
          index_exp TYPE string,
          lv_key    TYPE string,
          json      TYPE z_ui2_json=>json.

    CONCATENATE
    `{ "SSDC": {`
    `"checks_for_new_checkset": [`
    `{ "check_name": "OLC_1", "sort_order": "1", "mandatory": "X" },`
    `{ "check_name": "OLC_2", "sort_order": "2", "mandatory": " " },`
    `{ "check_name": "OLC_3", "sort_order": "3", "mandatory": "X" } ] } }`
    INTO json.

    lr_data = z_ui2_json=>generate( json = json ).
    access = create( ir_data = lr_data ).
    itab = access->at( `SSDC-checks_for_new_checkset` ).

    DO 3 TIMES.
      index_exp = sy-index.
      CONDENSE index_exp.
      CONCATENATE '[' index_exp ']-sort_order' INTO lv_key.
      value = itab->at( lv_key ).
      value->value( IMPORTING ev_data = index_act ).
      cl_aunit_assert=>assert_equals( act = index_act exp = index_exp msg = `Access with index fails!` ).
    ENDDO.

  ENDMETHOD.

ENDCLASS.
