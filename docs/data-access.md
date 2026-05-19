# Dynamic Data Accessor Helper Class for ABAP

> **Class names**: The SAP-delivered class is `/UI2/CL_DATA_ACCESS`. The open-source Z* copy in this repository is `Z_UI2_DATA_ACCESS`. All code examples on this page use the `/UI2/CL_DATA_ACCESS` name — substitute `Z_UI2_DATA_ACCESS` if you are working with the local copy.

# Why
Sometimes you need to access [ABAP data objects dynamically](https://help.sap.com/http.svc/rc/abapdocu_751_index_htm/7.51/en-US/abendyn_access_data_obj_guidl.htm). For example, when:
* You do not know the structure of the ABAP object, and to dynamically access the value of the field, you are forced to use ASSIGN .. COMPONENT with a field symbol
* You do a cross-release development in ABAP and do not know if some field exists, so you need dynamic access to data
*	You access data from the optional ABAP component and also need dynamic access to the ABAP structure field or internal table row
*	You need a key or index access to a dynamic internal table

Normally, if you need to access ABAP fields/structure components dynamically, you will end up coding like this:

## Accessing Data Objects Dynamically (as suggested in the documentation) 
```abap
FIELD-SYMBOLS: <table>  TYPE ANY TABLE,
               <line>   TYPE ANY,
               <field>  TYPE ANY.

ASSIGN COMPONENT `FIELD1` OF STRUCTURE <line> TO <field>.
IF <field> IS ASSIGNED.
  WRITE: <field>.
ENDIF.
```
That may be OK (however, still not very convenient), but if you have a dynamic ABAP object with a deeper structure, it becomes boring:

## Accessing Deeply Nested Data Objects Dynamically  
```abap
FIELD-SYMBOLS: 
  <table>  TYPE ANY TABLE,
  <line>   TYPE ANY,
  <field1>  TYPE ANY,
  <field2>  TYPE ANY,
  <field3>  TYPE ANY,
  <field4>  TYPE ANY.
 
ASSIGN COMPONENT `FIELD1` OF STRUCTURE <line> TO <field1>.
IF <field> IS ASSIGNED.
  ASSIGN COMPONENT `CHILD_1` OF STRUCTURE <fielld1> TO <field2>.
  IF <field2> IS ASSIGNED.
    ASSIGN COMPONENT `CHILD_11` OF STRUCTURE <fielld2> TO <field3>.
    IF <field3> IS ASSIGNED.
      ASSIGN COMPONENT `CHILD_111` OF STRUCTURE <fielld3> TO <field4>.
      IF <field4> IS ASSIGNED.
        WRITE: <field4>.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
```

And if one thinks about accessing dynamically elements from nested tables, it would be at all - hell.
So, below one can find a helper class, which may help in such cases, in a lean and tasty way.  
An original and actual version of the source can be found in class `/UI2/CL_DATA_ACCESS` delivered with the UI2 Add-on. The open-source Z* copy (`Z_UI2_DATA_ACCESS`) is in this repository. Chained method calls (`->at(...)-> at(...)`) require SAP_BASIS 7.02 or higher. Delivered with note [2526405](https://launchpad.support.sap.com/#/notes/2526405).

# What it can
The accessor is a single class, with the following features:
*	traversing of any data object (classes are not yet supported) passed as a reference to data or as data.
*	traversing nested objects without any level limitations
*	automatically resolving reference variables (REF TO fields accessed the same way as standard fields)
*	a method, like an XPath-like way of accessing data
*	read/modification access to elementary types
*	accessing table rows using index or key

## Limitations
The code of the dynamic data accessor class does not pretend to be a complete and fully robust solution, but it may become like this if requests come  
Current limitations are the following:
*	Only ABAP data structures are supported. Traversing of ABAP objects/classes is not yet supported; however, possible
*	Dynamic modification of tables (not data inside) is not supported. e.g., you can not add/remove rows with API, but you can do it via reference to the table
*	The syntax for key access of the rows in dynamic tables does not allow usage of the symbol "," as part of the query. No escaping is supported. 

# Usage
## Necessary type and data declarations for example below
```abap
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
   flag   TYPE abap_bool,
   props  TYPE t_data,
   params TYPE SORTED TABLE OF t_name_value WITH UNIQUE KEY name,
 END OF t_example.
 
DATA: ls_data TYPE t_example,
      lr_data TYPE REF TO data,
      lr_ref  TYPE REF TO data,
      lv_int  TYPE i,
      lo_data TYPE REF TO /ui2/cl_data_access.

FIELD-SYMBOLS: <data> TYPE data.

ls_data-props-id      = 12345.
ls_data-props-content = `Some Content`.
ls_data-props-properties-enabled     = abap_false.
ls_data-props-properties-length      = 10.
ls_data-props-properties-description = `My description`.

GET REFERENCE OF ls_data INTO lr_data.
```

## Usage examples

### Traversing ABAP data dynamically 
Simplest usage example:
```abap
CREATE OBJECT lo_data EXPORTING ir_data = lr_data.

" chained method calls (requires SAP_BASIS 7.02+)
lr_ref = lo_data->at(`PROPS`)->at(`id`)->ref( ).
IF lr_ref IS BOUND.
  ASSIGN lr_ref->* TO <data>.
  WRITE: <data>.
ENDIF.
" XPath like
lr_ref = lo_data->at(`PROPS-ID`)->ref( ).
IF lr_ref IS BOUND.
  ASSIGN lr_ref->* TO <data>.
  WRITE: <data>.
ENDIF.

" using helper method for creation
lr_ref = /ui2/cl_data_access=>create( ir_data = lr_data iv_component = `PROPS-ID`)->ref( ).
IF lr_ref IS BOUND.
  ASSIGN lr_ref->* TO <data>.
  WRITE: <data>.
ENDIF.

" reading value directly
lo_data->at(`props-properties-length`)->value( IMPORTING ev_data = lv_int ).
WRITE: lv_int.
```

### Dynamic data modification in ABAP in a nice way 
If you want to use modification operations, you can only operate with references when creating an accessor object.
```abap
" modifing value
lr_ref = lo_data->at(`props-properties-length`)->ref( ).
ASSIGN lr_ref->* TO <data>.
<data> = 25.

" or even more simple
lo_data->at(`props-properties-length`)->set( 15 ).
```

### Accessing not existing component 
The code is robust - accessing existing components of any level will not result in a crash but will return an empty reference or initial value.
```abap
" reading not existing value returns the initial value
lo_data->at(`props-properties-not_exist-length-not-exist`)->value( IMPORTING ev_data = lv_int ).
WRITE: lv_int. " -> 0
```

### Working with tables:
Dynamic Access in ABAP in an easy way 
```abap
DATA:
  ls_data  TYPE t_example,
  ls_line  LIKE LINE OF ls_data-params,
  lv_value TYPE string,
  lo_data  TYPE REF TO /ui2/cl_data_access.

ls_line-name = `KEY1`.
ls_line-value = `Value1`.
INSERT ls_line INTO TABLE ls_data-params.

ls_line-name = `KEY2`.
ls_line-value = `Value2`.
INSERT ls_line INTO TABLE ls_data-params.

/ui2/cl_data_access=>create( iv_data = ls_data iv_component = `params[2]-name`)->value( IMPORTING ev_data = lv_value ).
WRITE: lv_value.

/ui2/cl_data_access=>create( iv_data = ls_data iv_component = `params[name=KEY1]-value`)->value( IMPORTING ev_data = lv_value ).
WRITE: lv_value.

/ui2/cl_data_access=>create( iv_data = ls_data iv_component = `params`)->at(`[name=KEY1]-value`)->value( IMPORTING ev_data = lv_value ).
WRITE: lv_value.

/ui2/cl_data_access=>create( iv_data = ls_data iv_component = `params[name=KEY1, value=Value1]-value`)->value( IMPORTING ev_data = lv_value ).
WRITE: lv_value.
```

### Fields with special characters in names
Component names containing `/`, `:`, or other special characters work fine — pass the name exactly as it appears in the ABAP structure:
```abap
DATA: ls_data TYPE t_example,
      lv_year TYPE i,
      lo_data TYPE REF TO /ui2/cl_data_access.

ls_data-/bic/z_year = 1978.
GET REFERENCE OF ls_data INTO lr_data.

/ui2/cl_data_access=>create( ir_data = lr_data iv_component = `/BIC/Z_YEAR` )->value( IMPORTING ev_data = lv_year ).
WRITE: lv_year.   " -> 1978
```

### Counting table rows
```abap
DATA: lo_acc  TYPE REF TO /ui2/cl_data_access,
      lv_cnt  TYPE i.

lo_acc = /ui2/cl_data_access=>create( iv_data = ls_data iv_component = `params` ).
lv_cnt = lo_acc->count( ).
WRITE: lv_cnt.   " -> number of rows in ls_data-params
```

### VALUE with a default
When the accessor is not bound (component not found), `VALUE` returns initial by default. Pass `IV_DEFAULT` to get a fallback value instead:
```abap
DATA: lv_val TYPE string.
/ui2/cl_data_access=>create( iv_data = ls_data iv_component = `params[name=MISSING]-value` )
  ->value( EXPORTING iv_default = `(not found)` IMPORTING ev_data = lv_val ).
WRITE: lv_val.   " -> (not found)
```

### Robustness: accessing non-existent paths
The accessor never dumps — accessing a missing component at any depth returns an unbound accessor:
```abap
DATA: lv_int TYPE i.
/ui2/cl_data_access=>create( iv_data = ls_data
  iv_component = `props-properties-no_such_field-deep-deeper` )
  ->value( IMPORTING ev_data = lv_int ).
WRITE: lv_int.   " -> 0  (initial, no exception)
```

### Iterating table rows dynamically
When you already have an accessor pointing at a table, call `at('[index]-field')` directly on it in a loop — no need to re-resolve from the root each time:
```abap
DATA: lr_data TYPE REF TO data,
      lo_acc  TYPE REF TO /ui2/cl_data_access,
      lo_rows TYPE REF TO /ui2/cl_data_access,
      lv_key  TYPE string,
      lv_val  TYPE string.

lr_data = /ui2/cl_json=>generate( json = `{"items":[{"id":"A"},{"id":"B"},{"id":"C"}]}` ).
lo_acc  = /ui2/cl_data_access=>create( ir_data = lr_data ).
lo_rows = lo_acc->at( `items` ).          " accessor points at the array

DO 3 TIMES.
  lv_key = |[{ sy-index }]-id|.           " e.g. "[1]-id"
  lv_val = ``.
  lo_rows->at( lv_key )->value( IMPORTING ev_data = lv_val ).
  WRITE: / lv_val.                        " -> A, B, C
ENDDO.
```

### End-to-end: generate from JSON then navigate
Combining `Z_UI2_JSON=>GENERATE` (or `/UI2/CL_JSON=>GENERATE`) with the data accessor is the recommended way to work with unknown JSON structures:
```abap
DATA: lr_data TYPE REF TO data,
      lv_val  TYPE string.

lr_data = /ui2/cl_json=>generate(
  json       = `{"type":"UPDATE","org":{"duns":"429773946"}}`
  pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

/ui2/cl_data_access=>create( ir_data = lr_data iv_component = `ORG-DUNS` )
  ->value( IMPORTING ev_data = lv_val ).
WRITE: lv_val.   " -> 429773946
```

# API description 

## CREATE - Static Method Public Helper method for creating an instance of a dynamic accessor
*	\> IR_DATA (ref to data) - Importing Type Ref To DATA Reference to data (allows modification of embedded data)
*	\> IV_DATA (data) - any data (modification of embedded data not allowed)
*	\> IV_COMPONENT (string) - Sub-component name (XPath-like syntax is supported)
*	\< RO_REF (ref to /ui2/cl_data_access) - Reference to accessor object pointing to subcomponent

## CONSTRUCTOR   - Instance Method Public Constructor
*	\> IR_DATA (ref to data, optional) - Reference to data. The reference is automatically dereferenced if it points to another reference. Use this form when you need `SET`/`REF` to modify the original variable.
*	\> IV_DATA (data, optional) - Data passed by value. Modification of the original variable through `SET` is **not** possible with this form — use `IR_DATA` instead.

## AT  - Instance Method Public Component accessor
*	\> IV_COMPONENT (string, optional) - Sub-component name (XPath-like syntax is supported). If omitted, returns the current accessor — useful for chaining after a table index/key lookup.
*	\< RO_REF (ref to /ui2/cl_data_access) - Reference to accessor object pointing to subcomponent

## EMPTY  - Instance Method Public Returns TRUE if the embedded object is initial (not bound)
*	\< RV_VAL (boolean) - ABAP_TRUE if the object is initial and data is not bound

## REF  - Instance Method Public Returns a reference to the embedded object
*	\< RV_DATA (ref to data) - Reference to embedded data

## VALUE  - Instance Method Public Returns a copy of the value
*	\> IV_DEFAULT (data, optional) - Default value returned in EV_DATA when the accessor is not bound (empty)
*	\< EV_DATA (data) - Copy of the embedded data value; initial (or IV_DEFAULT if supplied) if data is not bound

## SET  - Instance Method Public Sets the value of the embedded object, if not initial
*	\> IV_DATA (data) - New value for embedded object
*	\< RV_SUCCESS (boolean) - ABAP_TRUE, if data was successfully modified

## COUNT  - Instance Method Public Returns the number of rows in the referenced table
*	\< RV_LINES (i) - Number of table rows; 0 if the data is not bound or is not a table

# XPath-like dynamic data access
To access nested components, you can use a nice, object-oriented way, using nested calls of the AT method (it is robust, and would not crash accessing non-existent components), as
```abap 
lo_object->at('subcomp1')->at('subcomp11')->...
```

But if you prefer a more compact form, or run code on SAP_BASIS < 702 (nested method calls are not supported), you may use XPath-like syntax for accessing components. Like this:
```abap
lo_object->at('subcomp1-subcomp11-subcomp111')
```

or like this
```abap
lo_object->at('subcomp1->subcomp11->subcomp111')
```

## The syntax:
* Supported component separators are `-`, `->`, and `=>`. The recommended separator is `-`.
* For dynamic index access of rows in nested tables, use `[index]` after the component name. E.g. `table_name[2]`. The indexing starts from 1. If the accessor object references the table data object directly, you may skip the component name. E.g. `[2]`. You may continue accessing components after index access, e.g.: `table_a[1]-struct-table_b[2]`. If you do out-of-range access, you get an empty reference back. Index access works only with index tables (STANDARD, SORTED).
* For dynamic key access of rows in nested tables use `[key=value]` for single key lookup, `[key1=value1, key2=value2]` for multi-key lookup, and `[value]` for table line lookup (matches the full line). You can NOT search for values containing `,` — no escaping is supported. You can use nested lookups: `table_a[key1=value1]-table_b[key2=value2, key3=value3]`. Values used in a query are assigned to the corresponding field, so the value must be compatible with the field type.

# Version History

## Note [2798102](https://launchpad.support.sap.com/#/notes/2798102) - PL12
* Fixed. Access to fields with special characters in the name (e.g,. "/BIC/YEAR") fails.

## Note [2786259](https://launchpad.support.sap.com/#/notes/2786259) - PL11
* Fixed. Short dump when accessing elements of a null array

## Note [2526405](https://launchpad.support.sap.com/#/notes/2526405)
* New: /UI2/CL_DATA_ACCESS class for working with dynamic ABAP data object (generated with method /UI2/CL_JSON=>GENERATE). The class can be used as a replacement for multiple ASSIGN COMPONENT language constructions.
