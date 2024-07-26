# Overview
- [Custom ABAP to JSON, JSON to ABAP name mapping](#custom-abap-to-json-json-to-abap-name-mapping)
- [Custom formatting of values for serialization of ABAP into JSON](#custom-formatting-of-values-for-serialization-of-abap-into-json)
- [Serialization/deserialization of hierarchical/recursive data](#serializationdeserialization-of-hierarchicalrecursive-data)
- [Partial serialization/deserialization](#partial-serializationdeserialization)
- [Deserialization of an untyped (unknown) JSON object](#deserialization-of-an-untyped-unknown-json-object)
- [Implicit generation of ABAP objects on deserialization](#implicit-generation-of-abap-objects-on-deserialization)
- [JSON/ABAP serialization/deserialization with runtime type information](#jsonabap-serializationdeserialization-with-runtime-type-information)
- [Exception Handling in /UI2/CL_JSON](#exception-handling-in-ui2cl_json)
- [JSON to ABAP transformation with the use of CALL TRANSFORMATION](#json-to-abap-transformation-with-the-use-of-call-transformation)

# Custom ABAP to JSON, JSON to ABAP name mapping
By default, you control how JSON names are formatted/mapped to ABAP names by selecting proper pretty_mode as a parameter for the SERIALIZE/DESERIALIZE/GENERATE method. But sometimes, the standard, hard-coded formatting, is not enough. For example, if you need special rules for name formatting (for using special characters) or because the JSON attribute name is too long and can't be mapped to the ABAP name (which has a 30-character length limit). 

The recommended way for custom mapping was an extension of the class and redefining methods PRETTY_NAME or PRETTY_NAME_EX, but since note [2526405](https://launchpad.support.sap.com/#/notes/2526405) there is an easier way, without the need for its class. If you have a static list of field mappings from ABAP to JSON you can pass the name mapping table as a parameter for the constructor/serialize/deserialize and control how JSON names are formatted/mapped to ABAP names. 

## ABAP to JSON name mapping example
```abap
TYPES:
  BEGIN OF tp_s_data,
    sschema             TYPE string,
    odatacontext        TYPE string,
    shortened_abap_name TYPE string,
    standard            TYPE string,
  END OF tp_s_data.

DATA: ls_exp      TYPE tp_s_data,
      lt_mapping  TYPE /ui2/cl_json=>name_mappings,
      lv_json     TYPE /ui2/cl_json=>json.

lt_mapping = VALUE #( ( abap = `SSCHEMA` json = `$schema` )
                      ( abap = `ODATACONTEXT` json = `@odata.context` )
                      ( abap = `SHORTENED_ABAP_NAME` json = `VeeeeryyyyyLooooongJSONAttrbuuuuuuuuuteeeeeeeeeee` ) ).

lv_json = /ui2/cl_json=>serialize( data = ls_exp name_mappings = lt_mapping ).
```

# Custom formatting of values for serialization of ABAP into JSON
Sometimes you need custom formatting for your ABAP data when serializing it into JSON. In another use case, you have some custom, DDIC-defined data types, that are not automatically recognized by standard code, so no appropriate formatting is applied (for example custom boolean or timestamp type). 

In such cases, you have the following options:
1. Extend the class and overwrite the method DUMP_TYPE. You can check an example in the section [class extension](class-extension.md).
2. Add conversion exits for your custom type and apply formatting as part of the conversion exit.
3. Create an alternative structure, with your custom types replaced by supported types, only for serialization, and do the move of data before the serialization.

# Serialization/deserialization of hierarchical/recursive data

Handling the recursive data structure in ABAP is not very trivial. And it is not very trivial to serialize and deserialize it either.
If you would like to model your hierarchical data (tree-like) as ABAP structures, the only allowed way will be to do it like in the example below, where you use references to generic data:
```abap
TYPES: 
  BEGIN OF ts_node,
    id        TYPE i,
    children  TYPE STANDARD TABLE OF REF TO data WITH DEFAULT KEY,
  END OF ts_node.

DATA: lv_exp    TYPE string,
      lv_act    TYPE string,
      ls_data   TYPE ts_node,
      lr_data   LIKE REF TO ls_data.

ls_data-id = 1.

CREATE DATA lr_data.
lr_data->id = 2.
APPEND lr_data TO ls_data-children.
```
Such a way is more or less straightforward and will work, but it leads to losing type information for data stored in the children's table. That means you will need to cast data when you access it. In addition, it blocks you from deserializing such data from JSON, while the parser cannot deduce the type of data that needs to be created in the children's table. But serialization will work fine:
```abap
lv_exp = '{"ID":1,"CHILDREN":[{"ID":2,"CHILDREN":[]}]}'.
lv_act = /ui2/cl_json=>serialize( data = ls_data ).
cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of recursive data structure fails' ).
```
The better way to model hierarchical data in ABAP is with the help of objects. In contrast, objects are always processed as references and ABAP allows you to create nested data structures, referring to objects of the same type:
```abap
CLASS lcl_test DEFINITION FINAL.
  PUBLIC SECTION.
    DATA: id TYPE i.
    DATA: children TYPE STANDARD TABLE OF REF TO lcl_test.
ENDCLASS.                    "lcl_test DEFINITION
```
In that manner, you can process data in the same way as with ABAP structures but using typed access and serialization/deserialization of data in JSON works fine while types can be deduced on 
```abap
DATA: lo_act    TYPE REF TO lcl_test,
      lo_exp    TYPE REF TO lcl_test,
      lv_json   TYPE string,
      lo_child  TYPE REF TO lcl_test.

CREATE OBJECT lo_exp.

lo_exp ->id = 1.

CREATE OBJECT lo_child.
lo_child->id = 2.
APPEND lo_child TO lo_exp->children.

lv_json = /ui2/cl_json=>serialize( data = lo_exp ).
ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data =  lo_act ).
```
Remark: There are some constraints for data design that exist regarding the deserialization of objects:
* You cannot use constructors with obligatory parameters
* References to interfaces will not be deserialized

## Serializing of protected and private attributes
If you do the serialization from outside of your class, you can access only the public attributes of that class. To serialize all types of attributes (private+protected) you need to allow the JSON serializer class to access them. This can be done by defining the serializer class as a friend of your class. In this way, you do not disrupt your encapsulation for other classes but enable the serializer class to access all data of your class.

If you do not own a class you want to serialize, you can inherit it from your class and add friends there. In this case, you can access at least protected attributes.

# Partial serialization/deserialization
When it is needed:
* You deserialize JSON to ABAP but would like some known parts to be deserialized as JSON string, while you do not know the nesting JSON structure.
* You deserialize a collection (array/associative array) having objects with heterogeneous structures (for example the same field has a different type depending on object type). Using partial deserialization, you can restore a type such as a JSON string in ABAP, and apply additional deserialization based on the object type later.  
* You serialize ABAP to JSON and have some ready JSON pieces (strings) you want to mix in. 

The solution /UI2/CL_JSON has for this type /UI2/CL_JSON=>JSON (alias for built-in type string). ABAP fields declared with this type will be serialized/deserialized as JSON pieces. During serialization from ABAP to JSON, the content of such JSON piece is not validated for correctness, so if you pass an invalid JSON block, it may destroy the complete resulting JSON string at the end.

I've included examples of partial serialization/deserialization below.

Serialization:
```abap
TYPES: BEGIN OF ts_record,
        id      TYPE string,
        columns TYPE /ui2/cl_json=>json,
       END OF ts_record.

DATA: lv_json   TYPE /ui2/cl_json=>json,
      lt_data   TYPE SORTED TABLE OF ts_record WITH UNIQUE KEY id,
      ls_data   LIKE LINE OF lt_data.

ls_data-id = 'O000001ZZ_SO_GRES_CONTACTS'.
ls_data-columns = '{"AGE":{"bVisible":true,"iPosition":2},"BRSCH":{"bVisible":true}}'.
INSERT ls_data INTO TABLE lt_data.

ls_data-id = 'O000001ZZ_TRANSIENT_TEST_A'.
ls_data-columns = '{"ABTNR":{"bVisible":false},"CITY1":{"bVisible":false},"IC_COMPANY_KEY":{"bVisible":true}}'.
INSERT ls_data INTO TABLE lt_data.

lv_json = /ui2/cl_json=>serialize( data = lt_data assoc_arrays = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

WRITE / lv_json.
```
Results in:
```json
{
    "O000001ZZ_SO_GRES_CONTACTS": {
        "columns": {
            "AGE": {
                "bVisible": true,
                "iPosition": 2
            },
            "BRSCH": {
                "bVisible": true
            }
        }
    },
    "O000001ZZ_TRANSIENT_TEST_A": {
        "columns": {
            "ABTNR": {
                "bVisible": false
            },
            "CITY1": {
                "bVisible": false
            },
            "IC_COMPANY_KEY": {
                "bVisible": true
            }
        }
    }
}
```
Deserialization:
```abap
TYPES: BEGIN OF ts_record,
        id      TYPE string,
        columns TYPE /ui2/cl_json=>json,
       END OF ts_record.

DATA: lv_json  TYPE string,
      lt_act   TYPE SORTED TABLE OF ts_record WITH UNIQUE KEY id.

CONCATENATE 
'{"O000001ZZ_SO_GRES_CONTACTS":{"columns":{"AGE":{"bVisible":true,"iPosition":2},"BRSCH":{"bVisible":true}}},'
'"O000001ZZ_TRANSIENT_TEST_A":{"columns":{"ABTNR":{"bVisible":false},"CITY1":{"bVisible":false},"IC_COMPANY_KEY":{"bVisible":true}}}}'
INTO lv_json.

" If you know the first level of the underlying structure ("columns" field) -> Output Var 1
/ui2/cl_json=>deserialize( EXPORTING json = lv_json assoc_arrays = abap_true CHANGING data = lt_act ).
 
" if you do not know the underlying structure of the first level (naming of the second field e.g columns in the example does not matter )
" => result is a little bit different -> Output Var 2
/ui2/cl_json=>deserialize( EXPORTING json = lv_json assoc_arrays = abap_true assoc_arrays_opt = abap_true CHANGING data = lt_act ).
```
Results in the following ABAP data:
## ABAP Output (variant 1)
```
ID(CString)	                COLUMNS(CString)
O000001ZZ_SO_GRES_CONTACTS  {"AGE":{"bVisible":true,"iPosition":2},"BRSCH":{"bVisible":true}}
O000001ZZ_TRANSIENT_TEST_A  {"ABTNR":{"bVisible":false},"CITY1":{"bVisible":false},"IC_COMPANY_KEY":{"bVisible":true}}
```
## ABAP Output (variant 2)
```
ID(CString)	                COLUMNS(CString)
O000001ZZ_SO_GRES_CONTACTS  {"columns":{"AGE":{"bVisible":true,"iPosition":2},"BRSCH":{"bVisible":true}}}
O000001ZZ_TRANSIENT_TEST_A  {"columns":{"ABTNR":{"bVisible":false},"CITY1":{"bVisible":false},"IC_COMPANY_KEY":{"bVisible":true}}}
```

# Deserialization of an untyped (unknown) JSON object
Suppose you need to deserialize a JSON object with an unknown structure, or you do not have a passing data type on the ABAP side, or the data type of the resulting object may vary. In that case, you can generate an ABAP object on the fly, using the corresponding GENERATE method. The method has some limitations compared to standard deserialization:
* Fields are generated as a reference (even elementary types). If you want a more user-friendly generation - use the optimization flag (see below).
* you can not control how deserialized arrays or timestamps
* you can not access components of generated structure statically (while the structure is unknown at compile time) and need to use dynamic access
* you need to accept default logic for type detection. Supported types are int, float, packaged, strings, boolean, date, time, and timestamps.

The simplest example, with straightforward access:
```abap
DATA: lv_json TYPE /ui2/cl_json=>json,
      lr_data TYPE REF TO data.

FIELD-SYMBOLS:
  <data>   TYPE data,
  <struct> TYPE any,
  <field>  TYPE any.

lv_json = `{"name":"Key1","properties":{"field1":"Value1","field2":"Value2"}}`.
lr_data = /ui2/cl_json=>generate( json = lv_json ).

" OK, generated, now let us access some field :(
IF lr_data IS BOUND.
  ASSIGN lr_data->* TO <data>.
  ASSIGN COMPONENT `PROPERTIES` OF STRUCTURE <data> TO <field>.
  IF <field> IS ASSIGNED.
    lr_data = <field>.
    ASSIGN lr_data->* TO <data>.
    ASSIGN COMPONENT `FIELD1` OF STRUCTURE <data> TO <field>.
    IF <field> IS ASSIGNED.
      lr_data = <field>.
      ASSIGN lr_data->* TO <data>.
      WRITE: <data>. " We got it -> Value1
    ENDIF.
  ENDIF.
ENDIF.
```
A nice alternative, using [dynamic data accessor helper class](data-access.md): 
```abap
DATA: lv_json TYPE /ui2/cl_json=>json,
      lr_data TYPE REF TO data,
      lv_val  TYPE string.

lv_json = `{"name":"Key1","properties":{"field1":"Value1","field2":"Value2"}}`.
lr_data = /ui2/cl_json=>generate( json = lv_json ).

/ui2/cl_data_access=>create( ir_data = lr_data iv_component = `properties-field1`)->value( IMPORTING ev_data = lv_val ).
WRITE: lv_val.
```

## Optimized type generation
From PL19 it is possible to use the new switch for GENERATE (optimize) and DESERIALIZE (gen_optimize) to activate the optimization of generated ABAP data for REF TO DATA (fewer references, easily readable and accessible). But it results in longer processing for data generation (~25%).
The generation without optimization flag:
With optimization flag:


## Implicit generation of ABAP objects on deserialization

In addition to the explicit generation of the ABAP data objects from the JSON string, the deserializer supports an implicit way of generation, during DESERIALIZE(INT) call. To trigger generation, your output data structure shall contain a field with the type REF TO DATA, and the field name shall match the JSON attribute (pretty name rules are considered). Depending on the value of the field, the behavior may differ:
* The value is not bound (initial): deserialize will use generation rules when creating corresponding data types of the referenced value
* The value is bound (but may be empty): the deserializer will create a new referenced value based on the referenced type.

### Example of implicit generation of ABAP data from JSON string
```abap
TYPES:
  BEGIN OF ts_dyn_data1,
    name     TYPE string,
    value    TYPE string,
  END OF ts_dyn_data1,
  BEGIN OF ts_dyn_data2,
    key      TYPE string,
    value    TYPE string,
  END OF ts_dyn_data2,
  BEGIN OF ts_data,
    str     TYPE string,
    data    TYPE REF TO data,
  END OF ts_data.

DATA:
  ls_data  TYPE ts_data,
  lv_json  TYPE /ui2/cl_json=>json.

lv_json = `{"str":"Test","data":{"name":"name1","value":"value1"}}`.

" deserialize data and use generic generation for field "data",
" the same as with method GENERATE (using temporary data type)
/ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_data ).

" deserialize data and use type TS_DYN_DATA1 for the field "data"
CREATE DATA ls_data-data TYPE ts_dyn_data1.
/ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_data ).

" deserialize data and use alternative type TS_DYN_DATA2 for the field "data"
CREATE DATA ls_data-data TYPE ts_dyn_data2.
/ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_data ).
```

# JSON/ABAP serialization/deserialization with runtime type information
Automatic deserialization of the JSON into the appropriate ABAP structure is not supported. The default implementation assumes that you need to know the target data structure (or at least partial structure, it will also work) to deserialize JSON in ABAP and then work with typed data. 

But if for some reason one needs the ability to deserialize JSON in source ABAP structure in a generic way, he can extend both serialize/deserialize methods and wrap outputs/inputs of /UI2/CL_JSON data by technical metadata describing source ABAP structure and use this information during deserialization (or use GENERATE method). Of course, you must ensure that the source ABAP data type is known in the deserialization scope (global and local types are "visible").

See the example below:
```abap
TYPES: BEGIN OF ts_json_meta,
         abap_type LIKE cl_abap_typedescr=>absolute_name,
         data      TYPE string,
       END OF ts_json_meta.

DATA: lt_flight TYPE STANDARD TABLE OF sflight,
      lv_json   TYPE string,
      lo_data   TYPE REF TO data,
      ls_json   TYPE ts_json_meta.

FIELD-SYMBOLS: <data> TYPE any.

SELECT * FROM sflight INTO TABLE lt_flight.

* serialize table lt_flight into JSON, skipping initial fields and converting ABAP field names into camelCase
ls_json-data      = /ui2/cl_json=>serialize( data = lt_flight compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
ls_json-abap_type = cl_abap_typedescr=>describe_by_data( lt_flight )->absolute_name.
lv_json           = /ui2/cl_json=>serialize( data = ls_json compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
WRITE / lv_json.

CLEAR: ls_json, lt_flight.

* deserialize JSON string json into internal table lt_flight doing camelCase to ABAP like field name mapping
/ui2/cl_json=>deserialize( EXPORTING json = lv_json pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = ls_json ).
CREATE DATA lo_data TYPE (ls_json-abap_type).
ASSIGN lo_data->* TO <data>.
/ui2/cl_json=>deserialize( EXPORTING json = ls_json-data pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = <data> ).

IF lo_data IS NOT INITIAL.  
  BREAK-POINT. " check here lo_data
ENDIF.
```

# Exception Handling in /UI2/CL_JSON
By default, /UI2/CL_JSON tries to hide from consumer-thrown exceptions (that may happen during deserialization) catching them at all levels. In some cases, it will result in missing attributes, in other cases, when an error is critical and the parser can not restore, you will get an empty object back. The main TRY/CATCH block prevents exceptions from the DESERIALIZE method.

If you want to get a report in case of error, use the instance method DESERIALIZE_INT which may fire CX_SY_MOVE_CAST_ERROR. The reporting is rather limited - all errors are translated into CX_SY_MOVE_CAST_ERROR and no additional information is available. But from PL19 you will also get extra details reported in the target (ABAP) and source (JSON) fields, as the type of ABAP field, that is not filled and the JSON node leading to the error. More details can be found in [this issue](https://github.com/SAP/abap-to-json/pull/8).

# JSON to ABAP transformation with the use of CALL TRANSFORMATION
Below is a small example of CALL TRANSFORMATION usage to produce JSON from ABAP structures. Don't ask me for details - I do not know them. (smile) It was just a small test of me.
```abap
DATA: lt_flight TYPE STANDARD TABLE OF sflight.
SELECT * FROM sflight INTO TABLE lt_flight.

* ABAP to JSON
DATA(lo_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
CALL TRANSFORMATION id SOURCE text = lt_flight RESULT XML lo_writer.
DATA(lv_jsonx) = lo_writer->get_output( ).
DATA(lv_json) = /ui2/cl_json=>raw_to_string( lv_jsonx ).

 * JSON to ABAP
 CALL TRANSFORMATION id SOURCE XML lv_jsonx RESULT text = lt_flight.
```
The transformation above uses the built-in ID transformation, that produces "ABAP JSON" output. If you want your custom transformation rules you can use [this project](https://github.com/timostark/abap-json-serialization) to create an XSLT transformation for your data structure.
If transformation for some reason does not work, please assist with the following notes: [1650141](http://service.sap.com/sap/support/notes/2368774) and [1648418](http://service.sap.com/sap/support/notes/2368774).
See also the [blog of Horst Keller](http://scn.sap.com/community/abap/blog/2013/07/04/abap-news-for-release-740--abap-and-json) for more details. 

# Continue reading
* [Basic usage of the class](basic.md)
* [Extension (inheriting) of the class](class-extension.md)
* [FAQ](faq.md)
* [Version History](history.md)
