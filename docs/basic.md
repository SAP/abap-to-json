# Basic Usage

# What the class can
## ABAP to JSON
* Serialize classes, structures, internal tables, class and data references, and any kind of elementary types. Complex types, such as a table of structures/classes, classes with complex attributes, etc. are also supported and recursively processed.
* **ABAP to JavaScript** adopted way of data type serializations:
  * strings, character types to JavaScript string format (no length limitation),
  * ABAP_BOOL / BOOLEAN / XFELD / BOOLE_D to JavaScript Boolean,
  * Built-in TRIBOOL (TRUE/FALSE/UNDEFINED = 'X'/'-'/'') support, for better control of initial values when serializing into JavaScript Boolean
  * int/floats/numeric/packed to JavaScript Integers/floats,
  * date/time to JavaScript date/time string representation as "2015-03-24" or "15:30:48",
  * timestamp to JavaScript integer or ISO8601 string
  * structures to JavaScript objects (include types are also supported; aliases => AS are ignored)
  * **convert ABAP internal table to JSON**, e.g JavaScript arrays or associative arrays (objects)
* Support of conversion exits on ABAP data serialization
* Pretty Printing of JavaScript property names: MY_DATA -> myData, /SAPAPO/MY_DATA -> sapapoMyData.
* Condensing of default values: initial values are not rendered into the resulting JSON string
* Optionally apply **JSON formatting/beautifing/pretty-print** for serialized JSON
* Performance is optimized for processing big internal tables with structures

## JSON to ABAP
* Deserialize JSON objects, arrays, and any elementary types into corresponding ABAP structures. Complex objects, with embedded arrays and objects with any level of nesting, are also supported.
* **Convert JSON to an internal table**
* Generic deserialization of JSON objects into reference data types: 
  * as simple data types (integer, boolean, or string into generic data reference (REF TO DATA) -> ABAP type is selected based on JSON type.
  * as dynamically generated complex object (structures, tables, mixed) for initial REF TO DATA fields
  * as typed references for prefilled REF TO DATA fields (you assign a reference to a typed empty data object to the REF TO DATA field in execution time)
* Deserialization of unknown JSON structures possible using method GENERATE into on-the-fly created data types
* On **JSON** to **ABAP** transformation following rules are used:
  * objects parsed into corresponding ABAP structures, classes (only classes with constructors with no obligatory parameters are supported), or internal hash/sorted tables
  * arrays converted to internal tables (complex tables are also supported). 
  * Boolean converted as ABAP_BOOL (‘’ or ‘X’)
  * Date/Time/Timestamps from JSON converted based on the type of corresponding ABAP element
  * integers/floats/strings moved to corresponding fields using ABAP move semantic (strings are un-escaped). There is no limit on the size of deserialized strings, the only restriction is the constraints of receiving data type. Escaped Unicode symbols (\u001F) in strings are decoded.
  * elementary data types are converted if do not match: JavaScript integer can come into ABAP string or JavaScript string into ABAP integer, etc.
  * Transformation takes into account property naming guidelines for **JSON** and **ABAP** so that camelCase names will be copied into the corresponding CAMEL_CASE field if the CAMELCASE field is not found in the ABAP structure. Do not forget to use the same PRETTY_MODE for deserialization, as you have used for serialization.
  * Default field values, specified in reference ABAP variable are preserved, and not overwritten if not found in the JSON object
  * Transformation of **JSON** structures into ABAP class instances is NOT supported.
* Support of conversion exits on deserialization

The parser for serializing/deserializing uses single-pass parsing and is optimized to provide the best possible performance in ABAP in release release-independent way. But for time-critical applications, which have kernel version 7.20 and higher, it is recommended to use built-in **JSON** to **ABAP** transformations (CALL TRANSFORMATION). If transformation for some reason does not work, please assist with the following notes: [1650141](http://service.sap.com/sap/support/notes/2368774) and [1648418](http://service.sap.com/sap/support/notes/2368774).

# Usage example
## ABAP to JSON usage example
```abap
CLASS demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS demo IMPLEMENTATION.
  METHOD main.

    DATA: lt_flight TYPE STANDARD TABLE OF sflight,
          lrf_descr TYPE REF TO cl_abap_typedescr,
          lv_json   TYPE /ui2/cl_json=>json.


    SELECT * FROM sflight INTO TABLE lt_flight.

    " serialize table lt_flight into JSON, skipping initial fields and converting ABAP field names into camelCase
    lv_json = /ui2/cl_json=>serialize( data          = lt_flight
                                       pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
                                       compress      = abap_true
                                      ).

    cl_demo_output=>write_json( lv_json ).

    CLEAR lt_flight.

    " deserialize JSON string json into internal table lt_flight doing camelCase to ABAP like field name mapping
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json pretty_name = /ui2/cl_json=>pretty_mode-camel_case 
                               CHANGING data  = lt_flight ).

    " serialize ABAP object into JSON string
    lrf_descr = cl_abap_typedescr=>describe_by_data( lt_flight ).
    lv_json = /ui2/cl_json=>serialize( data = lrf_descr format_output = abap_true ).

    cl_demo_output=>write_json( lv_json ).

    cl_demo_output=>display( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  demo=>main( ).
```

## JSON Output 1
```json
[
  {
    "mandt":"120",
    "carrid":"AA",
    "connid":17,
    "fldate":"2018-08-15",
    "price":422.94,
    "currency":"USD",
    "planetype":"747-400",
    "seatsmax":385,
    "seatsocc":268,
    "paymentsum":192361.84
  },
  {
    "mandt":"120",
    "carrid":"AA",
    "connid":17
  },
  ...
```
```json
{
  "ABSOLUTE_NAME":"\\TYPE=%_T00004S00000000O0000014656",
  "DECIMALS":0,
  "HAS_UNIQUE_KEY":false,
  "INITIAL_SIZE":0,
  "KEY":
  [
    {
      "NAME":"MANDT"
    },
    {
      "NAME":"CARRID"
    },
    {
      "NAME":"CONNID"
    }
....
```
# API description
Two static methods are of most interest in common cases: SERIALIZE and DESERIALIZE. The rest of the public methods are done public only for reuse purposes if you would like to build/extend your own serialization/deserialization code. 

## SERIALIZE: Serialize ABAP object into JSON

* \> **DATA** (any) - any ABAP object/structure/table/element to be serialized
* \> **COMPRESS** (bool, default=false) - tells serializer to skip empty elements/objects during serialization. So, all of which IS INITIAL = TRUE. 
* \> **NAME** (string, optional) - optional name of the serialized object. Will '"name" : {...}' instead of ' {...} ' if supplied.
* \> **PRETTY_NAME** (enum, optional)- mode, controlling how ABAP field names are transformed in JSON attribute names. See the description below.  
* \> **TYPE_DESCR** (ref to CL_ABAP_TYPEDESCR, optional) - if you know the object type already - pass it to improve performance. 
* \> **ASSOC_ARRAYS** (bool, default = false) - controls how to serialize hash or sorted tables with unique keys. See below for details.
* \> **ASSOC_ARRAYS_OPT** (bool, default = false) - when set, the serializer will optimize the rendering of name-value associated arrays (hash maps) in JSON
* \> **TS_AS_ISO8601** (bool, default = false) - says serializer to output timestamps using ISO8601 format.
* \> **NUMC_AS_STRING** (bool, default = false) - Controls the way NUMC fields are serialized. If set to ABAP_TRUE, NUMC fields are serialized not as integers, but as strings, with all leading zeros. Deserialization works compatible with both ways of NUMC serialized data.
* \> **NAME_MAPPINGS** (table) - ABAP<->JSON Name Mapping Table
* \> **CONVERSION_EXITS** (bool, default = false) - use DDIC conversion exits on serialize of values (performance loss!)
* \> **FORMAT_OUTPUT** (bool, default = false) - Indent, add formatting spaces, and split into lines serialized JSON
* \> **HEX_AS_BASE64** (bool, default = true) - Serialize hex values as base64
* \< **R_JSON** - output JSON string.

## DESERIALIZE: Deserialize ABAP object from JSON string

* \> **JSON** (string) - input JSON object string to deserialize
* \> **JSONX** (xstring) - input JSON object as a raw string to deserialize
* \> **PRETTY_NAME** (enum, optional) - mode, controlling how JSON field names are mapped to ABAP component names. See the description below.  
* \> **ASSOC_ARRAYS** (bool, default = false) -  controls how to deserialize JSON objects into hash or sorted tables with unique keys. See below for details.
* \> **ASSOC_ARRAYS_OPT** (bool, default = false) - when set, the deserializer will take into account the optimized rendering of associated arrays (properties) in JSON. 
* \> **TS_AS_ISO8601** (bool, default = false) - says deserializer to read timestamps from strings into timestamps fields using ISO 8601 format.
* \> **NAME_MAPPINGS** (table) - ABAP<->JSON Name Mapping Table
* \> **CONVERSION_EXITS** (bool, default = false) - use DDIC conversion exits on deserialize of values (performance loss!)
* \> **HEX_AS_BASE64** (bool, default = true) - Deserialize hex values as base64
* \<\> **DATA** (any) - ABAP object/structure/table/element to be filled from JSON string. If the ABAP structure contains more fields than in the JSON object, the content of unmatched fields is preserved.

## GENERATE: Generates ABAP object from JSON

* \> **JSON** (string) - input JSON object string to deserialize
* \> **PRETTY_NAME** (enum, optional) - mode, controlling how JSON field names are mapped to ABAP component names. See the description below.  
* \> **NAME_MAPPINGS** (table) - ABAP<->JSON Name Mapping Table
* \< **RR_DATA** (REF TO DATA) - a reference to ABAP structure/table dynamically generated from JSON string.
In addition to the explained methods, there are two options, that need a wider explanation:

## PRETTY_NAME : enumeration of modes, defined as constant /UI2/CL_JSON=>pretty_name.

* **NONE** - ABAP component names are serialized as is (UPPERCASE).
* **LOW_CASE** - ABAP component names serialized in low case 
* **CAMEL_CASE** - ABAP component types serialized in CamelCase where symbol "\_" is treated as a word separator (and removed from the resulting name). 
* **EXTENDED** - works the same way as CAMEL_CASE but also, has extended logic for encoding special characters, such as: ".", "@", "~", etc. Shall be used if you need JSON names with characters not allowed for ABAP data component names. Do not use it, if you do not have special characters in JSON names - the performance would be slower in comparison with CAMEL_CASE mode. Example: ABAP name '\_\_A\_\_SCHEMA' translates in JSON name '@schema'
Encoding rules (ABAP name → JSON name):
  * '\_\_E\_\_' → '!'
  * '\_\_N\_\_' → '#'
  * '\_\_D\_\_' → '$'
  * '\_\_P\_\_' → '%'
  * '\_\_M\_\_' → '&'
  * '\_\_S\_\_' → '*'
  * '\_\_H\_\_' → '-'
  * '\_\_T\_\_' → '~'
  * '\_\_L\_\_' → '/'
  * '\_\_C\_\_' → ':'
  * '\_\_V\_\_' → '|'
  * '\_\_A\_\_' → '@'
  * '\_\_O\_\_' or '\_\_\_' → '.'
**NONE** and **LOW_CASE** work the same way for DESERIALIZE.

## ASSOC_ARRAYS :

This option controls the way hashed or sorted tables with unique keys are serialized/deserialized. Normally, ABAP internal tables are serialized into JSON arrays, but in some cases, you will like to serialize them as associative arrays (JSON objects) where every row of the table shall be reflected as a separated property of the JSON object. This can be achieved by setting the *ASSOC_ARRAYS* parameter to TRUE. If set, the serializer checks for sorted/hashed tables with a UNIQUE key(s) and serialize them as an object. The JSON property name, reflecting row, constructed from values of fields, used in key separated by constant *MC_KEY_SEPARATOR* = '-'. If the table has only one field marked as key, the value of this single field becomes a property name and is REMOVED from the associated object (to eliminate redundancy). If TABLE_LINE is used as a unique key, all values of all fields construct key property names (separated by *MC_KEY_SEPARATOR*). During deserialization, logic works vice versa: if *ASSOC_ARRAYS* is set to TRUE, and the JSON object matches the internal hash or sorted table with the unique key, the object is transformed into the table, where every object property is reflected in a separated table row. If the ABAP table has only one key field, the property name is transformed into a value of this key field.

## ASSOC_ARRAYS_OPT:

By default, when dumping hash/sorted tables with a unique key into JSON, the serializer will write the key field as the property name, and the rest of the fields will write the object value of properties:
```abap
TYPES: BEGIN OF ts_record,
        key TYPE string,
        value TYPE string,
       END OF ts_record.
 
DATA: lt_act TYPE SORTED TABLE OF ts_record WITH UNIQUE KEY key.
lv_json = /ui2/cl_json=>serialize( data = lt_exp assoc_arrays = abap_true ).
```
Output JSON
```json
{
    "KEY1": {
        "value": "VALUE1"
    },
    "KEY2": {
        "value": "VALUE2"
    }
}
```
But if you will use the assoc_arrays_opt flag during serialization, the serializer will try to omit unnecessary object nesting on dumping of simple, name/value tables, containing only one key field and one value field:
```abap
lv_json = /ui2/cl_json=>serialize( data = lt_exp assoc_arrays = abap_true assoc_arrays_opt = abap_true ).
```
Output JSON
```json
{
    "KEY1": "VALUE1",
    "KEY2": "VALUE2"
}
```
For deserialization, the flag is used to tell the deserializer that the value shall be placed in a non-key field of the structure.

# Supported SAP_BASIS releases
The code was tested from SAP_BASIS 7.00 and higher, but I do not see the reasons why it cannot be downported on lower releases either. But if you plan to use it on SAP_BASIS 7.02 and higher (and do not need property name pretty-printing) better consider the standard solution for **ABAP**, using CALL TRANSFORMATION. It shall be faster, while implemented in the kernel. See the [blog of Horst Keller](http://scn.sap.com/community/abap/blog/2013/07/04/abap-news-for-release-740--abap-and-json) for details. Maybe the best will be, if you need support in lower SAP_BASIS releases as well as in 7.02 and higher, to modify provided a class in a way to generate the same **JSON** format as standard ABAP CALL TRANSFORMATION for JSON does and redirect flow to home-made code or built-in **ABAP** transformation depending on SAP_BASIS release.

# Further optimizations
* Be aware, that usage of flag conversion_exits may significantly decrease performance - use only in cases, when you are sure that you need it.
* Escaping property values can be expensive. To optimize performance, in this case, you can replace escapement code with some kernel-implemented function (from cl_http_utility class for example), instead of explicit *REPLACE ALL OCCURRENCES* calls.
* Unescaping can influence deserialization performance even worse, depending on the fact that your JSON has encoded \n\r\t\f\b\x. So, avoid the usage of them if you can. 
* It is possible to significantly increase performance for serialization/deserialization by dropping the support of releases below 7.40. That can be realized by moving base parsing from ABAP to kernel-implemented classes cl_sxml_string_writer and cl_sxml_string_reader.

# Remarks
Due to optimization reasons, some methods were converted to macros, to reduce overhead for calling methods for data type serialization. If performance in your case is not critical, and you prefer clean/debuggable code you can replace macro calls with corresponding methods. 

# Continue reading
* [Advanced Use cases](advanced.md)
* [Extension (inheriting) of the class](class-extension.md)
* [FAQ](faq.md)
* [Version History](history.md)

# Related pages
* [Does /UI2/CL_JSON work in ABAP Cloud?](https://answers.sap.com/questions/12699592/does-ui2cl-json-work-in-abap-cloud.html)
* [Return generic result via SAP RFC (performance)](https://stackoverflow.com/questions/54037544/abap-return-generic-result-via-sap-rfc-json)
* [RFC7159 - The JavaScript Object Notation (JSON) Data Interchange Format](https://tools.ietf.org/html/rfc7159)
* [OData JSON Deserialize](https://answers.sap.com/questions/12998367/odata-simple-json-deserialize.html?childToView=13007205)
