# /UI2/CL_JSON extension

If standard class functionality does not fit your requirements there are two ways how you can adapt it to your needs:

* Use a local copy of the class /UI2/CL_JSON and modify logic directly, by the change of original code.
* Inherit from class /UI2/CL_JSON and override methods where another logic is required. 

The advantage of the first approach is that you are completely free in what you may change and have full control of the class lifecycle. The disadvantage is that you will need to merge your changes with /UI2/CL_JSON updates. 

For the second approach you can use /UI2/CL_JSON directly (the prerequisite is the latest version of note 2330592), you do not need to care about the merge but can override only some methods. The methods are:

## IS_COMPRESSIBLE 
IS_COMPRESSIBLE is called to check, if the given type output may be suppressed during ABAP to JSON serialization when a value is initial. 

* \> **TYPE_DESCR** (ref to CL_ABAP_TYPEDESCR) – value type
* \< **RV_COMPRESS** (bool) – compress initial value
The default implementation of the method allows compressing any initial value.

## PRETTY_NAME
PRETTY_NAME – called to format ABAP field name written to **JSON** or deserialized from **JSON** to **ABAP** field, when the pretty_name parameter of SERIALIZE/DESERIALIZE method equal to PRETTY_MODE-CAMEL_CASE.

* \> **IN** (CSEQUENCE) – Field name to pretty print.
* \< **OUT** (STRING) – Pretty printed field name
The default implementation applies camelCase formatting, based on the usage of the “_” symbol. To output, the “_” symbol, use the double “__” symbol in the field name.

## PRETTY_NAME_EX
PRETTY_NAME_EX – called to format ABAP field name written to JSON or deserialized from JSON to ABAP field, when the pretty_name parameter of *SERIALIZE/DESERIALIZE*  method equal to *PRETTY_MODE-EXTENDED*.

* \> **IN** (CSEQUENCE) – Field name to pretty print.
* \< **OUT** (STRING) – Pretty printed field name
The default implementation does the same as PRETTY_NAME, plus converting special characters "!#$%&\*-~/:|@.". 

## DUMP_INT
DUMP_INT - called for recursive serialization of complex ABAP data objects (structure, class, table) into JSON string

* \> **DATA** (DATA) – Any data to serialize.
* \> **TYPE_DESCR** (ref to CL_ABAP_TYPEDESCR, optional) – Type of data provided
* \< **R_JSON** (JSON) – serialized JSON value

## DUMP_TYPE
DUMP_TYPE - called for serialization of elementary ABAP data type (string, boolean, timestamp, etc) into the JSON attribute value. Overwrite it if you, for example, want to apply data output data conversion of currency rounding
* \> **DATA** (DATA) – Any data to serialize
* \> **TYPE_DESCR** (ref to CL_ABAP_TYPEDESCR) – Type of data provided
* \< **R_JSON** (JSON) – serialized JSON value

## RESTORE
RESTORE - called for deserializing JSON objects into ABAP structures

* \> **JSON** (JSON) – JSON string to deserialize
* \>  **LENGTH** (I) – Length of the JSON string
* \> **TYPE_DESCR** (ref to CL_ABAP_TYPEDESCR, optional) – Type of changing data provided
* \> **FIELD_CACHE** (type T_T_FIELD_CACHE, optional) – Cache of ABAP data fields with type information
* \<\> **DATA** (type DATA, optional) – ABAP data object to fill
* \<\> **OFFSET** (I) – parsing start point in JSON string

## RESTORE_TYPE
RESTORE_TYPE - called to deserialize simple JSON attributes and JSON arrays

* \> **JSON** (JSON) – JSON string to deserialize
* \>  **LENGTH** (I) – Length of the JSON string
* \> **TYPE_DESCR** (ref to CL_ABAP_TYPEDESCR, optional) – Type of changing data provided
* \> **FIELD_CACHE** (type T_T_FIELD_CACHE, optional) – Cache of ABAP data fields with type information
* \<\> **DATA** (type DATA, optional) – ABAP data object to fill
* \<\> **OFFSET** (I) – parsing start point in JSON string

## CLASS_CONSTRUCTOR
CLASS_CONSTRUCTOR - used to initialize static variables. You can not overwrite it, but you can implement your class constructor that adapts default globals. For example, add the additional boolean types to be recognized during serialization/deserialization. 

## SERIALIZE/DESERIALIZE
SERIALIZE/DESERIALIZE - these methods are static and therefore cannot be redefined. Methods are helpers for a consumption code, hiding the construction of the class instance and further \*\_INT calls. So, if you would like to use something similar, in your custom class, you need to copy the mentioned methods to new ones e,g \*\_EX and overwrite there /UI2/CL_JSON type to your custom class name. And use these methods instead of standard.

Extension using inheritance:
```abap
CLASS lc_json_custom DEFINITION FINAL INHERITING FROM /ui2/cl_json.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      deserialize_ex IMPORTING json TYPE json OPTIONAL
                        pretty_name TYPE pretty_name_mode DEFAULT pretty_mode-none
                      CHANGING data TYPE data,
      serialize_ex IMPORTING data TYPE data
                         compress TYPE bool DEFAULT c_bool-false
                      pretty_name TYPE pretty_name_mode DEFAULT pretty_mode-none
          RETURNING value(r_json) TYPE json.

  PROTECTED SECTION.
    METHODS:
      is_compressable REDEFINITION,
      pretty_name REDEFINITION,
      dump_type REDEFINITION.
ENDCLASS.                    "lc_json_custom DEFINITION

CLASS lc_json_custom IMPLEMENTATION.

  METHOD class_constructor.
    CONCATENATE mc_bool_types `\TYPE=/UI2/BOOLEAN` INTO mc_bool_types.
  ENDMETHOD.                    "class_constructor

  METHOD is_compressable.
    IF type_descr->absolute_name EQ `\TYPE=STRING` OR name EQ `INITIAL`.
      rv_compress = abap_false.
    ELSE.
      rv_compress = abap_true.
    ENDIF.
  ENDMETHOD.                    "is_compressable

  METHOD pretty_name.
    out = super->pretty_name( in ).
    CONCATENATE out 'Xxx' INTO out.
  ENDMETHOD.                    "pretty_name

  METHOD dump_type.

    DATA: is_ddic    TYPE abap_bool,
          ddic_field TYPE dfies.

    is_ddic = type_descr->is_ddic_type( ).
    IF is_ddic EQ abap_true.
      ddic_field = type_descr->get_ddic_field( ).
      IF mv_ts_as_iso8601 EQ c_bool-true AND ddic_field-domname EQ `TZNTSTMPL`.
        r_json = data.
        CONCATENATE `"` r_json(4) `-` r_json+4(2) `-` r_json+6(2) `T` r_json+8(2) `:` r_json+10(2) `:` r_json+12(2) `.` r_json+15(7) `Z"`  INTO r_json.
        RETURN.
      ENDIF.
    ENDIF.
    IF mv_ts_as_iso8601 EQ c_bool-true AND type_descr->absolute_name EQ `\TYPE=LCM_CHANGED_ON`.
      r_json = data.
      CONCATENATE `"` r_json(4) `-` r_json+4(2) `-` r_json+6(2) `T` r_json+8(2) `:` r_json+10(2) `:` r_json+12(2) `.` r_json+15(7) `Z"`  INTO r_json.
      RETURN.
    ENDIF.

    r_json = super->dump_type( data = data type_descr = type_descr convexit = convexit ).

  ENDMETHOD. "dump_type

  METHOD serialize_ex.
    DATA: lo_json  TYPE REF TO lc_json_custom.
    CREATE OBJECT lo_json
      EXPORTING
        compress         = compress
        pretty_name      = pretty_name
        assoc_arrays     = abap_true
        assoc_arrays_opt = abap_true
        expand_includes  = abap_true
        numc_as_string   = abap_true
        ts_as_iso8601    = abap_true.
    r_json = lo_json->serialize_int( data = data ).
  ENDMETHOD.                    "serialize_ex

  METHOD deserialize_ex.
    DATA: lo_json TYPE REF TO lc_json_custom.
    IF json IS NOT INITIAL.
      CREATE OBJECT lo_json
        EXPORTING
          pretty_name      = pretty_name
          assoc_arrays     = abap_true
          assoc_arrays_opt = abap_true.
      TRY .
          lo_json->deserialize_int( EXPORTING json = json CHANGING data = data ).
        CATCH cx_sy_move_cast_error.
      ENDTRY.
    ENDIF.
  ENDMETHOD.                    "deserialize_ex 

ENDCLASS.                    "lc_json_custom IMPLEMENTATION

TYPES:
 BEGIN OF tp_s_data,
   tribool   TYPE lc_json_custom=>tribool,
   bool      TYPE lc_json_custom=>bool,
   str1      TYPE string,
   str2      TYPE string,
   initial   TYPE i,
 END OF tp_s_data.

DATA: ls_exp          TYPE tp_s_data,
      ls_act          LIKE ls_exp,
      lo_json_custom  TYPE REF TO lc_json_custom,
      lv_json_custom  TYPE lc_json_custom=>json.

ls_exp-tribool = lc_json_custom=>c_tribool-false.
ls_exp-bool    = lc_json_custom=>c_bool-false.
ls_exp-str1    = ''.
ls_exp-str2    = 'ABC'.
ls_exp-initial = 0.
CREATE OBJECT lo_json_custom
  EXPORTING
    compress    = abap_true
    pretty_name = lc_json_custom=>pretty_mode-camel_case.

lv_json_custom = lo_json_custom->serialize_int( data = ls_exp ).
lo_json_custom->deserialize_int( EXPORTING json = lv_json_custom CHANGING data = ls_act ).
 
" alternative way 
lc_json_custom=>deserialize_ex( EXPORTING json = lv_json_custom CHANGING data = ls_act ).
cl_aunit_assert=>assert_equals( act = ls_act exp = ls_exp msg = 'Custom pretty name fails!' ).

WRITE / lv_json_custom.
```
Results in the following JSON:
```json
{
	"triboolXxx": false,
	"str1Xxx": "",
	"str2Xxx": "ABC",
	"initialXxx": 0
}
```

# Continue reading
* [Basic usage of the class](basic.md)
* [Advanced Use cases](advanced.md)
* [FAQ](faq.md)
* [Version History](history.md)
