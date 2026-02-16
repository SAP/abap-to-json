# FAQ

* [It is slow](#it-is-slow)
* [GENERATE or DESERIALIZE into REF TO DATA vs. DESERIALIZE into a typed data structure](#generate-or-deserialize-into-ref-to-data-vs-deserialize-into-a-typed-data-structure)
* [JSON to ABAP data type conversion when using GENERATE or DESERIALIZE into REF TO DATA](#json-to-abap-data-type-conversion-when-using-generate-or-deserialize-into-ref-to-data)
* [Timestamp is not deserialized after PL22](https://github.com/SAP/abap-to-json/blob/main/docs/faq.md#timestamp-is-not-deserialized-after-pl22)
* [Serialize huge data objects into JSON and short dumps](#serialize-huge-data-objects-into-json-and-short-dumps)
* [Encoding of Unicode characters (for example, Chinese)](#encoding-of-unicode-characters-for-example-chinese)
* [Incompatible change for initial date/time fields serializing with PL16](#incompatible-change-for-initial-datetime-fields-serializing-with-pl16)
* [Is there a way to deserialize objects that have references to an Interface?](#is-there-a-way-to-deserialize-objects-that-have-references-to-an-interface)
* [Is it possible to have a defined order of fields in ABAP structures generated when deserializing into REF TO DATA fields? Is it possible to have the fields in the generated structure in the same order as in the JSON file?](#is-it-possible-to-have-a-defined-order-of-fields-in-abap-structures-generated-when-deserializing-into-ref-to-data-fields-is-it-possible-to-have-the-fields-in-the-generated-structure-in-the-same-order-as-in-the-json-file)
* [Is it possible to display the currency amount (CURR fields) formatted in the JSON output based on the related currency (CUKY field)?](#is-it-possible-to-display-the-currency-amount-curr-fields-formatted-in-the-json-output-based-on-the-related-currency-cuky-field)
* [My fields are NOT serialized as true/false, instead, serialized like 'X' or ''! E.g., how to control ABAP/JSON Boolean conversion?](#my-fields-are-not-serialized-as-truefalse-instead-serialized-like-x-or--eg-how-to-control-abapjson-boolean-conversion)
* [I can not use /UI2/CL_JSON for ABAP Cloud BADi development](#i-can-not-use-ui2cl_json-for-abap-cloud-badi-development)
* [You get a short dump OBJECTS_NOT_CHAR when serializing data with enabled conversion exits](#you-get-a-short-dump-objects_not_char-when-serializing-data-with-enabled-conversion-exits)
* [Why are special characters in JSON attribute names not escaped or unescaped?](#why-are-special-characters-in-json-attribute-names-not-escaped-or-unescaped)
* [How to define receiving structures for my JSON?](#how-to-define-receiving-structures-for-my-json)

## It is slow
It is as fast as possible to achieve, and it is already heavily optimized in pure ABAP. If you have suggestions on how to make it faster, we welcome them. Features like type conversions, type detections, renaming, data generation, etc, require processing time, and even if they are not active, you may pay the penalty because the class design allows this feature. Operations on strings are not fast in ABAP, and method calls are costly, which is why macros are used within the class. However, the class is robust and can handle any data type for serialization and deserialization, offering many convenient functions that would otherwise need to be implemented manually. It performs well in numerous use cases.

In scenarios where extensive functionality and flexibility are unnecessary, such as with simple, flat tables requiring fast JSON serialization/deserialization, tweaking /ui2/cl_json for speed by disabling flags is not viable. No other universal classes with comparable functionality are known to be faster. The best solutions currently are:

* **CALL TRANSFORMATION id**: This is the fastest alternative (10x), but it requires accepting a proprietary JSON format with missing type conversions and upper-case attribute names. It can be suitable if you control the other side and can parse the proprietary JSON.
* **CALL TRANSFORMATION with custom XSLT**: This option is slower (~7x, depending on XSLT complexity) but allows control over the JSON format through custom XSLT, including attribute names and values. However, it works only with ABAP component names and values, without RTTI information. You need to write, deliver, and synchronize your XSLT with your data structures.

For more details, please refer to the documentation: [JSON to ABAP transformation with the use of CALL TRANSFORMATION](https://github.com/SAP/abap-to-json/blob/main/docs/advanced.md#json-to-abap-transformation-with-the-use-of-call-transformation)

## GENERATE or DESERIALIZE into REF TO DATA vs. DESERIALIZE into a typed data structure
It is always better to deserialize into an explicit data structure, but not into an anonymous reference:

1. It is faster
2. It is type-safe
3. Processing deserialized results is much easier.
Deserializing into REF TO data is the same as using the GENERATE method and results in generating real-time ABAP types, which is quite slow. You can not specify the resulting types for data elements, and the deserializer needs to guess types. To process generated results, you always use dynamic programming, which is, by default, slow (or [/UI2/CL_DATA_ACCESS](https://github.com/SAP/abap-to-json/blob/main/docs/data-access.md), which is more comfortable but still uses dynamic programming inside).

## JSON to ABAP data type conversion when using GENERATE or DESERIALIZE into REF TO DATA
The data type selection logic of the GENERATE method (DESERIALIZE into REF TO data) is not guaranteed or defined. The class makes the best guess for the resulting ABAP data type based on the JSON value and the best-fitting data type on the ABAP side. For example, JSON booleans convert to ABAP_BOOL, JSON numbers can convert to I, P, or F types depending on the value, and JSON strings convert to date, time, or timestampl if the value matches a pattern; otherwise, they convert to a string. A new type of conversion may be introduced in the future. If you do not provide a fixed ABAP structure, you must be prepared to work with any of the generated types. Additionally, if you use the GEN_OPTIMIZE flag, you may receive direct types instead of references. For getting explicit data types, deserialize into a fixed structure.

## Timestamp is not deserialized after PL22
The difference that causes the changed deserialization behaviour is in the method RESTORE_TYPE. This code block:
```abap
WHEN e_typekind-ts_iso8601 OR e_typekind-tsl_iso8601.
  tstml = lcl_util=>read_iso8601( sdummy ).
  IF tstml IS INITIAL.
    tstml = lcl_util=>read_edm_datetime( sdummy ).
    IF tstml IS INITIAL.
      REPLACE FIRST OCCURRENCE OF REGEX so_regex_edm_time IN sdummy WITH '$1$2$3$4$5$6.$7' REPLACEMENT LENGTH match.
      IF sy-subrc EQ 0. " => Edm.Time
        tstml = sdummy(match).
      ENDIF.
    ENDIF.
  ENDIF.
```
It was like this in PL21:
```abap
WHEN cl_abap_typedescr=>typekind_packed.
  tstml = lcl_util=>read_iso8601( sdummy ).
  IF tstml IS INITIAL.
    tstml = lcl_util=>read_edm_datetime( sdummy ).
    IF tstml IS INITIAL.
      REPLACE FIRST OCCURRENCE OF REGEX so_regex_edm_time IN sdummy WITH '$1$2$3$4$5$6.$7' REPLACEMENT LENGTH match.
      IF sy-subrc EQ 0. " => Edm.Time
        tstml = sdummy(match).
      ENDIF.
    ENDIF.
  ENDIF.
```
E.g., in PL21 recognized timestamp is written as ISO8601/Edm.DateTime/Edm.Time in a JSON string was assigned to any packed variable. This was causing errors in cases when a packed variable was not actually a real timestamp data type, but the value was still parsed as a timestamp and wrongly assigned to the packed variable (an example error in PL21, packed values with 8 digits were assigned, but with 9 not). Because of that, I have modified the logic to do a stricter check for output data type and only extract from serialized timestamps for specific data types (see method DETECT_TYPEKIND). 
If, in your case, you get an initial value in the "timestamp" variable that was filled before PL22, probably your  data type is not one of the known timestamp types and does not inherit from them. That is why from PL22 it is not filled anymore. Fallback logic to move a string value into packed does not work here, because the string is an ISO8601 timestamp indeed. 
Supported types for now are everything that inherits (has a domain name) from TZNTSTMPS, XSDDATETIME_Z, TZNTSTMPL, XSDDATETIME_LONG_Z (see method DETECT_TYPEKIND for explicit detection logic).

## Serialize huge data objects into JSON and short dumps
You are using the class to serialize your data into JSON. Unfortunately, sometimes you pass too big tables, which results in too long a JSON string (for example, longer than 1GB), and this leads to dumps like SYSTEM_NO_ROLL, STRING_SIZE_TOO_LARGE, and MEMORY_NO_MORE_PAGING, while the system can not allocate such a big continuous memory chunk. This specific case could be solved by increasing the memory allocation limit, but you would still end up with an INT4 size limit for string length, which can not be more than 2GB.

The string (JSON) of such size can not be created, transported, or persisted. You would need to have special handling on your side for this case.
E.g., if you want to serialize such a large amount of data, you will need to split the input into chunks and do the serialization and transport of the resulting JSON chunks in parts.

The memory exceptions are not catchable, and you will need to do data size evaluations on your side before calling serialization.

So, the only robust way to solve the issue will be by limiting serialized data size, which can be done only on the /ui2/cl_json consumer side.  

Even if you select another format for serialization (XML or ABAP JSON), you will stick to some limit. So, no other way.

If you still need to serialize everything, you may split the data into chunks and give them to the serializer one by one. Then, deserialize all fragments into the same data object for merging.

## Encoding of Unicode characters (for example, Chinese)
The serializer does not do any explicit character encoding; this is done by ABAP. Normally, ABAP works with UTF-16, a 2-byte Unicode encoding that can represent any character (also Chinese). That is why you see Chinese characters in the debugger. Later on, after serializing in JSON (you may also check in the debugger JSON and see that Chinese characters are still in), you pass the JSON string further, maybe as a REST response. And there is, probably, converted into UTF8 encoding, which is a multibyte encoding, where some characters (Latin) are encoded with one byte and some (Chinese, Russian, etc.) are encoded with multiple bytes. Then the viewer of such UTF-8 text shall be able to interpret and display it properly. If you do not see characters as expected in your viewer tool, then, probably, nothing is corrupt, and the receiver will get them fine. It is just an issue of the viewer that does not recognize UTF-8, or has probably lost an encoding ID interpreted wrong.

## Incompatible change for initial date/time fields serializing with PL16
First of all, I would agree that this is an incompatible change, and I am asking you to excuse me for your efforts. It was an intentional change, and I was aware that someone could already rely on the current behavior and may have issues. 

The reason for this change of default was a customer complaint regarding the handling of initial date-time values, which are not 0000-00-00 or 00:00:00. In general, 0000-00-00 is an invalid date, 00:00:00 is valid, but how to understand that it is an initial but not an explicit midnight?

Because of that, I have decided not to render initial values for date/time and give a receiver a way to understand that it is initial and has its default/initial processing. I know that it is incompatible, but I want a default behavior to be the best and most common choice, even with the cost of modification of the consumer code that relies on the old behavior :/.

Because having custom rendering of the initial date/time is quite exotic, I have only let this for constructor calls and have not extended the serialize method, to keep the standard API simple. If I get multiple requests regarding extending SERIALIZE with these defaults, I will do it. 

My recommendation for you:
* Variant 1: Adopt your unit tests for new initial values for date-time. 
* Variant 2: Use the instance method for serialization. E.g., parametrized CONSTRUCTOR + SERIALIZE_INT (in this case, you can customize behavior by parametrizing the instance constructor with desired initial values for time and date).
* Variant 2: Extend the /ui2/cl_json class or create a helper method in your class with your static customized SERIALIZE call, which already considers new defaults for the /ui2/cl_json constructor.

## Is there a way to deserialize objects that have references to an Interface?
**Q**: I am using /ui2/cl_json to serialize an object that contains some reference attributes. These reference attributes are TYPE REF TO <Interface>. Upon deserialization, the references are not getting deserialized. Is there a way to deserialize objects that have references to an Interface?

**A**: Unfortunately, not. To deserialize an object, it shall be created. And how would you like to create an instance of the interface without knowing the class? It can not be done automatically. But you may try to [implement the deserialization logic by yourself](docs/advanced.md#jsonabap-serializationdeserialization-with-runtime-type-information).

## Is it possible to have a defined order of fields in ABAP structures generated when deserializing into REF TO DATA fields? Is it possible to have the fields in the generated structure in the same order as in the JSON file?
The order of fields in JSON and also in ABAP is undefined. It may happen that you will have two records of the same type in an array, but with attributes serialized in different orders. 
What to do in this case? In general, the answer is – no (there is no way to configure it). The current alphabetical order gives at least some predefined output (but the result is a name normalization and uniqueness check). 

If you want a specific order, just deserialize it in a predefined structure, but not in REF TO DATA. Generating into REF TO DATA is always a bad choice (from a performance and type definition perspective). 

But there is no easy way in the latest releases of the class.
If you still want a predefined sequence of the fields in the generated structure, you may inherit the class and prefill the structure buffer (mt_struct_type) in your inherited class constructor. See method GENERATE_STRUCT for details. In this case, later deserialize/generate calls will use your structure type, but not one created with default logic.

## Is it possible to display the currency amount (CURR fields) formatted in the JSON output based on the related currency (CUKY field)?
No, there is no built-in support for currency fields. Potentially, one can add it in a derived class, overwriting dump_int and restore_type methods, but I do not want to have it by default, because of implementation complexity and performance penalty. 

Only single-field conversion exits are supported. 

## My fields are NOT serialized as true/false, instead, serialized like 'X' or ''! E.g., how to control ABAP/JSON Boolean conversion?
JSON, as JavaScript, has a built-in Boolean type with true/false values. ABAP does not have a built-in Boolean type and uses fields of char 1 with constant values of 'X' (TRUE) and ''(space, FALSE). Different teams use different predefined types to be used as a Boolean type for them. It is a zoo. There is no way to detect the Boolean type or even be able to auto-convert them between different ABAP types. However, there are some more or fewer standard conventions for which standard types shall be used for booleans. The serializer class has the default list of standard boolean types hardcoded in constant MC_BOOL_TYPES (ABAP_BOOLEAN, ABAP_BOOL, BOOLEAN, BOOLE_D, XFELD, XSDBOOLEAN, WDY_BOOLEAN). If you use one of these types in your data, passed to the serializer, it will be automatically processed by the parser and converted from ''/'X' into false/true and vice versa. If you use any other type not in the list, there will be no auto-conversion. OK, you do not like the default (it processes too many types or two fewer), what do you do? You have the following choices:
* Do not use static methods for serialization and deserialization, but instance ones (e.g., json_obj-> serialize_int instead of /ui2/cl_json=> serialize) AND customize the behavior of the instance by constructor parameters. You can pass an alternative set of boolean types with the parameter BOOL_TYPES. The usage of instance methods is also faster if you repeat calls for serialization.
* Inherit the class and overwrite the default boolean types, stored in the class variable mv_bool_types, with your preferred default. Use your class everywhere instead of standard, to ensure consistency. How to inherit the class, you can find [here](docs/class-extension.md). 

## I can not use /UI2/CL_JSON for ABAP Cloud BADi development
The class has been released for ABAP Cloud development (Steampunk) for a long time. Initially, it was released only for the Public Cloud, and the Private Cloud was missed by mistake. That was corrected, and now you can use it for Private Cloud from release OP 2023 (SAP_BASIS 758). See details [here](docs/history.md#note-3424850-ui2cl_json-release-api-for-cloud-development). If you need JSON processing in ABAP Cloud BADis, you may need to use the XCO library (XCO_JSON), which is meant to be the official JSON processing library for Key User Extensibility Apps. If you still think that the use of /UI2/CL_JSON would be preferable, you may ask for its release via the Customer Influence program, as it was [done for Steampunk sometime](https://influence.sap.com/sap/ino/#/idea/234724/?section=sectionVotes). 

## You get a short dump OBJECTS_NOT_CHAR when serializing data with enabled conversion exits
You can apply conversion exits to serialized data when using the /ui2/cl_json. The class uses a temporary buffer of type STRING as an output for conversion exits. But some old conversion exits support only writing in char-like variables (C LENGTH...) (restriction of WRITE TO) and dumping when WRITE TO is executed with STRING output. But the current [programming guidelines for conversion exits](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconversion_exits.htm) say that output can be [C-LIKE type](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenbuilt_in_types_generic.htm) (c, n, and string, as well as the date/time types d, t, and character-like flat structures). So it can be a string. 
If you get such a dump, please raise a message and ask the conversion exit owner to update the code to support STRING type, following the example implementation for CONVERSION_EXIT_SDURA_OUTPUT from SAP Help]:
```abap
FUNCTION CONVERSION_EXIT_SDURA_OUTPUT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CLIKE
*"----------------------------------------------------------------------

  hours     = input DIV 60.
  minutes_n = input MOD 60.

  DESCRIBE FIELD output TYPE DATA(typ).

  IF typ = 'g'. "OUTPUT is type string, no WRITE TO for strings, enabled with string templates 20130423, KELLERH
    output = |{ hours WIDTH = 3 ALIGN = RIGHT }:{ minutes_n WIDTH = 2 }|.
  ELSE. "Old overflow behavior to stay compatible
    WRITE hours TO output(3) NO-SIGN.
    output+3(1) = ':'.
    WRITE minutes_n TO output+4(2).
  ENDIF.
ENDFUNCTION.
```
Use of output buffer TYPE C LENGHT ... in code of /ui2/cl_json would require an additional CONDENSE call that would negatively impact the performance of serialization and may still lead to incorrect data rendering (the logic with TYPE C LENGHT... was in PL19, but is reverted with PL 20, because of [this issue](issues/10)). 

## Why are special characters in JSON attribute names not escaped or unescaped?
This is a known limitation. Escaping, and especially unescaping, is very performance-critical and will significantly influence parsing time. Cases where attribute names contain special characters are unique — ABAP field names do not allow special characters. So, to optimize overall performance, I have decided not to support this. The only cases when the parser does escaping and unescaping the attribute names are usages of the ASSOC_ARRAYS flag when table key values are converted into leading attribute names (associative arrays in terms of JSON) and generation of the structures (internally it also uses ASSOC_ARRAY flag). 

## How to define receiving structures for my JSON?
This [online tool](https://www.findocs.xyz/tools/sap/json-to-abap) may help you to generate proper receiving ABAP structures for your input JSON.

# Continue reading
* [Basic usage of the class](basic.md)
* [Advanced Use cases](advanced.md)
* [Version History](history.md)
