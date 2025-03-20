# FAQ

* [It is slow](#it-is-slow)
* [GENERATE or DESERIALIZE into REF TO DATA vs. DESERIALIZE into a typed data structure](#generate-or-deserialize-into-ref-to-data-vs-deserialize-into-a-typed-data-structure)
* [JSON to ABAP data type conversion when using GENERATE or DESERIALIZE into REF TO DATA](#json-to-abap-data-type-conversion-when-using-generate-or-deserialize-into-ref-to-data)
* [Serialize huge data objects into JSON and short dumps](#serialize-huge-data-objects-into-json-and-short-dumps)
* [Encoding of Unicode characters (for example Chinese)](#encoding-of-unicode-characters-for-example-chinese)
* [Incompatible change for initial date/time fields serializing with PL16](#incompatible-change-for-initial-datetime-fields-serializing-with-pl16)
* [Is there a way to deserialize objects that have references to Interface?](#is-there-a-way-to-deserialize-objects-that-have-references-to-interface)
* [Is it possible to have a defined order of fields in ABAP structures generated when deserializing into REF TO DATA fields? Is it possible to have the fields in the generated structure in the same order as in the JSON file?](#is-it-possible-to-have-a-defined-order-of-fields-in-abap-structures-generated-when-deserializing-into-ref-to-data-fields-is-it-possible-to-have-the-fields-in-the-generated-structure-in-the-same-order-as-in-the-json-file)
* [Is it possible to display the currency amount (CURR fields) formatted in the JSON output based on the related currency (CUKY field)?](#is-it-possible-to-display-the-currency-amount-curr-fields-formatted-in-the-json-output-based-on-the-related-currency-cuky-field)
* [My fields are/NOT serialized as true/false instead and serialized like 'X' or ''! E.g. how to control ABAP/JSON Boolean conversion?](#my-fields-arenot-serialized-as-truefalse-instead-and-serialized-like-x-or--eg-how-to-control-abapjson-boolean-conversion)
* [I can not use /UI2/CL_JSON for ABAP Cloud BADi development](#i-can-not-use-ui2cl_json-for-abap-cloud-badi-development)
* [You get a short dump OBJECTS_NOT_CHAR when serializing data with enabled conversion exits](#you-get-a-short-dump-objects_not_char-when-serializing-data-with-enabled-conversion-exits)
* [Why special characters in JSON attribute names are not escaped or unescaped?](#why-are-special-characters-in-json-attribute-names-not-escaped-or-unescaped)

## It is slow
It is as fast as possible to achieve it in pure ABAP. If you have suggestions on how to make it faster - you are welcome. Features like type conversions, type detections, renaming, data generation, etc require processing time, and even if they are not active you may pay the penalty because the class design allows this feature. Operation on strings is not fast in ABAP, calling of the methods is not cheap (that is why the class uses macros). More to come...

## GENERATE or DESERIALIZE into REF TO DATA vs. DESERIALIZE into a typed data structure
It is always better to deserialize into explicit data structure but not into anonymous reference:

1. It is faster
2. It is type-safe
3. Processing deserialized results is much easier.
Deserializing into REF TO data is the same as using the GENERATE method and results in generating real-time ABAP types, which is quite slow. You can not specify the resulting types for data elements and the deserializer needs to guess types. To process generated results, you always use dynamic programming, which is by default slow (or [/UI2/CL_DATA_ACCESS](https://github.com/SAP/abap-to-json/blob/main/docs/data-access.md), which is more comfortable but still uses dynamic programming inside).

## JSON to ABAP data type conversion when using GENERATE or DESERIALIZE into REF TO DATA
The data type selection logic of the GENERATE method (DESERIALIZE into REF TO data) is not guaranteed or defined. The class makes the best guess for the resulting ABAP data type based on the JSON value and the best-fitting data type on the ABAP side. For example, JSON booleans convert to ABAP_BOOL, JSON numbers can convert to I, P, or F types depending on the value, and JSON strings convert to date, time, or timestampl if the value matches a pattern, otherwise, they convert to a string. A new type of conversion may be introduced in the future. If you do not provide a fixed ABAP structure, you must be prepared to work with any of the generated types. Additionally, if you use the GEN_OPTIMIZE flag, you may receive direct types instead of references. For explicit data types, deserialize into a fixed structure.

## Serialize huge data objects into JSON and short dumps
You are using the class to serialize your data into JSON. Unfortunately, sometimes you pass too big tables, which results in too long a JSON string (for example, longer than 1GB), and this leads to dumps like SYSTEM_NO_ROLL, STRING_SIZE_TOO_LARGE, MEMORY_NO_MORE_PAGING, while the system can not allocate such a big continuous memory chunk. This specific case could be solved by increasing the memory allocation limit, but you would still end up with an INT4 size limit for string length, which can not be more than 2GB.

The string (JSON) of such size can not be created, transported, or persisted. You would need to have special handling on your side for this case.
E.g. if you want to serialize such a big amount of data, you will need to split the input into chunks and do the serialization and transport of the resulting JSON chunks by parts.

The memory exceptions are not catchable and you will need to do data size evaluations on your side, before calling serialization.

So, the only robust way to solve the issue will be by limiting serialized data size, which can be done only on the /ui2/cl_json consumer side.  

Even if you select another format for serialization (XML or ABAP JSON) you will stick to some limit. So, no other way.

If you still need to serialize everything, you may split data into chunks and give them to the serializer one by one. Then deserialize all fragments into the same data object for merging.

## Encoding of Unicode characters (for example Chinese)
The serializer does not do any explicit character encoding, this is done by ABAP. Normally, ABAP works with UTF16, a 2-byte Unicode encoding that can represent any character (also Chinese). That is why you see Chinese characters in the debugger. Later on, after serializing in JSON (you may also check in debugger JSON and see that Chinese characters are still in), you pass the JSON string further, maybe as a REST response. And there is, probably, converted into UTF8 encoding, which is multibyte encoding, where some characters (Latin) are encoded with one byte and some (Chinese, Russian, etc.) as multibyte. Then the viewer of such UTF8 text shall be able to interpret and display them properly. If you do not see characters as expected in your viewer tool, then, probably, nothing is corrupt and the receiver will get them fine. It is just an issue of the viewer that does not recognize UTF8, or probably, lost an encoding ID interpreted wrong.

## Incompatible change for initial date/time fields serializing with PL16
First of all, I would agree that this is an incompatible change and I am asking you to excuse me for your efforts. It was an intentional change and I was aware that someone can already rely on current behavior and may get issues. 

The reason for this change of default was a customer complaint regarding the handling of initial date-time values, which are not 0000-00-00 or 00:00:00. In general 0000-00-00 is an invalid date, 00:00:00 is valid, but how to understand that it is initial but not explicit midnight?

Because of that, I have decided not to render initial values for date/time and give a receiver a way to understand that it is initial and has its default/initial processing. I know that it is incompatible, but I want a default behavior to be the best and most common choice, even with the cost of modification of the consumer code that relies on old behavior :/.

Because having custom rendering of the initial date/time is quite exotic, I have only let this for constructor calls and have not extended the serialize method, to keep standard API simple. If I get multiple requests regarding extending SERIALIZE with these defaults - I will do it. 

My recommendation for you:
* Variant 1: Adopt your unit tests for new initial values for date-time. 
* Variant 2: Use the instance method for serialization. E.g. parametrized CONSTRUCTOR + SERIALIZE_INT (in this case you can customize behavior by parametrizing instance constructor with desired initial values for time date).
* Variant 2: Extend the /ui2/cl_json class or create a helper method in your class with your static customized SERIALIZE call which already considers new defaults for the /ui2/cl_json constructor.

## Is there a way to deserialize objects that have references to Interface?
**Q**: I am using /ui2/cl_json to serialize an object that contains some reference attributes. These reference attributes are TYPE REF TO <Interface>. Upon deserialization, the references are not getting deserialized. Is there a way to deserialize objects that have references to Interface?

**A**: Unfortunately - not. To deserialize an object, it shall be created. And how would you like to create an instance of the interface without knowing the class? It can not be done automatically. But you may try to [implement the deserialization logic by yourself](docs/advanced.md#jsonabap-serializationdeserialization-with-runtime-type-information).

## Is it possible to have a defined order of fields in ABAP structures generated when deserializing into REF TO DATA fields? Is it possible to have the fields in the generated structure in the same order as in the JSON file?
The order of fields in JSON and also in ABAP is undefined. It may happen that you will have two records of the same type in an array but with attributes serialized in different orders. 
What to do in this case? In general, the answer is – no (there is no way to configure it). The current alphabetical order gives at least some predefined output (but the result is a name normalization and uniqueness check). 

If you want a specific order, just deserialize it in a predefined structure, but not in REF TO DATA. Generating into REF TO DATA is always a bad choice (from a performance and type definition perspective). 

But there is not an easy way, in the latest releases of the class.
If you still want a predefined sequence of the fields in the generated structure, you may inherit the class, and prefill structure buffer (mt_struct_type) in your inherited class constructor. See method GENERATE_STRUCT for details. In this case, later deserialize/generate calls will use your structure type, but not one created with default logic.

## Is it possible to display the currency amount (CURR fields) formatted in the JSON output based on the related currency (CUKY field)?
No, there is no built-in support for currency fields. Potentially one can add it in a derived class, overwriting dump_int and restore_type methods, but I do not want to have it in by default, because of implementation complexity and performance penalty. 

Only single-field conversion exits are supported. 

## My fields are/NOT serialized as true/false instead and serialized like 'X' or ''! E.g. how to control ABAP/JSON Boolean conversion?
JSON, as JavaSctript, has a built-in Boolean type with true/false values. ABAP does not have a built-in Boolean type and uses fields of char 1 with constant values of 'X' (TRUE) and ''(space, FALSE). Different teams use different predefined types to be used as a Boolean type for them. It is a zoo. There is no way to detect the boolean type or even be able to auto-convert them between different ABAP types. However, there are some more or fewer standard conventions for which standard types shall be used for booleans. The serializer class has the default list of standard boolean types hardcoded in constant MC_BOOL_TYPES (ABAP_BOOLEAN, ABAP_BOOL, BOOLEAN, BOOLE_D, XFELD, XSDBOOLEAN, WDY_BOOLEAN). If you use one of these types in your data, passed to the serializer, it will be automatically processed by the parser and converted from ''/'X' into false/true and vice versa. If you use any other type not in the list, there will be no auto-conversion. OK, you do not like default (it processes too many types or two less), what do you do? You have the following choices:
* Do not use static methods for serialization and deserialization, but instance ones (e.g. json_obj->serilaize_int instead of /ui2/cl_json=>serilaize) AND customize the behavior of the instance by constructor parameters. You can pass an alternative set of boolean types with the parameter BOOL_TYPES. The usage of instance methods is also faster if you repeat calls for serialization.
* Inherit the class and overwrite default boolean types, stored in the class variable mv_bool_types with your preferred default. Use your class everywhere instead of standard, to ensure consistency. How to inherit the class you can find [here](docs/class-extension.md). 

## I can not use /UI2/CL_JSON for ABAP Cloud BADi development
The class has been released for ABAP Cloud development (Steampunk) for a long time. Initially, it was released only for the Public Cloud, and the Private Cloud was missed by mistake. That was corrected and now you can use it for Private Cloud from release OP 2023 (SAP_BASIS 758). See details [here](docs/history.md#note-3424850-ui2cl_json-release-api-for-cloud-development). If you need JSON processing in ABAP Cloud BADis, you may need to use the XCO library (XCO_JSON), which is meant to be the official JSON processing library for Key User Extensibility Apps. If you still think that the use of /UI2/CL_JSON would be preferable you may ask for releasing of it via the Customer Influence program, like it was [done for Steampunk sometime](https://influence.sap.com/sap/ino/#/idea/234724/?section=sectionVotes). 

## You get a short dump OBJECTS_NOT_CHAR when serializing data with enabled conversion exits
You can apply conversion exits to serialized data when using the /ui2/cl_json. The class uses a temporary buffer of type STRING as an output for conversion exits. But some old conversion exits support only writing in char-like variables (C LENGHT ...) (restriction of WRITE TO) and dumping when WRITE TO is executed with STRING output. But the current [programming guidelines for conversion exits](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconversion_exits.htm) say that output can be [C-LIKE type](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenbuilt_in_types_generic.htm) (c, n, and string, as well as the date/time types d, t and character-like flat structures). So it can be a string. 
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
Use of output buffer TYPE C LENGHT ... in code of /ui2/cl_json would require an additional CONDENSE call that would negatively impact the performance of serialization and may still lead to incorrect data rendering (the logic with TYPE C LENGHT... was in PL19, but is reverted with PL 20, because on [this issue](issues/10)). 

## Why are special characters in JSON attribute names not escaped or unescaped?
This is a known limitation. Escaping, and especially unescaping, is very performance-critical and will significantly influence parsing time. Cases where attribute names contain special characters are quite unique — ABAP field names do not allow special characters. So, to optimize overall performance, I have decided not to support this. The only cases when the parser does escaping and unescaping the attribute names are usages of the ASSOC_ARRAYS flag when table key values are converted into leading attribute names (associative arrays in terms of JSON) and generation of the structures (internally it also uses ASSOC_ARRAY flag). 

# Continue reading
* [Basic usage of the class](basic.md)
* [Advanced Use cases](advanced.md)
* [Version History](history.md)
