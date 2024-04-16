# FAQ

## GENERATE or DESERIALIZE into REF TO DATA vs. DESERIALIZE into a typed data structure
It is always better to deserialize into explicit data structure but not into anonymous reference:

1. It is faster
2. It is type-safe
3. Processing deserialized results is much easier.
Deserializing into REF TO data is the same as using the GENERATE method and results in generating real-time ABAP types, which is quite slow. You can not specify the resulting types for elements and the deserializer needs to guess. To process generated results, you need to always use dynamic programming by default slow (or /UI2/CL_DATA_ACCESS, which is more comfortable but still uses dynamic programming inside).

## Serialize huge data objects into JSON and short dumps
You are using the class to serialize your data into JSON. Unfortunately, sometimes you pass too big tables, which results in too long a JSON string (for example, longer than 1GB), and this leads to dumps like SYSTEM_NO_ROLL, STRING_SIZE_TOO_LARGE, MEMORY_NO_MORE_PAGING, while the system can not allocate such a big continuous memory chunk. Potentially this specific case can be solved by increasing the memory allocation limit, but you would still end up with an INT4 size limit for string length, which can not be more than 2GB size.

The string (JSON) of such size can not be created and also can not be transported or persisted. You would need to have special handling on your side for this case.
E.g. if you want to serialize such a big amount of data, you will need to split the input into chunks and do the serialization and transport of the resulting JSON chunks by parts.

The memory exceptions are not catchable and you will need to do data size evaluations on your side, before calling serialization.

So, the only robust way to solve the issue will be by having a limit on serialized data size. Which can be done only on the /ui2/cl_json consumer side.  

Even if you select another format for serialization (XML or ABAP JSON) you will stick to some limit. So, no other way.

If you still need to serialize everything, you may split data into chunks and give them to the serializer one by one. Then deserialize all fragments into the same data object for merging.

## Encoding of Unicode characters (for example Chinese)
The serializer does not do any explicit character encoding, this is done by ABAP. Normally, ABAP works with UTF16, a 2-byte Unicode encoding that can represent any character (also Chinese). That is why you see Chinese characters in the debugger. Later on, after serializing in JSON (you may also check in debugger JSON and see that Chinese characters are still in), you pass the JSON string further, maybe as a REST response. And there is, probably, converted into UTF8 encoding, which is multibyte encoding, where some characters (Latin) are encoded with one byte and some (Chinese, Russian, etc.) as multibyte. Then the viewer of such UTF8 text shall be able to interpret and display them properly. If you do not see characters as expected in your viewer tool, then, probably, nothing is corrupt and the receiver will get them fine. It is just an issue of the viewer that does not recognize UTF8, or probably, lost an encoding ID interpreted wrong.

## Incompatible change for initial date/time fields serializing with PL16
First of all, I would agree that this is an incompatible change and I am asking you to excuse me for your efforts. It was an intentional change and I was aware that someone can already rely on current behavior and may get issues. 

The reason for this change of default was a customer complaint regarding the handling of initial date-time values, which are not 0000-00-00 or 00:00:00. In general 0000-00-00 is an invalid date, 00:00:00 is valid, but how to understand that it is initial but not explicit midnight?

Because of that, I have decided not to render initial values for date/time and give a receiver a way to understand that it is initial and has its processing of default/initial. I know that it is incompatible, but I want a default behavior to be the best and most common choice, even with the cost of modification of the consumer code that relies on old behavior :/.

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
What to do in this case? In general, the answer is – no (there is no way to configure it). Current alphabetical order gives at least some predefined output (but the result of name normalization and uniqueness check). 

If you want a specific order – just deserialize in a predefined structure, but not in REF TO DATA. Generating into REF TO DATA is always a bad choice (from a performance and type definition perspective). 

But there is not an easy way, in the latest releases of the class.
If you still want a predefined sequence of the fields in the generated structure, you may inherit the class, and prefill structure buffer (mt_struct_type) in your inherited class constructor. See method GENERATE_STRUCT for details. In this case, later deserialize/generate calls will use your structure type, but not one created with default logic.

## Is it possible to display the currency amount (CURR fields) formatted in the JSON output based on the related currency (CUKY field)?
No, there is no built-in support for currency fields. Potentially one can add it in a derived class, overwriting dump_int and restore_type methods, but I do not want to have it in by default, because of implementation complexity and performance penalty. 

Only single-field conversion exits are supported. 

## My fields are/NOT serialized as true/false instead and serialized like 'X' or ''! E.g. how to control ABAP/JSON Boolean conversion?
JSON, as JavaSctript, has a built-in Boolean type with true/false values. ABAP does not have a built-in Boolean type and uses fields of char 1 with constant values of 'X' (TRUE) and ''(space, FALSE). Different teams use different predefined types to be used as a Boolean type for them. It is a zoo. There is no way to detect the boolean type or even be able to auto-convert them between different ABAP types. But there are some more or less standard conventions of which standard types shall be used for booleans. The serializer class has the default list of standard boolean types hardcoded in constant MC_BOOL_TYPES (ABAP_BOOLEAN, ABAP_BOOL, BOOLEAN, BOOLE_D, XFELD, XSDBOOLEAN, WDY_BOOLEAN). If you use one of these types in your data, passed to the serializer, it will be automatically processed by the parser and converted from ''/'X' into false/true and vice versa. If you use any other type not in the list, there will be no auto-conversion. OK, you do not like default (it processes too many types or two less), what to do? You have the following choices:
* Do not use static methods for serialization and deserialization, but instance ones (e.g. json_obj->serilaize_int instead of /ui2/cl_json=>serilaize) AND customize the behavior of the instance by constructor parameters. You can pass an alternative set of boolean types with the parameter BOOL_TYPES. The usage of instance methods is also faster if you repeat calls for serialization.
* Inherit the class and overwrite default boolean types, stored in the class variable mv_bool_types with your preferred default. Use your class everywhere instead of standard, to ensure consistency. How to inherit the class you can find [here](docs/class-extension.md). 


# Continue reading
* [Basic usage of the class](basic.md)
* [Advanced Use cases](advanced.md)
* [Version History](history.md)
