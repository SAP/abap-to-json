# FAQ

## GENERATE or DESERIALIZE into REF TO DATA vs DESERIALIZE into a typed data structure
It is always better to deserialize into explicit data structure but not into anonymous reference:

1. It is faster
2. It is type-safe
3. Processing deserialized result is much easier.
Deserializing into REF TO data is the same as using of GENERATE method and results in generating real-time ABAP types, which is quite slow. You can not specify the resulting types for elements and the deserializer need to guess. To process generated results, you need to always use dynamic programming by default slow (or /UI2/CL_DATA_ACCESS, which is more comfortable but still uses dynamic programming inside).

## Serialize huge data objects into JSON and short dumps
You are using the class to serialize your data into JSON. Unfortunately, sometimes you pass too big tables, which results in too long a JSON string (for example, longer than 1GB), and this leads to dumps like SYSTEM_NO_ROLL, STRING_SIZE_TOO_LARGE, MEMORY_NO_MORE_PAGING, while the system can not allocate such a big continuous memory chunk. Potentially this specific case can be solved by increasing the memory allocation limit, but you would still end up with an INT4 size limit for string length, which can not be more than 2GB size.

The string (JSON) of such size can not be created and also can not be transported or persisted. You would need to have special handling on your side for this case.
E.g. if you want to serialize such a big amount of data, you will need to split the input into chunks and do the serialization and transport of the resulting JSON chunks by parts.

The memory exceptions are not catchable and you will need to do data size evaluations on your side, before calling serialization.

So, the only robust way to solve the issue will be by having a limit on serialized data size. Which can be done only on the /ui2/cl_json consumer side.  

Even if you would select another format for serialization (XML or ABAP JSON) you will stick to some limit. So, no other way.

If you still need to serialize everything, you may split data into chunks and give them to the serializer one by one. And then do deserialize all fragments into the same data object for merging.

## Encoding of Unicode characters (for example Chinese)
The serializer does not do any explicit character encoding, this is done by ABAP. Normally, ABAP works with UTF16, a 2-byte Unicode encoding that can represent any character, also Chinese. That is why you see Chinese characters in the debugger. Later on, after serializing in JSON (you may also check in debugger JSON and see that Chinese characters are still in), you pass the JSON string further, maybe as a REST response. And there is, probably, goes converted into UTF8 encoding, which is multibyte encoding, where some characters (Latin) are encoded with one byte and some (Chinese, Russian, etc) as multibyte. And then the viewer of such UTF8 text shall be able to interpret and display them properly. If you do not see characters as expected in your viewer tool, then, probably, nothing is corrupt and the receiver will get them fine. It is just an issue of the viewer that does not recognize UTF8, or probably, lost an encoding ID interpreted wrong.

## Incompatible change for initial date/time fields serializing with PL16
First of all, I would agree that this is an incompatible change and I am asking you to excuse me for your efforts. But it was an intentional change and I was aware that someone can already rely on current behavior and may get issues. 

The reason for this change of default was a customer complaint regarding the handling of initial date-time values, which are not 0000-00-00 or 00:00:00. In general 0000-00-00 is an invalid date, 00:00:00 is valid, but how to understand that it is initial but not explicit midnight?

Because of that, I have decided not to render initial values for date/time and give a receiver a way to understand that it is initial and has its processing of default/initial. I know that it is incompatible, but I want that a default behavior would be the best and most common choice, even with the cost of modification of the consumer code that relies on old behavior :/.

Because having custom rendering of the initial date/time is quite exotic, I have only let this for constructor calls and have not extended the serialize method, to keep standard API simple. If I would get multiple requests regarding extending SERIALIZE with these defaults - I will do. 

My recommendation for you:
* Variant 1: Adopt your unit tests for new initial values for date-time. 
* Variant 2: Use the instance method for serialization. E.g. parametrized CONSTRUCTOR + SERIALIZE_INT.
* Variant 2: Extend the /ui2/cl_json class or create a helper method in your class with your static SERIALIZE call which already considers new defaults for the /ui2/cl_json constructor.
