# Version History
   * [Note 3568088 - PL22](#note-3615316---pl22)
   * [Note 3568088 - PL21](#note-3568088---pl21)
   * [Note 3515438 - PL20](#note-3515438---pl20)
   * [Note 3414589 - PL19](#note-3414589---pl19)
   * [Note 3424850 - /UI2/CL_JSON - Release API for Cloud Development](#note-3424850---ui2cl_json---release-api-for-cloud-development)
   * [Note 3315430 - PL18](#note-3315430---pl18)
   * [Note 3106267 - PL17](#note-3106267---pl17)
   * [Note 3038042 - PL16](#note-3038042---pl16)
   * [Note 2944398 - PL15](#note-2944398---pl15)
   * [Note 2904870 - PL14](#note-2904870---pl14)
   * [Note 2870163 - PL13](#note-2870163---pl13)
   * [Note 2798102 - PL12](#note-2798102---pl12)
   * [Note 2786259 - PL11](#note-2786259---pl11)
   * [Note 2763854 - PL10](#note-2763854---pl10)
   * [Note 2650040](#note-2650040)
   * [Note 2629179](#note-2629179)
   * [Note 2526405](#note-2526405)
   * [Note 2292558](#note-2292558)
   * [Note 2300508](#note-2300508)
   * [Note 2330592](#note-2330592)
   * [Note 2368774](#note-2368774)
   * [Note 2382783](#note-2382783)
   * [Note 2429758](#note-2429758)
   * [Note 2480119](#note-2480119)

## Note [3615316](https://launchpad.support.sap.com/#/notes/3615316) - PL22 (not released)
### /UI2/CL_JSON
* Fixed: Bug with generation of the structures with similar field names (fex, "a", "bc" vs "ab", "c").
* Fixed: No exception is propagated when fired from within the conversion exit routine (deserialize)
* Fixed: Problems with deserialization of packed fields with length 8 from a string value
* Fixed: Bug during conversion from EDM time into Unix time (it shall be truncation for seconds extraction instead of rounding)
* Added: new parameter for code page for DESERIALIZE/DESERIALIZE_INT methods for proper internal conversion from XSTRING to STRING. 

## Note [3568088](https://launchpad.support.sap.com/#/notes/3568088) - PL21
### /UI2/CL_JSON
* Fixed: The wrong elementary type is used when generating data for REF TO data (TIMESTAMP instead of string)
* Fixed: attribute names created from table key values are not escaped/unescaped when using the ASSOC_ARRAYS option.
* Fixed: additional performance optimization for deserializing long strings with escaped characters. Because of the use of CALL TRANSFORMATION id for JSON, it requires kernel updates for SAP_BASIS 7.02 and 7.31 (SAP Notes 1648418 and 1650141).

## Note [3515438](https://launchpad.support.sap.com/#/notes/3515438) - PL20
### /UI2/CL_JSON
* Fixed: Empty objects ( "a":{} ) are not initialized when using GENERATE or DESERIALIZE into REF TO data, taking the value of the previous field
* Added: Support for PascalCase for pretty printing was added.
* Fixed. The logic for OUTPUT conversion exits reverted to the state of PL18 ([details](https://github.com/SAP/abap-to-json/issues/10)). 

## Note [3414589](https://launchpad.support.sap.com/#/notes/3414589) - PL19
### /UI2/CL_JSON
* Fixed: enhanced processing of deserialization into typed TYPE REFs
* Fixed: deserialization of JSON 'null' into complex, not reference ABAP fields does not lead to exception in strict mode ([details](https://github.com/SAP/abap-to-json/pull/5))
* Fixed: Support for ABAP_BOOLEAN type was added.
* Fixed: performance optimization for deserializing strings with escaped line breaks and special characters. When you have escaped "\\", e.g., "\\\\", it is still slow.
* Fixed: short dump in conversion exits routines on SERIALIZE/DESERIALIZE because of the wrong data type (OBJECTS_NOT_CHAR).
* Fixed. Added support for the timezone offsets for ISO8601.
* Fixed: rounding bug when deserializing timestamps with sub-seconds into short timestamps (seconds)
* New: Information about the invalid field added to the exception data of move_cast_error ([details](https://github.com/SAP/abap-to-json/pull/8))
* New: Serialization now can detect "timestamps" defined with the help of data domains ([details](https://github.com/SAP/abap-to-json/pull/9))
* New: new switch for GENERATE (optimize) and DESERIALIZE (gen_optimize) that enables optimization of generated ABAP data for REF TO DATA (fewer references, easily readable and accessible). Results in longer processing.
* New: added detection of time, date, and timestamp values on generation/deserialization into TYPE REF TO data.

## Removed dependency to /ui2/cl_data_access for z_ui2_json
* Added: Open Source version of the /ui2/cl_data_access as Z_UI2_DATA_ACCESS

## Note [3424850](https://launchpad.support.sap.com/#/notes/3424850) - /UI2/CL_JSON - Release API for Cloud Development
You can not use the class in the Private Cloud, because the class has not been released for Cloud Development (for the Public Cloud, it was already released).
Delivered with OP 2025 and OP 2023 FPS2. Note for OP 2023 (SAP_BASIS 758)

## Note [3315430](https://launchpad.support.sap.com/#/notes/3315430) - PL18
### /UI2/CL_JSON
* Fixed: handling of cycle references when serializing data and object references. The serialization will stop processing of the reference if it is already in the serialization stack.
* Fixed: performance by serialization of timestamps and UTCLONG fields ([details](https://github.com/SAP/abap-to-json/issues/4)).
* Fixed: performance in class-constructor.
* Fixed: deserialization into typed TYPE REF does not work as expected - always the generic GENERATE approach is used.
* Fixed: null references generated as ABAP_BOOL types

## Note [3106267](https://launchpad.support.sap.com/#/notes/3106267) - PL17
### /UI2/CL_JSON
* Fixed: added support for XSDBOOLEAN Boolean type, to be consistent with CALL TRANSFORMATION id rules.
* Fixed: added support for UTCLONG. This new built-in ABAP data type comes with SAP_BASIS 7.54 and adds native support for timestamps to ABAP. The type is always represented in ISO8601 and does not depend on the TS_AS_ISO8601 parameter.
* Fixed: serialization of the timestamp fields into ISO8601 does not add any more sub-second sections into JSON, while it is always initial for timestamps.
* Fixed: deserialization of the JSON strings with non-breakable spaces (nbsp)
* Fixed: processing of the JSON attribute names with escaped double quotes ("abc efg \\\" etc": "value")
* Fixed: deserialization (generation without typed output structure) does not consider exponential numeric values and tries to transform them into an integer and fails, resulting in 0.

### New language features used:
* escape function => min requirement SAP_BASIS 7.31
* find_any_not_of function => min requirement SAP_BASIS 7.02
* FIND/REPLACE PCRE => min requirement SAP_BASIS 7.55 (CE 2008/OP 2020) - delayed

## Note [3038042](https://launchpad.support.sap.com/#/notes/3038042) - PL16
### /UI2/CL_JSON
* Fixed: Serializing of hash tables with empty values and the parameters assoc_arrays_opt and compress produces invalid JSON. 
* Fixed: Method serialize produces corrupt data when an initial date or time field is serialized (something like "--" or "::" ).
* Fixed: Performance for serializing and deserializing dynamic data objects (REF TO DATA) is greatly improved. 
* New: You can apply text formatting/beautifying to serialized JSON with the new parameter format_output.
* New: You can control the format of how hex values are serialized or deserialized with the new parameter hex_as_base64 for the SERIALIZE and DESERIALIZE methods. If hex_as_base64 is set to abap_true, binary/hex values are processed as base64 (default, compatible behavior). If hex_as_base64 is set to abap_false value is processed in raw hex form.
* New: Now you can provide bool types (true, false), 3 bool types (true, false, undefined), initial timestamp, initial time, and initial date (JSON value for initial ABAP value) in the instance constructor. This would allow you to not inherit class but just parametrize (better performance). 

## Note [2944398](https://launchpad.support.sap.com/#/notes/2944398 ) - PL15
### /UI2/CL_JSON
* Fixed. Generating ABAP structures for JSON attributes that include special characters like "/\:;~.,-+=><|()[]{}@+\*?!&$#%^'§\`" fails.
* Fixed. Edm.Guid with the mixed or lowercase is not recognized.
* Fixed. Iterating and data assignments for the generated data object (REF TO DATA), produced by the GENERATE and DESERIALIZE methods, fail in ABAP Cloud. 

### /UI2/CL_DATA_ACCESS
* Fixed. Index access to generated tables in LOOP construction fails.

## Note [2904870](https://launchpad.support.sap.com/#/notes/2904870 ) - PL14
### /UI2/CL_JSON
* Fixed. Unescaping of strings with a single Unicode entity (e.g., "\uXXXX") does not work 
* New. More robust logic for handling invalid JSON (e.g, cases with extra "," without further element  { "a": 1, } )

## Note [2870163](https://launchpad.support.sap.com/#/notes/2870163 ) - PL13
### /UI2/CL_JSON
* Fixed. Conversion exists and does not work when data is located inside internal tables.
* Fixed. TIMESTAMPL subsecond values are truncated when deserializing from Edm.DateTime.

## Note [2798102](https://launchpad.support.sap.com/#/notes/2798102 ) - PL12
### /UI2/CL_JSON
* New. DESERIALIZE and GENERATE methods supporting decoding of Unicode symbols (\u001F) 
* Fixed. Invalid JSON causing <STRING_OFFSET_TOO_LARGE> exception and dump.
### /UI2/CL_DATA_ACCESS
* Fixed. Access to fields with special characters in the name (e.g., "/BIC/YEAR") fails.

## Note [2786259](https://launchpad.support.sap.com/#/notes/2786259 ) - PL11
### /UI2/CL_JSON
* Optimized. Performance lost, introduced in PL10 (note 2763854) when unescaping special characters (\r\n\t\")
* Fixed. Short dump, with <STRING_OFFSET_TOO_LARGE> when running the GENERATE method with empty or invalid input

### /UI2/CL_DATA_ACCESS
* Fixed. Short dump, when accessing elements of a null array

## Note [2763854](https://launchpad.support.sap.com/#/notes/2763854) - PL10
### /UI2/CL_JSON
* Fixed: Deserialization and generation of the ABAP data from JSON strings with Unicode characters fail
* Fixed: Unescaping of \\t and \\n char combinations in strings handled incorrectly
* Fixed: GENERATE method fails on JSON attribute names containing spaces

## Note [2650040](https://launchpad.support.sap.com/#/notes/2650040)
### /UI2/CL_JSON
* New: Support for deserialization of OData Edm.Guid
* New: Support for Enum data types in ABAP. From SAP_BASIS 7.51, below, the enums are ignored.
* New: Support for conversion exits for serializing and deserializing.
* Fixed: SERIALIZE method delivers an invalid JSON string when NUMC type, filled with spaces, is used.

## Note [2629179](https://launchpad.support.sap.com/#/notes/2629179)
### /UI2/CL_JSON
* New: JSON timestamp fields, serialized in OData Edm.DateTime format (e.g. "\/Date(1467981296000)\/") is supported, and properly deserialized in ABAP date, time, or timestamp fields
* New: JSON timestamp fields, serialized in OData Edm.Time format (e.g., "PT10H34M55S") is supported, and properly deserialized in ABAP date, time, or timestamp fields
* Fixed: content is scrambled when using the GENERATE method for JSON objects with a name containing special characters (for example "__metadata")
* Fixed: GENERATE method does not consider custom name mapping pairs passed as a parameter for CONSTRUCTOR or GENERATE methods
* Fixed: generation of very long integers (serialized numeric date) fails, due to I type overflow (you get 0 instead of the expected number) 

## Note [2526405](https://launchpad.support.sap.com/#/notes/2526405)
### /UI2/CL_JSON
* Fixed: Deserialization of the inconsistent data (JSON string into ABAP table) leads to a short dump if the JSON string is empty.
* Fixed: Serialization of data with includes defined suffix (RENAME WITH SUFFIX) dumps
* Fixed: GENERATE method fails if the JSON object contains duplicate attributes and PRETTY_MODE-CAMEL_CASE is not used.
* Fixed: GENERATE method fails if JSON object contains attribute names longer than 30 characters (allowed ABAP field length). This can also occur in case the name is shorter than 30 characters, but PRETTY_MODE-CAMEL_CASE is used.
* New: methods DUMP_INT, DUMP_TYPE, RESTORE_TYPE, and RESTORE can be overridden now. So, you can introduce your data type conversion on serialization and deserialization.
* New: now it is possible to pass the name mapping table as a parameter for the constructor/serialize/deserialize and control the way JSON names are formatted/mapped to ABAP names. This may help if you need special rules for name formatting (for special characters or two long JSON attributes) and standard pretty printing modes cannot help. With this feature, you may eliminate the need for the class extension and redefine the PRETTY_NAME and PRETTY_NAME_EX methods. 
* New: The PRETTY_NAME_EX method was extended to support the encoding of more special characters (characters needed in JSON names but can not be used as part of the ABAP name). The supported characters are: "!#$%&\*-~/:|@.". Used with pretty_mode-extended.

### /UI2/CL_DATA_ACCESS
* New: /UI2/CL_DATA_ACCESS class for working with dynamic ABAP data object (generated with method /UI2/CL_JSON=>GENERATE). The class can be used as a replacement for multiple ASSIGN COMPONENT language constructions. 

## Note [2292558](https://launchpad.support.sap.com/#/notes/2292558)
* Fixed: Empty JSON objects, serialized as entries of the table, are not deserialized into corresponding ABAP structures, and further parsing of the JSON string after an empty object is skipped.
* Fixed: JSON fields containing stringified timestamp representation in ISO 8601 format are not deserialized properly in the corresponding ABAP timestamp field.

## Note [2300508](https://launchpad.support.sap.com/#/notes/2300508)
* Fixed: Recursive (hierarchical) JSON objects cannot be deserialized.

## Note [2330592](https://launchpad.support.sap.com/#/notes/2330592)
* Fixed: Partial serialization/deserialization of the JSON is not supported
* New: Extending the class is supported
* New: Added support for serializing named include structures from ABAP as embedded sub-objects in JSON

## Note [2368774](https://launchpad.support.sap.com/#/notes/2368774)
* Fixed: /UI2/CL_JSON creates unnecessary wrapping JSON object around value for name/value (table with 2 fields), tables with 1 unique key
* Fixed: Performance of serialization/deserialization of big tables into/from JSON associative arrays (maps) is slow
* Fixed: When trying to deserialize an invalid (not matching) structure from JSON to ABAP dump OBJECTS_MOVE_NOT_SUPPORTED occurs

## Note [2382783](https://launchpad.support.sap.com/#/notes/2382783)
* Fixed: Unescape of symbol '\' on JSON deserialization does not work
* Fixed: Short dump on serialization of classes with protected/private attributes
* Fixed: Short dump when serializing dynamic, non-existing types

## Note [2429758](https://launchpad.support.sap.com/#/notes/2429758)
* Fixed: Short Dump on deserialization of classes with read-only attributes
* New: The serialization parameter was added to NUMC_AS_STRING, controlling the way NUMC fields are serialized. The default is FALSE. If set to TRUE, NUMC fields are serialized not as numbers, but as strings, with all leading zeroes. Deserialization works compatibly with both ways of NUMC serialized data.
* New: GENERATE and GENERATE_INT methods are added for the on-the-fly creation of ABAP data objects from JSON, without the need to have a predefined ABAP structure. Supports automatic creation of ABAP structures, tables, and elementary types, concerning JSON types. Supports structure/table nesting.
* New: DESERIALIZE_INT method throws an exception CX_SY_MOVE_CAST_ERROR and stops further processing in case of malformed data found, and the STRICT_MODE parameter in the constructor is set to TRUE.
* New: Support for XSTRING was added as input for deserialization.

## Note [2480119](https://launchpad.support.sap.com/#/notes/2480119)
*Last note with support of SAP_BASIS 740*
* New: GENERATE method creates a local custom class for deserialization (lc_json_custom), instead of standard /ui2/cl_json
* Fixed: Internal tables are not initialized when deserializing JSON with empty arrays
* New: Deserialization into a field with REF TO data type, if the field is bound, using a referenced data type
* New: Deserialization uses automatic generation of the data if the field has a "REF TO DATA" type and the bound data is initial
