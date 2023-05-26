# Version History

## Note [3038042](https://launchpad.support.sap.com/#/notes/3038042) - PL16
### /UI2/CL_JSON
* Fixed: Serializing of hash tables with empty values and the parameters assoc_arrays_opt and compress produces invalid JSON. 
* Fixed: Method serialize produces corrupt data when an initial date or time field is serialized (something like "--" or "::" ).
* Fixed: Performance for serializing and deserializing dynamic data objects (REF TO DATA) is greatly improved. 
* New: You can apply text formatting/beautifying to serialized JSON with the new parameter format_output.
* New: You can control the format of how hex values are serialized or deserialized with the new parameter hex_as_base64 for SERIALIAZE and DESERIALIZE methods. If hex_as_base64 set to abap_true, binary/hex values processed as base64 (default, compatible behaviour). If hex_as_base64 is set to abap_false value processed in raw hex form.
* New: Now you can provide bool types (true, false), 3 bool types (true, false, undefined), initial timestamp, initial time, initial date (JSON value for initial ABAP value) in the instance constructor. This would allow you to not inherit class but just parametrize (better performance). 

## Note [2944398](https://launchpad.support.sap.com/#/notes/2944398 ) - PL15
### /UI2/CL_JSON
* Fixed. Generating of ABAP structures for JSON attributes which include special characters like "/\:;~.,-+=><|()[]{}@+\*?!&$#%^'§\`" fails.
* Fixed. Edm.Guid with the mixed or lower case is not recognized.
* Fixed. Iterating and data assignments for the generated data object (REF TO DATA), produced by GENERATE and DESERIALIZE methods fail in ABAP Cloud. 

### /UI2/CL_DATA_ACCESS
* Fixed. Index access to generated tables in LOOP construction fails.

## Note [2904870](https://launchpad.support.sap.com/#/notes/2904870 ) - PL14
### /UI2/CL_JSON
* Fixed. Unescaping of strings with a single Unicode entity (e.g "\uXXXX") does not work 
* New. More robust logic for handling invalid JSON (e.g cases with extra "," without further element  { "a": 1, } )

## Note [2870163](https://launchpad.support.sap.com/#/notes/2870163 ) - PL13
### /UI2/CL_JSON
* Fixed. Conversion exists does not work when data is located inside of internal tables.
* Fixed. TIMESTAMPL subsecond values are truncated when deserializing from Edm.DateTime.

## Note [2798102](https://launchpad.support.sap.com/#/notes/2798102 ) - PL12
### /UI2/CL_JSON
* New. DESERIALIZE and GENERATE methods supporting decoding of Unicode symbols (\u001F) 
* Fixed. Invalid JSON causing <STING_OFFSET_TOO_LARGE> exception and dump.
### /UI2/CL_DATA_ACCESS
* Fixed. Access to fields with special characters in the name (e.g "/BIC/YEAR") fails.

## Note [2786259](https://launchpad.support.sap.com/#/notes/2786259 ) - PL11
### /UI2/CL_JSON
* Optimized. Performance lost, introduced in PL10 (note 2763854) when unescaping special characters (\r\n\t\")
* Fixed. Short dump, with <STRING_OFFSET_TOO_LARGE> when running GENERATE method with empty or invalid input

### /UI2/CL_DATA_ACCESS
* Fixed. Short dump, when accessing elements of null array

## Note [2763854](https://launchpad.support.sap.com/#/notes/2763854) - PL10
### /UI2/CL_JSON
* Fixed: Deserialization and generation of the ABAP data from JSON strings with Unicode characters fail
* Fixed: Unescaping of \\t and \\n char combinations in strings handled incorrectly
* Fixed: GENERATE method fails on JSON attribute names containing spaces

## Note [2650040](https://launchpad.support.sap.com/#/notes/2650040)
### /UI2/CL_JSON
* New: Support for deserialization of OData Edm.Guid
* New: Support of Enum data types in ABAP. From SAP_BASIS 7.51, below - enums are ignored.
* New: Support of conversion exits for serializing and deserializing.
* Fixed: SERIALIZE method delivers an invalid JSON string, when NUMC type, filled with spaces is used.

## Note [2629179](https://launchpad.support.sap.com/#/notes/2629179)
### /UI2/CL_JSON
* New: JSON timestamp fields, serialized in OData Edm.DateTime format (e.g. "\/Date(1467981296000)\/") are supported, and properly deserialized in ABAP date, time or timestamp fields
* New: JSON timestamp fields, serialized in OData Edm.Time format (e.g. "PT10H34M55S") are supported, and properly deserialized in ABAP date, time, or timestamp fields
* Fixed: content is scrambled, when using GENERATE method for JSON objects with a name containing special characters (for example "__metadata")
* Fixed: GENERATE method does not consider custom name mapping pairs passed as a parameter for CONSTRUCTOR or GENERATE methods
* Fixed: generation of very long integers (serialized numeric date) fails, due to I type overflow (you get 0 instead of an expected number) 

## Note [2526405](https://launchpad.support.sap.com/#/notes/2526405)
### /UI2/CL_JSON
* Fixed: Deserialization of the inconsistent data (JSON string into ABAP table) leads to a short dump if the JSON string is empty.
* Fixed: Serialization of data with includes with defined suffix (RENAME WITH SUFFIX) dumps
* Fixed: GENERATE method fails, if the JSON object contains duplicate attributes and PRETTY_MODE-CAMEL_CASE is not used.
* Fixed: GENERATE method fails, if JSON object contains attribute names longer than 30 characters (allowed ABAP field length). Can also occur in case the name is shorter than 30 characters, but PRETTY_MODE-CAMEL_CASE is used.
* New: methods DUMP_INT, DUMP_TYPE, RESTORE_TYPE, and RESTORE can be overridden now. So, you can introduce your data type conversion on serialization and deserialization.
* New: now it is possible to pass the name mapping table as a parameter for the constructor/serialize/deserialize and control the way JSON names are formatted/mapped to ABAP names. This may help if you need special rules for name formattings (for special characters or two long JSON attributes) and standard pretty printing modes cannot help. With this feature, you may eliminate the need for the class extension and redefine PRETTY_NAME and PRETTY_NAME_EX methods. 
* New: PRETTY_NAME_EX method extended to support the encoding of more special characters (characters needed in JSON names but that can not be used as part of ABAP name). The supported characters are: "!#$%&\*-~/:|@.". Used with pretty_mode-extended.

### /UI2/CL_DATA_ACCESS
* New: /UI2/CL_DATA_ACCESS class for working with dynamic ABAP data object (generated with method /UI2/CL_JSON=>GENERATE). The class can be used as a replacement for multiple ASSIGN COMPONENT language constructions. 

## Note [2292558](https://launchpad.support.sap.com/#/notes/2292558)
* Fixed: Empty JSON objects, serialized as entries of the table, are not deserialized into corresponding ABAP structures and further parsing of the JSON string after an empty object is skipped.
* Fixed: JSON fields containing stringified timestamp representation in ISO 8601 format are not deserialized properly in the corresponding ABAP timestamp field.

## Note [2300508](https://launchpad.support.sap.com/#/notes/2300508)
* Fixed: Recursive (hierarchical) JSON objects cannot be deserialized.

## Note [2330592](https://launchpad.support.sap.com/#/notes/2330592)
* Fixed: Partial serialization/deserialization of the JSON is not supported
* New: Extending of the class is supported
* New: Added support for serializing named include structures from ABAP as embedded sub-objects in JSON

## Note [2368774](https://launchpad.support.sap.com/#/notes/2368774)
* Fixed: /UI2/CL_JSON creates unnecessary wrapping JSON object around value for name/value (table with 2 fields) tables with 1 unique key
* Fixed: Performance of serialization/deserialization of big tables into/from JSON associative arrays (maps) is slow
* Fixed: When trying to deserialize invalid (not matching) structure from JSON to ABAP dump OBJECTS_MOVE_NOT_SUPPORTED occurs

## Note [2382783](https://launchpad.support.sap.com/#/notes/2382783)
* Fixed: Unescape of symbol '\' on JSON deserialization does not work
* Fixed: Short dump on serialization of classes with protected/private attributes
* Fixed: Short dump when serializing dynamic, not existing, types

## Note [2429758](https://launchpad.support.sap.com/#/notes/2429758)
* Fixed: Short Dump on deserialization of classes with read-only attributes
* New: Serialization parameter added NUMC_AS_STRING, controlling the way how NUMC fields are serialized. The default is FALSE. If set to TRUE, NUMC fields are serialized not as numbers, but as strings, with all leading zeroes. Deserialization works compatibly with both ways of NUMC serialized data.
* New: GENERATE and GENERATE_INT methods are added for on-the-fly creation of ABAP data objects from JSON, without the need to have a predefined ABAP structure. Supports automatic creation of ABAP structures, tables, and elementary types, concerning JSON types. Supports structure/table nesting.
* New: DESERIALIZE_INT method throws an exception CX_SY_MOVE_CAST_ERROR and stops further processing in case of malformed data found and STRICT_MODE parameter in constructor set to TRUE.
* New: Added support of XSTRING as input for deserialization.

## Note [2480119](https://launchpad.support.sap.com/#/notes/2480119)
* New: GENERATE method creates local custom class for deserialization (lc_json_custom), instead of standard /ui2/cl_json
* Fixed: Internal tables are not initialized when deserializing JSON with empty arrays
* New: Deserialization into a field with REF TO data type, if the field is bound, using a referenced data type
* New: Deserialization uses automatic generation of the data if the field has "REF TO DATA" type and bound data is initial
