CLASS z_ui2_yaml DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES yaml TYPE string.
    TYPES pretty_name_mode TYPE c LENGTH 1.
    TYPES: BEGIN OF name_mapping,
             abap TYPE abap_compname,
             yaml TYPE string,
           END OF name_mapping.
    TYPES name_mappings TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY abap.

    CONSTANTS:
      BEGIN OF pretty_mode,
        none        TYPE c LENGTH 1 VALUE ``,
        low_case    TYPE c LENGTH 1 VALUE 'L',
        camel_case  TYPE c LENGTH 1 VALUE 'X',
        pascal_case TYPE c LENGTH 1 VALUE 'P',
        extended    TYPE c LENGTH 1 VALUE 'Y',
      END OF pretty_mode.

    CONSTANTS version TYPE i VALUE 1 ##NO_TEXT.

    " methods added in Tasks 8-11

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z_ui2_yaml IMPLEMENTATION.
ENDCLASS.
