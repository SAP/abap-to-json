*"* use this source file for your LOCAL class DEFINITION
*"* which is only visible within this one class include

"=== Shared node-tree types =============================================

TYPES:
  BEGIN OF ty_node,
    kind     TYPE c LENGTH 1,  "S"=scalar "M"=mapping "Q"=sequence
    value    TYPE string,
    is_null  TYPE abap_bool,
  END OF ty_node.

CLASS lcl_node_ref DEFINITION DEFERRED.
TYPES ty_node_ref TYPE REF TO lcl_node_ref.
TYPES:
  BEGIN OF ty_child,
    key  TYPE string,
    node TYPE ty_node_ref,
  END OF ty_child.
TYPES ty_children TYPE STANDARD TABLE OF ty_child WITH DEFAULT KEY.

"=== Shared scanner line types =========================================

TYPES:
  BEGIN OF ty_line,
    lineno        TYPE i,
    indent        TYPE i,
    content       TYPE string,
    doc_marker    TYPE abap_bool,
    blk_scalar_hd TYPE abap_bool,
  END OF ty_line.
TYPES ty_lines TYPE STANDARD TABLE OF ty_line WITH DEFAULT KEY.

"=== Node kind constants ===============================================

CLASS c_node DEFINITION FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      scalar   TYPE c LENGTH 1 VALUE 'S',
      mapping  TYPE c LENGTH 1 VALUE 'M',
      sequence TYPE c LENGTH 1 VALUE 'Q'.
ENDCLASS.

"=== Node ref carrier ==================================================

CLASS lcl_node_ref DEFINITION FINAL.
  PUBLIC SECTION.
    DATA node      TYPE ty_node.
    DATA children  TYPE ty_children.
ENDCLASS.

"=== Scanner ===========================================================

CLASS lcl_scanner DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS scan
      IMPORTING text         TYPE string
      RETURNING VALUE(lines) TYPE ty_lines
      RAISING   cx_sy_conversion_error.
  PRIVATE SECTION.
    CLASS-METHODS strip_comment
      IMPORTING body          TYPE string
      RETURNING VALUE(result) TYPE string.
    CLASS-METHODS trim_right
      IMPORTING body          TYPE string
      RETURNING VALUE(result) TYPE string.
ENDCLASS.

CLASS lcl_parser DEFINITION.
  PUBLIC SECTION.
    " filled in Task 4
ENDCLASS.

CLASS lcl_typed_mapper DEFINITION.
  PUBLIC SECTION.
ENDCLASS.

CLASS lcl_gen_mapper DEFINITION.
  PUBLIC SECTION.
ENDCLASS.

CLASS lcl_emitter DEFINITION.
  PUBLIC SECTION.
ENDCLASS.
