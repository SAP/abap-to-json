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
    blk_value     TYPE string,
  END OF ty_line.
TYPES ty_lines TYPE STANDARD TABLE OF ty_line WITH DEFAULT KEY.

"=== Anchor table entry ================================================

TYPES:
  BEGIN OF ty_anchor_entry,
    name TYPE string,
    node TYPE ty_node_ref,
  END OF ty_anchor_entry.

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

"=== Node-tree factory =================================================

CLASS lcl_tree DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS new_scalar
      IMPORTING value   TYPE string
                is_null TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(node) TYPE ty_node_ref.
    CLASS-METHODS new_collection
      IMPORTING kind        TYPE c
      RETURNING VALUE(node) TYPE ty_node_ref.
    CLASS-METHODS add_child
      IMPORTING node  TYPE ty_node_ref
                key   TYPE string OPTIONAL
                child TYPE ty_node_ref.
ENDCLASS.

"=== Scanner ===========================================================

CLASS lcl_scanner DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS scan
      IMPORTING text         TYPE string
      RETURNING VALUE(lines) TYPE ty_lines
      RAISING   cx_sy_conversion_error.
    CLASS-METHODS trim
      IMPORTING val           TYPE string
      RETURNING VALUE(result) TYPE string.
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
    CLASS-METHODS parse
      IMPORTING lines       TYPE ty_lines
      RETURNING VALUE(root) TYPE ty_node_ref
      RAISING   cx_sy_conversion_error.
    CLASS-METHODS resolve_scalar
      IMPORTING raw     TYPE string
      EXPORTING value   TYPE string
                is_null TYPE abap_bool
      RAISING   cx_sy_conversion_error.
    CLASS-METHODS parse_flow
      IMPORTING raw          TYPE string
      RETURNING VALUE(node)  TYPE ty_node_ref
      RAISING   cx_sy_conversion_error.
  PRIVATE SECTION.
    " ponytail: class-data anchor table — single-threaded/one-run state, cleared at parse() entry
    CLASS-DATA mt_anchors TYPE HASHED TABLE OF ty_anchor_entry WITH UNIQUE KEY name.
    CLASS-METHODS strip_anchor
      CHANGING  raw          TYPE string
      RETURNING VALUE(aname) TYPE string.
    CLASS-METHODS resolve_alias
      IMPORTING raw          TYPE string
                lineno        TYPE i DEFAULT 0
      RETURNING VALUE(node)  TYPE ty_node_ref
      RAISING   cx_sy_conversion_error.
    CLASS-METHODS parse_block
      IMPORTING lines        TYPE ty_lines
      CHANGING  idx          TYPE i
      RETURNING VALUE(node)  TYPE ty_node_ref
      RAISING   cx_sy_conversion_error.
    CLASS-METHODS parse_mapping
      IMPORTING lines        TYPE ty_lines
                own_indent   TYPE i
      CHANGING  idx          TYPE i
      RETURNING VALUE(node)  TYPE ty_node_ref
      RAISING   cx_sy_conversion_error.
    CLASS-METHODS parse_sequence
      IMPORTING lines        TYPE ty_lines
                own_indent   TYPE i
      CHANGING  idx          TYPE i
      RETURNING VALUE(node)  TYPE ty_node_ref
      RAISING   cx_sy_conversion_error.
    CLASS-METHODS parse_seq_item
      IMPORTING lines        TYPE ty_lines
                own_indent   TYPE i
                rest         TYPE string
      CHANGING  idx          TYPE i
      RETURNING VALUE(node)  TYPE ty_node_ref
      RAISING   cx_sy_conversion_error.
    CLASS-METHODS split_key_value
      IMPORTING content      TYPE string
                lineno       TYPE i
      EXPORTING key          TYPE string
                inline_value TYPE string
                has_inline   TYPE abap_bool
                is_mapping   TYPE abap_bool
      RAISING   cx_sy_conversion_error.
    CLASS-METHODS value_or_block
      IMPORTING lines        TYPE ty_lines
                own_indent   TYPE i
                has_inline   TYPE abap_bool
                inline_value TYPE string
      CHANGING  idx          TYPE i
      RETURNING VALUE(node)  TYPE ty_node_ref
      RAISING   cx_sy_conversion_error.
ENDCLASS.

CLASS lcl_typed_mapper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS map
      IMPORTING node          TYPE ty_node_ref
                pretty_name   TYPE z_ui2_yaml=>pretty_name_mode
                name_mappings TYPE z_ui2_yaml=>name_mappings
                strict        TYPE abap_bool
      CHANGING  data          TYPE data
      RAISING   cx_sy_move_cast_error.
ENDCLASS.

CLASS lcl_gen_mapper DEFINITION.
  PUBLIC SECTION.
    "! Build a typed REF TO data tree from a parsed YAML node (always-optimized: no REF TO data
    "! wrappers around leaf values).  Scalars are typed by character-check detection (no regex).
    CLASS-METHODS generate
      IMPORTING node          TYPE ty_node_ref
      RETURNING VALUE(rr_data) TYPE REF TO data
      RAISING   cx_sy_conversion_error.
  PRIVATE SECTION.
    "! Detect scalar type by char checks and return a typed data ref.
    "! Integer:   1-9 digit string (optional leading '-') → TYPE i.
    "!            ponytail: 10+ digit integers fall back to TYPE string.
    "! Decimal:   digits + single '.' → TYPE decfloat34.
    "! Boolean:   exact tokens 'true'/'false' → TYPE abap_bool ('X'/'').
    "! Date:      YYYY-MM-DD (10 chars, '-' at pos 4+7, digits elsewhere) → TYPE d.
    "! Else:      TYPE string.
    CLASS-METHODS detect_scalar_type
      IMPORTING value          TYPE string
      RETURNING VALUE(rr_data) TYPE REF TO data.
    "! Uppercase raw key, replace invalid ABAP component chars with '_',
    "! prefix with 'F' if first char is a digit.  Max 30 chars.
    CLASS-METHODS sanitize_name
      IMPORTING raw            TYPE string
      RETURNING VALUE(result)  TYPE abap_compname.
ENDCLASS.

CLASS lcl_emitter DEFINITION.
  PUBLIC SECTION.
ENDCLASS.
