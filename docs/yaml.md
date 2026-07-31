# Z_UI2_YAML — Usage Reference

`Z_UI2_YAML` is a self-contained YAML 1.2 config reader/writer for ABAP.
Requires SAP_BASIS 7.57+. No external dependencies.

---

## API Overview

Three static entry points mirror the JSON class pattern:

| Method | Direction |
|---|---|
| `Z_UI2_YAML=>SERIALIZE( data )` | ABAP → YAML string |
| `Z_UI2_YAML=>DESERIALIZE( yaml = … CHANGING data = … )` | YAML string → ABAP |
| `Z_UI2_YAML=>GENERATE( yaml )` | YAML string → `REF TO data` (schema-free) |

Instance methods `SERIALIZE_INT` / `DESERIALIZE_INT` are available when you create an instance with custom constructor options and want to reuse it.

---

## SERIALIZE

```abap
TYPES: BEGIN OF ty_server,
         host    TYPE string,
         port    TYPE i,
         enabled TYPE abap_bool,
       END OF ty_server.
DATA servers TYPE STANDARD TABLE OF ty_server WITH DEFAULT KEY.
servers = VALUE #( ( host = `db1` port = 5432 enabled = abap_true )
                   ( host = `db2` port = 5432 enabled = abap_false ) ).

" Default: ABAP component names emitted as UPPERCASE
DATA(yaml_upper) = z_ui2_yaml=>serialize( servers ).
" Result:
" - HOST: db1
"   PORT: 5432
"   ENABLED: true

" Use pretty_name = pretty_mode-low_case for lowercase keys
DATA(yaml_lower) = z_ui2_yaml=>serialize(
  data        = servers
  pretty_name = z_ui2_yaml=>pretty_mode-low_case ).
" Result:
" - host: db1
"   port: 5432
"   enabled: true
```

> **Key case note:** The default `pretty_mode-none` preserves ABAP's UPPERCASE component names.
> Use `pretty_name = pretty_mode-low_case` (or `camel_case` / `pascal_case`) to produce
> lowercase keys compatible with standard YAML configs.

### Static parameters

| Parameter | Default | Notes |
|---|---|---|
| `data` | — | Any ABAP data (structure, table, elementary) |
| `pretty_name` | `pretty_mode-none` (UPPERCASE) | Key case: `low_case`, `camel_case`, `pascal_case` |
| `name_mappings` | — | Explicit ABAP→YAML key overrides |
| `compress` | `abap_false` | Skip initial/empty fields |
| `header_comment` | — | Prepended as `# …` comment lines |

### Constructor-only options (use instance API for these)

```abap
DATA(o) = NEW z_ui2_yaml(
  indent           = 4           " indentation spaces (default 2)
  emit_doc_markers = abap_true   " prepend --- / append ...
  quote_style      = 'D'         " always double-quote strings ('P' = smart, default)
  flow_threshold   = 3           " emit sequences with ≤3 items as flow [a, b, c]
).
DATA(yaml) = o->serialize_int( my_data ).
```

### Bool fields

`TYPE abap_bool` fields round-trip as `true` / `false`:

```abap
TYPES: BEGIN OF ty, enabled TYPE abap_bool, END OF ty.
DATA(in) = VALUE ty( enabled = abap_true ).
DATA(yaml) = z_ui2_yaml=>serialize( data = in pretty_name = z_ui2_yaml=>pretty_mode-low_case ).
" → "enabled: true"
```

---

## DESERIALIZE

```abap
TYPES: BEGIN OF ty_cfg,
         name     TYPE string,
         enabled  TYPE abap_bool,
         replicas TYPE i,
       END OF ty_cfg.
DATA cfg TYPE ty_cfg.

z_ui2_yaml=>deserialize(
  EXPORTING yaml = |name: web\nenabled: true\nreplicas: 3|
  CHANGING  data = cfg ).
" cfg-name = 'web', cfg-enabled = 'X', cfg-replicas = 3
```

Key matching is **case-insensitive**: YAML key `host` matches ABAP component `HOST`.

### Nested structures and sequences

```abap
TYPES: BEGIN OF ty_s, host TYPE string, port TYPE i, END OF ty_s.
TYPES: BEGIN OF ty_w,
         servers TYPE STANDARD TABLE OF ty_s WITH DEFAULT KEY,
       END OF ty_w.
DATA w TYPE ty_w.

" Indented-style sequences (items 2 spaces deeper than key):
z_ui2_yaml=>deserialize(
  EXPORTING yaml = |servers:\n  - host: a\n    port: 1\n  - host: b\n    port: 2|
  CHANGING  data = w ).

" Flush-style sequences (items at same indent as key):
z_ui2_yaml=>deserialize(
  EXPORTING yaml = |servers:\n- host: a\n  port: 1\n- host: b\n    port: 2|
  CHANGING  data = w ).

" Both forms produce: lines( w-servers ) = 2,  w-servers[ 2 ]-host = 'b'
```

### Strict mode (instance API)

```abap
DATA(o) = NEW z_ui2_yaml( strict_mode = abap_true ).
TRY.
    o->deserialize_int( EXPORTING yaml = |port: notanumber| CHANGING data = out ).
  CATCH cx_sy_move_cast_error.
    " field-level type mismatch raised here
ENDTRY.
```

The static `DESERIALIZE` is always lenient — type errors leave the field initial.

### name_mappings

```abap
DATA nm TYPE z_ui2_yaml=>name_mappings.
INSERT VALUE #( abap = `MY_FIELD` yaml = `myField` ) INTO TABLE nm.
z_ui2_yaml=>deserialize(
  EXPORTING yaml          = |myField: hello|
            name_mappings = nm
  CHANGING  data          = my_struct ).
```

---

## GENERATE (schema-free)

Returns a `REF TO data` — type is inferred from YAML values (`true`/`false` → `abap_bool`, integers → `i`, decimals → `decfloat34`, dates `YYYY-MM-DD` → `d`, everything else → `string`).

Like `DESERIALIZE`, `GENERATE` raises `CX_SY_CONVERSION_ERROR` on structurally invalid YAML (e.g. tabs in indentation, bad nesting). Callers should handle it if the input is untrusted.

```abap
DATA(r) = z_ui2_yaml=>generate( |host: db1\nport: 5432| ).
FIELD-SYMBOLS <s> TYPE any.
ASSIGN r->* TO <s>.
FIELD-SYMBOLS <f> TYPE any.
ASSIGN COMPONENT `HOST` OF STRUCTURE <s> TO <f>.
WRITE <f>.  " db1
```

Component names are always **UPPERCASE** (sanitized: non-alphanumeric → `_`, max 30 chars).

---

## Limitations (v1)

The following features are **not supported** in v1:

- **YAML key case on DESERIALIZE** — `pretty_name` inverse (camelCase→field) is not applied during deserialization; key matching is always case-insensitive uppercase comparison.
- **Anchor emission** — writing `&anchor` / `*alias` references in serialized output is not supported. Anchors are fully supported on **read**.
- **Multi-document streams** — `---` document separators are silently skipped; only the first document is processed.
- **Tags** — `!!str`, `!!int`, `!<uri>` tags are ignored during parse.
- **Explicit block-scalar indent indicator** — `|2`, `>4` etc. are not supported; the indent column is auto-detected from body content.
- **Block-header anchors** — `key: &a |` (anchor on a block scalar header line) is deferred.
- **Default key case** — `pretty_mode-none` (the default) emits ABAP's UPPERCASE component names. Use `pretty_mode-low_case` to produce standard lowercase YAML.
