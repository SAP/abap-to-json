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

### Pretty-name mapping on DESERIALIZE

As of v1.1, `pretty_name` inverse mapping (camelCase/PascalCase → ABAP field names) is now supported during deserialization:

```abap
TYPES: BEGIN OF ty,
         first_name TYPE string,
         last_name  TYPE string,
       END OF ty.
DATA person TYPE ty.

" YAML with camelCase keys:
z_ui2_yaml=>deserialize(
  EXPORTING yaml          = |firstName: John\nlastName: Doe|
            pretty_name   = z_ui2_yaml=>pretty_mode-camel_case
  CHANGING  data          = person ).
" person-first_name = 'John', person-last_name = 'Doe'
```

**Caveat:** ABAP names with consecutive interior capitals (e.g. `MY_URL`) do not round-trip: MY_URL → myURL → inverse MY_U_R_L (no match). This is an inherent limitation of inverting a lossy forward transform. For such fields, use explicit `name_mappings` instead.

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

## Multi-Document Streams (v1.1)

For YAML inputs with multiple `---` document separators, use `GENERATE_ALL` and `DESERIALIZE_ALL`:

### GENERATE_ALL (schema-free, all documents)

Returns a typed table of `REF TO data`, one entry per document:

```abap
DATA yaml_multi TYPE string.
yaml_multi = |---\nhost: server1\nport: 8080\n---\nhost: server2\nport: 9090|.

DATA(docs) = z_ui2_yaml=>generate_all( yaml_multi ).
" docs is a table with 2 rows; each row->* is an inferred structure
LOOP AT docs INTO DATA(ref_doc).
  FIELD-SYMBOLS <s> TYPE any.
  ASSIGN ref_doc->* TO <s>.
  FIELD-SYMBOLS <host> TYPE any.
  ASSIGN COMPONENT `HOST` OF STRUCTURE <s> TO <host>.
  WRITE <host>.  " server1, then server2
ENDLOOP.
```

### DESERIALIZE_ALL (typed table, all documents)

Fills a caller-provided typed internal table, one row per document (all docs assumed same shape):

```abap
TYPES: BEGIN OF ty_srv, host TYPE string, port TYPE i, END OF ty_srv.
DATA servers TYPE STANDARD TABLE OF ty_srv WITH DEFAULT KEY.

z_ui2_yaml=>deserialize_all(
  EXPORTING yaml = |---\nhost: db1\nport: 5432\n---\nhost: db2\nport: 5433|
  CHANGING  data = servers ).
" servers has 2 rows: (db1, 5432), (db2, 5433)
```

**Note:** Existing `DESERIALIZE` and `GENERATE` (without `_ALL` suffix) remain single-document-only for backward compatibility. Use the `_ALL` variants for multi-document processing.

---

## Limitations & Design Notes

### Producing Lowercase Config Keys

**By default**, `SERIALIZE` emits ABAP component names in UPPERCASE (via `pretty_mode-none`):
```abap
" Default output:
DATA(yaml) = z_ui2_yaml=>serialize( servers ).
" Result: HOST, PORT, ENABLED (all caps)
```

To produce conventional **lowercase** config keys, always pass `pretty_name`:
```abap
" Lowercase output:
DATA(yaml) = z_ui2_yaml=>serialize(
  data        = servers
  pretty_name = z_ui2_yaml=>pretty_mode-low_case ).
" Result: host, port, enabled (lowercase)
```

This default choice preserves consistency with `Z_UI2_JSON` and avoids breaking existing callers. The lowercase pattern is the recommended standard for new YAML configs.

### Unsupported v1 Features

The following features are **declined** and will not be implemented:

- **Anchor emission on write** — `&anchor` / `*alias` in serialized output. Anchors are fully supported on **read** (including block-header anchors). Reopen on concrete demand.
- **YAML tags** — `!!str`, `!!int`, `!<uri>` tags are silently ignored on parse. The ABAP target type (via RTTI on DESERIALIZE, type inference on GENERATE) determines interpretation.
- **Explicit block-scalar indent indicator** — `|2`, `>4` etc. are not supported; the indent column is auto-detected from body content.
- **Consecutive-caps name round-trip** — ABAP names with interior caps (e.g. `MY_URL`) do not round-trip under camelCase/PascalCase inverse: MY_URL → myURL → inverse MY_U_R_L (no match). Use explicit `name_mappings` for such fields.
- **GENERATE of a heterogeneous sequence of structurally-different objects** — a uniform list of objects (all elements the same shape) generates a typed table correctly. A list mixing objects of *different* shapes falls back to a string table and silently drops the deep (struct/table) elements rather than dumping. Uniform lists (the common config case) and mixed *scalar* sequences are fully supported.

### Supported v1 Features

- **Anchors & aliases on read** — `&anchor` and `*alias` are fully processed, including block-header anchors (`key: &a |`).
- **Multi-document streams** — Use `GENERATE_ALL` and `DESERIALIZE_ALL` for documents with `---` separators.
- **Pretty-name inverse on DESERIALIZE** — camelCase/PascalCase keys now map to ABAP field names (with the consecutive-caps caveat above).

### Performance Baseline

Captured via `Z_UI2_YAML_PERF=>run()` on ER1 (SAP_BASIS 7.57+). Small scenarios: 1000 iterations on a 3-element config. Large scenarios: wall-clock single op (or 2-3 iterations) on synthetic flat sequences of `{host, port, enabled}` rows.

#### Small config (1000 rows, 1000× iterations)

| Scenario | µs / op | notes |
|----------|---------|-------|
| SERIALIZE small config | 172 | |
| DESERIALIZE small config | 511 | |
| GENERATE small config | 542 | optimized (-17% vs pre-opt 652 via struct-type cache, Opt D) |

Pre-optimization numbers (2026-07-31): SERIALIZE 181 µs, DESERIALIZE 510 µs, GENERATE 652 µs.

#### Large scenarios — Deserialize (flat 3-field rows, typed target table)

| Scenario | Before Opt D | After Opt D | delta |
|----------|-------------|-------------|-------|
| Deserialize 10k rows | 1,197k µs | 1,195k µs | ~0% (unaffected — opt D is gen_mapper only) |
| Deserialize 100k rows | 11,984k µs | 11,974k µs | ~0% |

DESERIALIZE is dominated by: scanner (SPLIT + line processing ~300k lines) + parser (~400k `lcl_node_ref` heap allocations + tree building) + typed mapper (trivial component assignment). Mapper-side caches (A: component-name hash) show no gain at any scale because the per-row component scan over 3 fields is O(3) — identical cost to a hash lookup. The floor is parser/scanner, not mapper.

#### Large scenarios — Generate (flat 3-field rows, schema-free)

| Scenario | Before Opt D | After Opt D | delta |
|----------|-------------|-------------|-------|
| Generate 10k rows | 1,852k µs | 1,440k µs | **-22%** |
| Generate 100k rows | 18,539k µs | 14,271k µs | **-23%** |

GENERATE per-row cost dropped from ~185 µs to ~143 µs by caching the struct type descriptor (`cl_abap_structdescr=>create` was called 100k times for identical 3-field shapes; now called once, result cached by component-name+type fingerprint). Remaining cost per row: ~40 µs `cl_abap_tabledescr=>create` (called once at sequence level, outside the row loop) + ~100 µs per-row work (300k `detect_scalar_type` calls, 300k `describe_by_data_ref` calls, 100k `CREATE DATA` for row instances, 100k `INSERT INTO TABLE`).

**Optimizations tested but not kept:**
- **A (struct component-match cache in mapper):** No gain at any scale. 3-field row means O(3) linear scan = O(1) hash overhead — a wash.
- **B (workarea reuse, CREATE DATA hoisted out of loop):** -1.7% on deserialize-100k — noise. ABAP's `INSERT <wa> INTO TABLE` copies by value regardless; the CREATE DATA itself is not the bottleneck.
- **C (inline `detect_scalar_type` to avoid method calls):** -0.8% on generate-100k — the work inside the method (string scans) dominates; call overhead (~1-3 µs × 300k ≈ 0.3-0.9s) was measured and confirmed immaterial.
- **E (hoist `describe_by_data` out of loops):** Not applicable — already called once per recursive invocation, not per-child.
- **F (buffer `&&` string concatenation):** No hot-path `&&` loop identified beyond the 3-field fingerprint build in Opt D (already included).
- **G (node-tree allocation):** 400k `NEW lcl_node_ref()` per 100k-row deserialize confirmed as a structural floor (~30% of deserialize cost). Not addressable without a non-tree-based parser design. Reported as a ceiling finding.

To refresh: run ABAP Unit on `Z_UI2_YAML_PERF` (the `baseline` method intentionally fails with the numbers in its message) or call `Z_UI2_YAML_PERF=>run()` directly.
