# Z_UI2_YAML — Design Spec

**Date:** 2026-07-30
**Status:** Design approved — ready for implementation plan
**Related:** [kb/z_ui2_yaml-evaluation.md](../../../kb/z_ui2_yaml-evaluation.md) (feasibility)

## Purpose

A self-contained ABAP YAML serializer/deserializer, `Z_UI2_YAML`, for **reading and producing configuration YAML files**. Target spec: **YAML 1.2**. API and feature design based on `Z_UI2_JSON`; JSON-only specialties dropped, YAML-only specialties added on top.

**Fully independent from `Z_UI2_JSON`:** no code or logic sharing. Only shared concern is feature tracking (own feature-requests list, not mixed with JSON) and double maintenance awareness. Self-contained single class like `Z_UI2_JSON` — locals in class includes, standard exceptions, no separate artifacts.

## Constraints & decisions

| Decision | Value |
|---|---|
| Min SAP_BASIS | **7.57+** (modern ABAP: inline decls, string templates, constructor/table expressions) |
| Parse model | **One scan of text → lightweight node tree → dual mapper.** Single tokenization pass; node tree is the parse product (needed for clean anchor/alias resolution). Not a two-pass YAML→JSON bridge. |
| GENERATE | **Always optimized** (mirrors JSON2 Decision 12). No `GEN_OPTIMIZE`/`OPTIMIZE` param. Typed tables + directly-typed struct components, no `REF TO data` wrappers. Char-check type detection, no regex. |
| Born modern | Never carry JSON legacy utilities (see API §Dropped). |
| Exceptions | Reuse standard `CX_SY_CONVERSION_ERROR` (structural) + `CX_SY_MOVE_CAST_ERROR` (strict-mode type mismatch). No dedicated exception class. |
| Perf | Establish-baseline only for v1. Config files are small; no shared hot path. Not a regression gate. |
| Post-v1 | Validate & optimize scanner/parser against real config examples. |

## v1 scope

**Read features (all in v1):**
- Block mappings & sequences (core)
- Plain / single-quoted / double-quoted scalars
- `#` comments, single `---` doc start / `...` end
- Flow style `{a: 1}` / `[x, y]` (YAML 1.2 ⊇ JSON; inline char cursor)
- Block scalars `|` and `>` with chomping (`-`/`+`/clip) and folding
- Anchors `&name` / aliases `*name` (resolve transparently on read)

**Data directions:** typed (`SERIALIZE`/`DESERIALIZE`) **and** untyped (`GENERATE`).

**Write:** block style, quote-when-necessary. **Anchors NOT emitted in v1** (would need cycle detection + dedup heuristic) — documented limitation.

**Out of v1 (documented as unsupported):** multiple documents in a stream, tags (`!!str`, `!Custom` — RTTI target provides type instead), complex/mapping keys (`?`), anchor emission on write.

## Architecture

```
READ:  yaml text ─▶ lcl_scanner ─▶ lcl_parser ─▶ node tree ─▶ mapper ─▶ ABAP data
                   (lines,indent,   (indent stack, (+anchor tbl)  ├ lcl_typed_mapper (RTTI)  → DESERIALIZE
                    comments,        flow cursor,                  └ lcl_gen_mapper (optimized) → GENERATE
                    blk-scalar hdr)  anchors)

WRITE: ABAP data + RTTI ─▶ lcl_emitter ─▶ yaml text  (block style, quote-when-needed)
```

Six focused, independently-testable units (all locals inside the class):

1. **`lcl_scanner`** — text → flat stream of logical lines: `indent` (leading spaces), comment-stripped `content`, flags `doc_marker` / `block_scalar_header`. Drops comments & blank lines *except* inside block scalars. **Tab in indentation → fatal.**
2. **`lcl_parser`** — indent stack drives nesting; first child line decides mapping (`key:`) vs sequence (`- `). Flow collections & quoted scalars scanned inline via a char cursor (never touch indent stack). Block scalars grab more-indented following lines, apply chomping/folding. `&anchor` registers node ref in anchor table after parse; `*alias` splices the referenced node. Output: node tree.
3. **Node tree** — 3 node kinds: scalar, sequence, mapping (`kind`, `value`, children). Minimal intermediate.
4. **`lcl_typed_mapper`** — walks nodes against `CL_ABAP_TYPEDESCR`, honoring name_mappings + pretty_name inverse. DESERIALIZE path.
5. **`lcl_gen_mapper`** — infers types (uniform seq → typed table; mapping → typed components), builds always-optimized `REF TO DATA`. GENERATE path.
6. **`lcl_emitter`** — RTTI walk → block YAML. Quote-when-necessary: values with `:`, `#`, leading `-`, `[`/`{`, boolean-like strings, empty (`''`). Indent by depth.

`Z_UI2_YAML` public class is a thin facade over the locals, mirroring `Z_UI2_JSON`'s public shape.

## Public API

```abap
" ── READ ──
DESERIALIZE( importing yaml type string / yamlx type xstring optional
                       pretty_name   type pretty_name_mode default none
                       name_mappings type name_mappings optional
             changing  data type data )                    " typed, RTTI target

GENERATE( importing yaml type string ...                   " untyped → always-optimized REF TO DATA
          returning value(rr_data) type ref to data )

" ── WRITE ──
SERIALIZE( importing data type data
                     name          type string optional    " root key, optional
                     compress      type bool default false " skip initial fields
                     pretty_name   type pretty_name_mode default none
                     name_mappings type name_mappings optional
           returning value(r_yaml) type string )
```

Dual static + `_INT` instance pattern; advanced switches on `CONSTRUCTOR` only (narrow static API rule, carried from JSON).

**Kept from JSON (1:1 concepts):** dual static/`_INT` API; `PRETTY_NAME` modes + `PRETTY_NAME`/`PRETTY_NAME_EX` override hooks; `NAME_MAPPINGS`; `COMPRESS`; `STRICT_MODE` (constructor) + `CX_SY_MOVE_CAST_ERROR` contract; `ASSOC_ARRAYS`; scalar formatting (dates / timestamps / numbers / booleans).

**Dropped (JSON legacy — never added):** `DUMP` (use SERIALIZE), `RAW_TO_STRING` / `STRING_TO_RAW` / `XSTRING_TO_STRING` / `STRING_TO_XSTRING`, `ESCAPE` / `UNESCAPE`, `BOOL_TO_TRIBOOL` / `TRIBOOL_TO_BOOL`, `GET_INDENT`, `GEN_OPTIMIZE` / `OPTIMIZE`, `JSONX_CP`, `FORMAT_OUTPUT` (YAML always formatted).

**Added (YAML-only, CONSTRUCTOR unless noted):**

| Param | Where | Purpose | Default |
|---|---|---|---|
| `INDENT` | CONSTRUCTOR | spaces per level | 2 |
| `EMIT_DOC_MARKERS` | CONSTRUCTOR | emit leading `---` | off |
| `QUOTE_STYLE` | CONSTRUCTOR | plain-preferred / always-double (emit) | plain |
| `FLOW_THRESHOLD` | CONSTRUCTOR | inline short collections as flow | off (always block) |
| `HEADER_COMMENT` | SERIALIZE | optional `#` banner line(s) at top | — |

## Data flow (read)

- **Scanner → Parser handoff:** scanner emits logical lines with indent + flags. Parser uses an **indent stack**:
  - Deeper indent → open child collection; first child line decides mapping vs sequence.
  - Equal indent → sibling. Shallower → pop until match; landing between levels → fatal.
  - `key:` with nothing after → value is nested block below, or null (`~`/empty) if none.
  - `- ` → sequence item; `- key: val` → item is a mapping (indent continues past dash).
  - Flow `{}`/`[]` & quoted scalars → inline char cursor, never touch indent stack.
  - Block scalars → grab following more-indented lines, apply chomping (`-` strip / `+` keep / clip default) + folding (`>`), emit one scalar node.
  - `&anchor` → store `name → node ref` after node parses; `*alias` → splice referenced node.
- **Emit:** RTTI walk, one node per component, quote-when-necessary, indent by depth.

## Error handling

- **Default (lenient):** best-effort. Unmappable scalar → target stays initial. Unknown mapping key → skipped. Duplicate key → last wins.
- **`STRICT_MODE = abap_true`** → `DESERIALIZE_INT` raises `CX_SY_MOVE_CAST_ERROR` with path (`$.server.ports[0]`). No success/failure boolean (JSON rule).
- **Structural YAML errors** (tab in indent, bad dedent, unterminated quote/flow, alias to undefined anchor, block-scalar indent violation) → **always fatal in both modes**, `CX_SY_CONVERSION_ERROR`, message carries line number. This is the one divergence from JSON: indentation makes structural errors real and they must fail loudly even when lenient.

## Testing

`z_ui2_yaml.clas.testclasses.abap`, run via `RunAbapUnit`. By unit:

- **Scanner:** indent counting, comment stripping, tab rejection, doc markers, block-scalar header detection.
- **Parser/read:** per-feature fixtures — block mapping/sequence, nested mix, `- key:` items, plain/single/double scalars, flow `{}`/`[]`, block scalars `|`/`>` all chomping modes, anchors→aliases shared content, null, interleaved comments.
- **Typed vs GENERATE:** same YAML into known structure vs GENERATE; assert optimized output (typed tables, no `REF TO data`).
- **Emit:** quote-when-necessary matrix (`:`, `#`, leading `-`, `[`, boolean-like, empty), indent widths, name_mappings + pretty_name, header comment.
- **Round-trip:** emit → read → assert-equal on representative structures (strongest config check).
- **Strict vs lenient:** structural fatal in both; type mismatch fatal only in strict; path in message.
- **Real-world fixtures:** a few actual config shapes (CI-file-ish, app-config, list-of-servers) as reality anchor.

**Performance:** `Z_UI2_YAML_PERF` in the same style as `Z_UI2_JSON_PERF` (static `run()` → typed results; testclass `fail()` with formatted numbers). **Baseline-establishment only for v1** — not a regression gate.

## Post-v1 (deferred, on demand)

- Anchor **emission** on write (cycle detection + dedup).
- Multiple documents in a stream.
- Tags (`!!str`, custom).
- Scanner/parser validation & optimization against real-world config corpus.

## Source file layout (abapGit)

- `src/z_ui2_yaml.clas.abap` — main class + local class definitions
- `src/z_ui2_yaml.clas.locals_imp.abap` — `lcl_scanner` / `lcl_parser` / mappers / `lcl_emitter` implementations
- `src/z_ui2_yaml.clas.testclasses.abap` — unit tests
- `src/z_ui2_yaml.clas.xml` — abapGit metadata (generated)
- `src/z_ui2_yaml_perf.clas.abap` (+ testclasses) — performance harness
- `docs/feature-requests-yaml.md` — **own** feature tracking, separate from JSON
