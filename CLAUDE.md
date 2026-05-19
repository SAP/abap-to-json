# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an ABAP JSON serializer/deserializer — a local (Z*) copy of SAP's `/UI2/CL_JSON` class. It converts ABAP data structures to/from JSON and is deployed to SAP systems via abapGit. Minimum requirement: SAP_BASIS 7.31.

## No Build System

There is no Makefile, npm, or CI/CD pipeline. This is a pure ABAP project. Development, testing, and deployment happen inside an SAP system using abapGit (Eclipse plugin or SAPGUI). Changes are pushed/pulled between GitHub and an SAP system via abapGit.

## Running Tests

Unit tests live in `src/z_ui2_json.clas.testclasses.abap` (62+ test methods), `src/z_ui2_json2.clas.testclasses.abap` (57 test methods), and `src/z_ui2_data_access.clas.testclasses.abap`. Tests are executed inside the SAP system via SE80/ADT (ABAP Development Tools), not via a CLI command.

## Architecture

### Main Class: `Z_UI2_JSON` (`src/z_ui2_json.clas.abap`)

**Dual API pattern:**
- **Static methods** (`SERIALIZE`, `DESERIALIZE`, `DUMP`, `GENERATE`) — simple one-liner usage, auto-instantiate internally
- **Instance methods** (`SERIALIZE_INT`, `DESERIALIZE_INT`, `GENERATE_INT`) — use when calling repeatedly for performance, share the cached state

**Key design elements:**
- `mt_struct_cache` — caches RTTI structure metadata to avoid repeated reflection overhead
- `mt_name_mappings_ex` — cached expanded name mapping table for O(1) lookups
- Recursive processing with cycle detection (tracks seen references to prevent infinite loops)
- Macros in `z_ui2_json.clas.macros.abap` handle low-level escaping, type checks, and base64 encoding

**Pretty-printing modes** (controlled by `pretty_mode` constants):
- `none`, `low_case`, `camel_case` (MY_FIELD → myField), `pascal_case`, `extended` (camelCase + special chars)
- `user` / `user_low_case` — **internal-only**, used as signals to call `PRETTY_NAME` / `PRETTY_NAME_EX` overrides; do not document or expose these in user-facing docs

**Extensibility via inheritance:** Override these methods in a subclass:
- `PRETTY_NAME` / `PRETTY_NAME_EX` — custom name formatting
- `IS_COMPRESSIBLE` — custom rules for skipping fields
- `DUMP_TYPE` — custom value serialization
- `RESTORE` — custom deserialization logic

### Kernel API Edition: `Z_UI2_JSON2` (`src/z_ui2_json2.clas.abap`)

Drop-in replacement using SAP kernel APIs (`IF_JSON_READER` / `IF_JSON_WRITER`). Requires SAP_BASIS 7.57+. Same public API as `Z_UI2_JSON` with minor incompatibilities (see `docs/z_ui2_json2.md`). Source files:
- `src/z_ui2_json2.clas.abap` — main class
- `src/z_ui2_json2.clas.macros.abap` — serialization macros (`dump_type_int`, `dump_type`, `restore_dref`, `restore_convexit`, `read_timestamp`)
- `src/z_ui2_json2.clas.locals_imp.abap` — local helpers (`lcl_util`: RTTI helpers, ISO8601/EDM parsing, JSON tree-walking workaround)
- `src/z_ui2_json2.clas.testclasses.abap` — unit tests

### Helper Class: `Z_UI2_DATA_ACCESS` (`src/z_ui2_data_access.clas.abap`)

Dynamic runtime accessor for hierarchical ABAP data navigation. Supports path syntax like `STRUCTURE-FIELD` and `TABLE[0]`. Used to navigate/modify deeply nested data without compile-time type knowledge.

### Local Helpers (`src/z_ui2_json.clas.locals_imp.abap`)

`lcl_util` contains string escaping, MD5 hashing, ISO8601/OData datetime parsing, and RTTI type description utilities. These are private implementation details of the main class.

## Source File Layout

All ABAP source is in `src/` in abapGit format (`.clas.abap`, `.clas.macros.abap`, `.clas.locals_imp.abap`, `.clas.testclasses.abap`, `.clas.xml`). The `.xml` files are abapGit metadata — do not edit manually.

## Documentation

Detailed usage docs are in `docs/`:
- [`docs/basic.md`](docs/basic.md) — SERIALIZE/DESERIALIZE/GENERATE API reference and common examples
- [`docs/advanced.md`](docs/advanced.md) — CONSTRUCTOR, custom name mapping, partial serialization, GENERATE, exception handling
- [`docs/class-extension.md`](docs/class-extension.md) — how to inherit and override DUMP_TYPE, PRETTY_NAME, IS_COMPRESSIBLE, etc.
- [`docs/data-access.md`](docs/data-access.md) — Z_UI2_DATA_ACCESS dynamic accessor API and examples
- [`docs/faq.md`](docs/faq.md) — performance, boolean handling, timestamps, known limitations
- [`docs/z_ui2_json2.md`](docs/z_ui2_json2.md) — kernel API edition: migration guide, incompatible changes, performance results
- [`docs/migration-decisions.md`](docs/migration-decisions.md) — architectural decisions log for the Z_UI2_JSON2 migration
- [`docs/history.md`](docs/history.md) — patch level history and VERSION constant

The current patch level is tracked via the `VERSION` constant in `Z_UI2_JSON` and documented in `docs/history.md`.

## Contribution Guidelines

See `CONTRIBUTING.md`. There is also `CONTRIBUTING_USING_GENAI.md` with specific rules for AI-generated contributions — review this before submitting AI-assisted changes.
