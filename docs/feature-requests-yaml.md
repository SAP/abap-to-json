# Z_UI2_YAML Feature Requests

This is the **own backlog for Z_UI2_YAML**, separate from the JSON class backlog
(`docs/feature-requests.md`). Do not mix YAML items into the JSON list.

---

## Known v1 Deferrals — Status Update (2026-07-31)

Items #1, #3, #4 have been completed and shipped:
- **#1 camelCase/PascalCase inverse on DESERIALIZE** — now supported with `pretty_name = pretty_mode-camel_case` / `pascal_case`
- **#3 Block-header anchors on read** — now supported (e.g. `key: &a |`)
- **#4 Multi-document streams** — new methods `GENERATE_ALL` and `DESERIALIZE_ALL` now process all documents

Remaining items #2, #5, #6, #7 are documented in the "Declined" section below.

---

## Declined (see CLAUDE.md API Design Decisions)

The following items have been evaluated and formally declined:

| Item | Reason | Decision Source |
|------|--------|-----------------|
| **Anchor emission on write** | Config producers emit expanded YAML for human readability; anchors need cycle detection + dedup heuristics. Read-side anchors already supported. | CLAUDE.md |
| **YAML tags (`!!str`, `!<uri>`)** | ABAP target type (RTTI on DESERIALIZE, inference on GENERATE) already determines interpretation. Tags rare in config. | CLAUDE.md |
| **Explicit block-scalar indent indicator (`\|2`, `>4`)** | Auto-detection from body content covers all real cases; explicit form addresses rare edge case. | CLAUDE.md |
| **Default key case** | Keeping `pretty_mode-none` (UPPERCASE) for consistency with Z_UI2_JSON. Users should pass `pretty_name = pretty_mode-low_case` for lowercase. Caveat: consecutive-capitals names (e.g. MY_URL) don't round-trip under camelCase inverse. | CLAUDE.md |

---

## Open Requests

*(none yet)*

