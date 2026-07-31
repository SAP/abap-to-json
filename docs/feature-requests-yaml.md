# Z_UI2_YAML Feature Requests

This is the **own backlog for Z_UI2_YAML**, separate from the JSON class backlog
(`docs/feature-requests.md`). Do not mix YAML items into the JSON list.

---

## Known v1 Deferrals (future work)

| # | Item | Notes |
|---|------|-------|
| 1 | **pretty_name inverse on DESERIALIZE** | camelCase/PascalCase key → ABAP component name mapping not applied during deserialization; only case-insensitive uppercase match is used. |
| 2 | **Anchor emission on write** | `&anchor` / `*alias` in serialized output. Anchors are fully supported on read (Task 7). |
| 3 | **Block-header anchors on read** | `key: &a |` — anchor on a line that also carries a block-scalar indicator. Deferred from Task 7. |
| 4 | **Multi-document streams** | `---` separators produce only the first document; subsequent documents are ignored. |
| 5 | **Tags** | `!!str`, `!!int`, `!<uri>` tags are silently ignored. |
| 6 | **Explicit block-scalar indent indicator** | `|2`, `>4` — indent column from indicator digit. Auto-detection from body content is used instead. |
| 7 | **Reconsider default key case** | `pretty_mode-none` (default) preserves ABAP UPPERCASE names. Many YAML users expect lowercase. Consider making `low_case` the default in a future major version, or documenting the pattern more prominently. |

---

## Open Requests

*(none yet)*
