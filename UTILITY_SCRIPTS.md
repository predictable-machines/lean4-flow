# Utility Scripts

Pre-approved read-only scripts in `.claude/scripts/` for querying GitHub without permission prompts. Permissions are defined in `.claude/settings.json`.

## PR Scripts

| Script | Args | Description |
|--------|------|-------------|
| `pr-view.sh` | — | PR metadata (number, title, url, state) for current branch |
| `pr-review-threads.sh` | owner repo pr | Review threads with resolved/unresolved status |
| `pr-conversation-comments.sh` | owner repo pr | PR conversation comments |
| `pr-reviews.sh` | owner repo pr | Review summaries (excludes dismissed) |
| `pr-inline-comments.sh` | owner repo pr | Inline review comments |

## Issue Scripts

| Script | Args | Description |
|--------|------|-------------|
| `issue-details.sh` | owner repo issue | Full issue details (title, body, state, assignees, labels) |
| `issue-parent.sh` | owner repo issue | Parent issue |
| `issue-children.sh` | owner repo issue | Sub-issues (children) |
| `issue-related.sh` | owner repo issue | Cross-references and closing PRs |
| `issue-linked-branches.sh` | owner repo issue | Branches linked to an issue |

## Repo Scripts

| Script | Args | Description |
|--------|------|-------------|
| `my-open-issues.sh` | — | Open issues assigned to current user |
| `repo-owner-name.sh` | — | Returns `owner/name` for current repo |
