#!/usr/bin/env bash
# Usage: pr-conversation-comments.sh <owner> <repo> <pr_number>
# Fetches PR conversation comments (issue comments API)
set -euo pipefail
owner="$1"
repo="$2"
pr="$3"

gh api "repos/${owner}/${repo}/issues/${pr}/comments" \
  --jq '.[] | {id: .id, body: .body, user: .user.login, created_at: .created_at}'
