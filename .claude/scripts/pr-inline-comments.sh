#!/usr/bin/env bash
# Usage: pr-inline-comments.sh <owner> <repo> <pr_number>
# Fetches inline review comments on a PR
set -euo pipefail
owner="$1"
repo="$2"
pr="$3"

gh api "repos/${owner}/${repo}/pulls/${pr}/comments" \
  --jq '[.[] | {id: .id, path: .path, line: .line, body: .body, user: .user.login, in_reply_to_id: .in_reply_to_id}]'
