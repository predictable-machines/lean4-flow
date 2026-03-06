#!/usr/bin/env bash
# Usage: pr-reviews.sh <owner> <repo> <pr_number>
# Fetches review summaries, excluding dismissed reviews
set -euo pipefail
owner="$1"
repo="$2"
pr="$3"

gh api "repos/${owner}/${repo}/pulls/${pr}/reviews" \
  --jq '[.[] | select(.state == "DISMISSED" | not) | {id: .id, state: .state, body: .body, user: .user.login}]'
