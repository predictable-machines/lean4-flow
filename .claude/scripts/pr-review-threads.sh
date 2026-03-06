#!/usr/bin/env bash
# Usage: pr-review-threads.sh <owner> <repo> <pr_number>
# Fetches review threads with resolved status via GraphQL
set -euo pipefail
owner="$1"
repo="$2"
pr="$3"

gh api graphql -f query='
  query($owner: String!, $repo: String!, $pr: Int!) {
    repository(owner: $owner, name: $repo) {
      pullRequest(number: $pr) {
        reviewThreads(first: 100) {
          nodes {
            isResolved
            path
            line
            comments(first: 20) {
              nodes { body author { login } }
            }
          }
        }
      }
    }
  }
' -f owner="$owner" -f repo="$repo" -F pr="$pr"
