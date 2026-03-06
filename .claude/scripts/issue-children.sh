#!/usr/bin/env bash
# Usage: issue-children.sh <owner> <repo> <issue_number>
# Fetches sub-issues (children) of a given issue
set -euo pipefail
owner="$1"
repo="$2"
issue="$3"

gh api graphql -f query='
  query($owner: String!, $repo: String!, $issue: Int!) {
    repository(owner: $owner, name: $repo) {
      issue(number: $issue) {
        subIssues(first: 50) {
          nodes { number title url state }
        }
      }
    }
  }
' -f owner="$owner" -f repo="$repo" -F issue="$issue"
