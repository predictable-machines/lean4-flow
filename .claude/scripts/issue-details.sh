#!/usr/bin/env bash
# Usage: issue-details.sh <owner> <repo> <issue_number>
# Fetches full details of an issue
set -euo pipefail
owner="$1"
repo="$2"
issue="$3"

gh api graphql -f query='
  query($owner: String!, $repo: String!, $issue: Int!) {
    repository(owner: $owner, name: $repo) {
      issue(number: $issue) {
        number
        title
        url
        state
        body
        author { login }
        assignees(first: 10) { nodes { login } }
        labels(first: 10) { nodes { name } }
        milestone { title }
        createdAt
        updatedAt
      }
    }
  }
' -f owner="$owner" -f repo="$repo" -F issue="$issue"
