#!/usr/bin/env bash
# Usage: issue-linked-branches.sh <owner> <repo> <issue_number>
# Fetches linked branches for a given issue via GraphQL
set -euo pipefail
owner="$1"
repo="$2"
issue="$3"

gh api graphql -f query="
  { repository(owner:\"${owner}\", name:\"${repo}\") {
      issue(number:${issue}) {
        linkedBranches(first:10) {
          nodes { ref { name } }
        }
      }
    }
  }
"
