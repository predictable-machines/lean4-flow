#!/usr/bin/env bash
# Usage: issue-related.sh <owner> <repo> <issue_number>
# Fetches cross-referenced issues and PRs that close this issue
set -euo pipefail
owner="$1"
repo="$2"
issue="$3"

gh api graphql -f query='
  query($owner: String!, $repo: String!, $issue: Int!) {
    repository(owner: $owner, name: $repo) {
      issue(number: $issue) {
        closedByPullRequestsReferences(first: 10) {
          nodes { number title url }
        }
        timelineItems(itemTypes: [CROSS_REFERENCED_EVENT], first: 50) {
          nodes {
            ... on CrossReferencedEvent {
              source {
                ... on Issue { number title url }
                ... on PullRequest { number title url }
              }
            }
          }
        }
      }
    }
  }
' -f owner="$owner" -f repo="$repo" -F issue="$issue"
