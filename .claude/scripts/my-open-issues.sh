#!/usr/bin/env bash
# Usage: my-open-issues.sh
# Fetches open issues assigned to the current user
gh issue list --assignee @me --state open --json number,title,body
