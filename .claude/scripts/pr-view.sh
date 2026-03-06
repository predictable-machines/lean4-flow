#!/usr/bin/env bash
# Usage: pr-view.sh
# Returns PR number, title, url, and state for the current branch
gh pr view --json number,title,url,state
