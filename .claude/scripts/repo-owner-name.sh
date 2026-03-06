#!/usr/bin/env bash
# Usage: repo-owner-name.sh
# Returns "owner/name" for the current repository
gh repo view --json owner,name --jq '.owner.login + "/" + .name'
