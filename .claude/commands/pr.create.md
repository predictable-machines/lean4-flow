# Create Pull Request

Create a pull request for the current branch.

## Step 1: Check Current State

Get the current branch and verify it's not main:

```bash
git branch --show-current
```

If on `main`, inform the user and stop.

Check if the branch has a remote tracking branch:

```bash
git status -sb
```

Check for uncommitted changes:

```bash
git status --porcelain
```

If there are uncommitted changes, ask the user if they want to commit them first.

## Step 2: Find Linked Issues

Fetch open issues assigned to the current user:

```bash
gh issue list --assignee @me --state open --json number,title,body
```

Get the repository owner and name:

```bash
gh repo view --json owner,name --jq '.owner.login + "/" + .name'
```

For each issue found, check if it has a linked branch matching the current branch using the GitHub GraphQL API:

```bash
gh api graphql -f query='{ repository(owner:"OWNER", name:"REPO") { issue(number:ISSUE_NUMBER) { linkedBranches(first:10) { nodes { ref { name } } } } } }'
```

Categorize each issue:

- **Linked**: The issue has a `linkedBranches` entry whose `ref.name` matches the current branch name exactly. These issues will be automatically included in the PR body with `Fixes #<number>`.
- **Possibly related**: The issue has no linked branch matching the current branch, but the branch name starts with the issue number (e.g., branch `199-some-slug` and issue #199). Ask the user whether they want the PR to fix this issue.
- **Unrelated**: No match. Skip silently.

After categorization, if there are "possibly related" issues, present them to the user and ask which (if any) should be linked with `Fixes #<number>`.

Collect the final list of issue numbers to include in the PR body.

## Step 3: Review Changes for PR

Get the diff between the current branch and main:

```bash
git log main..HEAD --oneline
git diff main...HEAD --stat
```

Review the commits and changes to understand what the PR should describe.

## Step 4: Push Branch if Needed

If the branch is not pushed or is behind the remote:

```bash
git push -u origin HEAD
```

## Step 5: Create the PR

Use `gh pr create` with the format from CLAUDE.md:

```bash
gh pr create --title "the pr title" --body "$(cat <<'EOF'
Fixes #<issue-number>

## Summary
<1-3 bullet points summarizing the changes>

## Test plan
- [ ] <testing steps>

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Guidelines for the PR:
- Title should be concise and describe the main change
- Summary should have 1-3 bullet points covering the key changes
- Test plan should include relevant testing steps
- If issues were identified in Step 2, include a `Fixes #<issue-number>` line for each linked issue on the first lines of the body (one per line, before the Summary heading)
- If no issues were linked, omit the `Fixes` lines entirely

## Step 6: Request Review

After creating the PR, post a comment requesting a review:

```bash
gh pr comment <pr-number> --body "@claude review this, I don't need any compliments, focus on what needs fixing"
```

## Step 7: Confirm Creation

After creating the PR, display:
- PR URL
- PR number
- Title
- Linked issues (if any)

Remind the user they can view comments with `/pr.review`.
