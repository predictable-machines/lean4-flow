# Review PR Comments

Check and address comments and suggestions on the pull request for the current branch.

## Step 1: Get Current Branch and PR

Get the current branch name:

```bash
git branch --show-current
```

Find the open PR for this branch:

```bash
.claude/scripts/pr-view.sh
```

If no PR exists, inform the user and stop.

## Step 2: Fetch Unresolved PR Comments and Reviews

### GitHub API Reference

**`gh pr view` available JSON fields** (use `gh pr view --json` with no args to see the full list):
- PR metadata: `number`, `title`, `url`, `state`, `body`, `author`, `baseRefName`, `headRefName`
- Review data: `reviews`, `latestReviews`, `reviewDecision`, `reviewRequests`
- Other: `comments`, `commits`, `files`, `labels`, `assignees`, `milestone`
- NOTE: `reviewThreads` is **NOT a valid field** — use `reviews` instead

**`gh api` jq gotchas:**
- `!=` gets shell-escaped to `\!=` and breaks jq. Use `select(.field == "VALUE" | not)` instead
- Conversation comments (issues API): user field is `.user.login`
- Review comments (pulls API): user field is `.user.login`
- `gh pr view --json reviews`: author field is `.author.login`

### Fetch Commands

All fetch commands use scripts from `.claude/scripts/` (pre-approved in `.claude/settings.json`).
Pass `{owner}`, `{repo}`, and `{pr_number}` as positional arguments.

Get review threads via GraphQL (the only way to get resolved/unresolved thread status):

```bash
.claude/scripts/pr-review-threads.sh {owner} {repo} {pr_number}
```

Get PR conversation comments:

```bash
.claude/scripts/pr-conversation-comments.sh {owner} {repo} {pr_number}
```

Get review summaries (exclude dismissed):

```bash
.claude/scripts/pr-reviews.sh {owner} {repo} {pr_number}
```

Get inline review comments:

```bash
.claude/scripts/pr-inline-comments.sh {owner} {repo} {pr_number}
```

## Step 3: Present Comments to User

List all unresolved comments in a clear format. Skip any resolved threads.

### Filtering Already-Handled Suggestions

Before presenting comments, analyze the reply chain for each comment thread:
- If a reply indicates the suggestion was implemented (e.g., "done", "fixed", "addressed", "solved", or a bot summary showing it was completed), mark that suggestion as **already handled** and exclude it from the actionable list
- If a reply explicitly rejects or declines the suggestion with reasoning, mark it as **declined** and exclude it
- Only present suggestions that have no resolution indicated in the replies

### Splitting Multi-Suggestion Comments

When a single comment contains multiple distinct suggestions or action items:
- Parse the comment to identify each separate suggestion (look for numbered lists, bullet points, section headers, or distinct paragraphs proposing different changes)
- Present each suggestion as a separate actionable item with a reference back to the original comment
- Format: "Comment #X - Suggestion A: [description]", "Comment #X - Suggestion B: [description]"

### Review Comments (Code Suggestions)

For each review comment, show:
- File and line number
- Author
- Comment body
- Any suggested code changes (look for ```suggestion blocks)
- **Status**: Unresolved / Already Handled / Declined (with brief reason from replies)

### Conversation Comments

For each conversation comment, show:
- Author
- Comment body
- If it contains a review with multiple issues, split into individual actionable items

### Review Summaries

For each review with a body, show:
- Author
- State (APPROVED, CHANGES_REQUESTED, COMMENTED)
- Review body (split into individual suggestions if multiple are present)

## Step 4: Ask User Which Issue to Address

Use AskUserQuestion to ask which **single issue** the user wants to address.

Present only **unresolved, actionable** suggestions (exclude already-handled and declined items).

**IMPORTANT:** At this stage, only LIST the issues. Do NOT present any suggestions or approaches for how to fix them yet.

Options should include:
- Individual actionable issues (one per option, even if from the same original comment)
- "None - just viewing"

## Step 5: Present Approaches for Selected Issue

**Only after the user selects a specific issue**, analyze it and propose approaches:

### 5.1: Analyze the Selected Issue

1. Read the relevant file(s)
2. Understand the suggestion or concern
3. Identify 2-4 distinct approaches to address the suggestion, considering:
   - Minimal change: The simplest fix that addresses the concern
   - Suggested approach: What the reviewer explicitly requested (if specific)
   - Alternative approaches: Other reasonable ways to solve the underlying issue
   - Scope variations: Different levels of abstraction or refactoring

### 5.2: Present Options to User

Use AskUserQuestion to let the user choose how to proceed:

- Present each approach as an option with a brief description of what it involves and its trade-offs
- Always include a "Custom approach" option that lets the user describe their preferred solution
- Always include a "Skip this issue" option
- If the suggestion is straightforward with only one reasonable approach, still offer at least:
  - The obvious approach
  - "Skip this issue"
  - "Custom approach"

Example format for options:
- "Minimal fix: [brief description]"
- "Reviewer's suggestion: [brief description]"
- "Alternative: [brief description]"
- "Skip this issue"
- "Custom approach" (user provides their own direction)

### 5.3: Implement Chosen Approach

1. Make the appropriate changes following CLAUDE.md guidelines:
   - Don't over-engineer
   - Keep changes focused on what was requested
   - Avoid adding unnecessary features or refactoring
2. Run `make build` to verify changes compile
3. If build passes, stage and commit the changes using separate tool calls for `git add`, `git commit`, and `git push`
4. Reply to the comment indicating what was done. Start replies with "Claude here:" to make it clear the response is from Claude, not the user.
5. Mark the comment as resolved if possible

### 5.4: Loop for Additional Issues

After implementing the chosen approach (or skipping), ask if the user wants to address another issue. If yes, return to Step 4 with the updated list of remaining issues.

## Step 6: Summary

After addressing comments (or when user chooses "None - just viewing"), provide a summary:

| Comment | Status | Action Taken |
|---------|--------|--------------|
| File:line - description | Addressed/Skipped | Brief description of change |

All commits are pushed immediately after creation, so no manual push is needed.

## Step 7: Merge Check

After the summary, check if the PR is ready to merge:

1. Check review status, build status, and unresolved threads:

```bash
gh pr view --json reviewDecision,reviews,state
.claude/scripts/pr-review-threads.sh {owner} {repo} {pr_number}
```

   Check the PR status page for build status (Actions tab), or if available:

```bash
.claude/scripts/pr-build-logs.sh {owner} {repo} {pr_number}
```

2. If the PR has an approval (`reviewDecision` is `APPROVED`), all review threads are resolved, and the build is passing, ask the user if they want to merge. If the build is failing or pending, inform the user and do not suggest merging.

3. If the user confirms, perform the merge:

```bash
gh pr merge <pr-number> --squash --delete-branch
```

4. (Optional) If the repo has project board integration configured, move any linked issues to "Done":

   a. Find linked issues from the PR body (`Fixes #<number>` lines)

   b. For each linked issue, find its project item ID (if available):

   ```bash
   .claude/scripts/project-item-id.sh <board-number> <owner> {issue_number}
   ```

   c. Update the item status to "Done":

   ```bash
   gh project item-edit --project-id <project-id> --id {item_id} --field-id <field-id> --single-select-option-id <done-option-id>
   ```
