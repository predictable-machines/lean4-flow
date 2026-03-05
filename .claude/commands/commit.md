# Commit Changes

Commit staged and unstaged changes to the current branch.

## Step 1: Gather Information

Run these commands in parallel:

```bash
git status
```

```bash
git diff
```

```bash
git diff --staged
```

```bash
git log -5 --oneline
```

## Step 2: Analyze and Draft Commit Message

Based on the changes:
- Summarize the nature of the changes (new feature, enhancement, bug fix, refactoring, test, docs, etc.)
- Use the correct verb: "Add" for new features, "Update" for enhancements, "Fix" for bug fixes, "Refactor" for restructuring, "Remove" for deletions
- Draft a concise commit message (1-2 sentences) focusing on the "why" rather than the "what"
- Follow the commit message style from recent commits

Do NOT commit files that likely contain secrets (.env, credentials.json, etc.).

## Step 3: Stage and Commit

Stage specific files (prefer explicit file names over `git add -A` or `git add .`):

```bash
git add <files>
```

Create the commit using a HEREDOC for proper formatting:

```bash
git commit -m "$(cat <<'EOF'
<commit message here>

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
EOF
)"
```

## Step 4: Verify

Run git status to confirm the commit succeeded:

```bash
git status
```

Display the commit hash and message to confirm.
