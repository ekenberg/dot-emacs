# Feature backlog

## Org agenda + expanded capture templates

**Motivation**: org is configured (capture, journal, babel) but the agenda
is unused, `org-refile-targets` is unset, and the only capture templates
are "Todo" and "Note". With more than ~20 tasks, finding and refiling
becomes friction. Currently the user manual-navigates `~/ownCloud/org`.
**Scope**: define `org-refile-targets`, add a minimal
`org-agenda-custom-commands` (today, week, search), and add 3-4 capture
templates (link, code snippet, web clip, journal).
**Smoke test**: open `~/ownCloud/org/Capture.org`, run capture, see new
templates, run agenda (`M-x org-agenda a`), refile a task.
**Rollback**: revert the `configuration.org` change; nothing else touched.

## Workspace switcher hydra

**Motivation**: `eyebrowse` is configured with custom `M-1..M-8` bindings
and `eyebrowse-new-workspace t`, but there's no in-buffer UI for cycling,
renaming, or sending a buffer to a workspace. Each `M-1..M-8` is a blind
switch. A small hydra would match the existing `hydra-git-gutter` pattern.
**Scope**: a `defhydra` with: switch to 1-8, next/prev, rename, create,
close, "send current buffer to N".
**Smoke test**: open 2-3 buffers, switch workspaces via the hydra, verify
buffer list and window config follow.
**Rollback**: remove the file and the `use-package` block.
