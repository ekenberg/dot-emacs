# AGENTS.md — dot-emacs project

> Lean and functional. Stable line numbers, no mass reformat.

> **Audience**: AI coding assistants (pi sessions) and human collaborators.
> The config works. Any change must not regress what currently works.

## Missions

### Primary — cleanup

Reduce bloat in this Emacs config — dead code, dead commented-out blocks,
vestigial vendored files, drift-prone hand-maintained lists, brittle paths,
deprecated patterns — and selectively modernize the stack **without regressing
the user's daily workflow**. Long-running project: many small, reviewable
changes, not a single big-bang refactor.

### Secondary — features

Add small, focused features that fill real workflow gaps: in-house helpers
in `lisp/`, hydras, custom commands, new capture templates, advice on
existing packages, faces, keybindings. **Not** net-new external packages
(hard rule 4) and **not** major stack changes (those are work streams
below). Every feature must come with:

- **A concrete motivation** — "I keep reaching for X and the current way
  is awkward", not "this would be cool".
- **A clear scope** — one concern, one file (or a tightly coupled group).
- **A smoke test** — how to verify it works after the change, and what the
  rollback path is.

## Architecture (read this first)

| File / dir | Role | Edit it? |
|---|---|---|
| `init.el` | Bootstrap. Loads `configuration.org` via `org-babel-load-file`. | rarely |
| `early-init.el` | Frame geometry, font, Wayland/pgtk setup. | carefully |
| `configuration.org` | **Single source of truth** for all elisp. | **yes — the only place to edit elisp** |
| `configuration.el` | Tangled output of `configuration.org`. Gitignored. | **never — regenerated on load** |
| `custom.el` | `custom-set-variables` and `custom-set-faces`. Gitignored. | via `M-x customize`, then commit `custom.el` |
| `lisp/` | Hand-rolled and vendored elisp. One file per concern. | per-file policy (see Hotspots) |
| `themes/` | Custom themes. `material-theme` (MELPA) is the live default. | `my-dev-1-theme` only with care |
| `elpa/`, `eln-cache/`, `tree-sitter/`, `framegeometry`, `places`, `history` | Runtime state. | **never** |

Git is on `master` and currently in sync with `origin/master` at `f262dea`.
Two stale local branches (`fix/frame-geometry-wayland*`) are leftover refs from
merged work — flag in PRs, do not force-delete without confirmation.

## Hard rules

1. **No regression.** The config works. Every change must leave the daily
   workflow (code, org, magit) working. When in doubt, do nothing and ask.
2. **Edit `configuration.org` only.** `configuration.el` is regenerated on
   every load. Custom themes, the WIP `lisp/templ-ts-web-mode.el`, and the
   Wayland frame-geometry code are also off-limits without asking.
3. **One concern per branch / commit.** Don't bundle fixes with refactors.
4. **No new external packages without sign-off.** Suggest in the PR
   description, don't add.
5. **Comment out before deleting.** Test removals with `:disabled t` or
   commented blocks for at least one full session before permanent
   deletion. Log the date in `plans/STATUS.md`.

## Verification protocol

Every change must pass all three gates before commit:

1. **Load test.** The config must byte-compile and load cleanly. The
   author's local equivalent of:
   ```sh
   emacs --batch -Q -l init.el --eval '(kill-emacs 0)' 2>&1 | tee /tmp/emacs-load.log
   ```
   No `Error`, no `Warning: assignment to free variable`, no missing
   `require` warnings for things we touched.
2. **Targeted smoke test.** Whatever feature the change touched must still
   work. Examples:
   - Org change → open `~/ownCloud/org/Capture.org`, run capture, journal.
   - LSP change → open a `.go` or `.rs` file, check `eglot-ensure` connects.
   - Minibuffer change → `C-x C-f`, `C-s`, `M-x`, `counsel-git-grep`.
   - Custom `lisp/` change → trigger the affected command.
3. **Day-in-the-life test (for removals only).** A removal sits commented
   out for at least one full user session before deletion. Log it in
   `plans/STATUS.md` with the date it was disabled.

## Work streams

1. **Bug fixes + dead-code purge** — `pdf-tools` path, `ob-php` require,
   `split-width-treshold` typo, `lsp-ivy` in `package-selected-packages`.
   Dead code: `seq-25.el`, `guess-offset.el`, `org-learn.el`, `autopackage.el`,
   `sv-kalender.el`, commented-out `use-package` blocks. Each removal is
   commented for a session before deletion.
2. **`package-selected-packages` hygiene** — move the source of truth to a
   `defvar` in `configuration.org`. Needs a design call.
3. **LSP decision** — finish removing `lsp-mode` (and `lsp-ivy` from
   `package-selected-packages`) or re-enable alongside eglot. **Defer to user.**
4. **Minibuffer modernization** — vertico + marginalia + orderless + embark +
   consult, retiring ivy/counsel. **Gated on user sign-off.**
5. **Project workflow** — `project.el` (zero-dep) or projectile, optional
   `tab-bar-mode` with `eyebrowse` labels.
6. **Org overhaul** — agenda + refile + useful capture templates; wire
   `ob-php` if kept.
7. **Misc small polish** (defcustoms, `:if` clauses) — fold into the relevant stream.

## Feature pipeline

For in-house features (secondary mission):

1. **Capture** in `plans/features.md`: motivation, scope, smoke test, rollback.
2. **Branch** `feature/<short-slug>` off `master`.
3. **Build + verify**: implement (config in `configuration.org`, code in
   `lisp/<name>.el` with `;;;###autoload` + `(provide ...)`), then run the
   three-gate verification protocol **plus** exercise the new behavior.
4. **Use + document**: a real session trial, then update `plans/STATUS.md`.

If the scope grows into a major stack change mid-implementation, promote it
to a work stream and get user sign-off before continuing.

## Hotspots — read before touching

- **`themes/my-dev-1-theme.el`.** Hand-rolled, currently unused but kept
  as the dark alternative to `material-theme`. Don't delete.
- **`lisp/restore-framegeometry.el`** and `early-init.el`. Recent work
  (`f262dea`, `1c09255`, `b4404fe`, `4c11679`) — Wayland-aware frame
  geometry. Be very careful here; the user just cleaned this up.
- **`lisp/octorgopress.el`, `lisp/org-learn.el`, `lisp/ob-php.el`** —
  vendored from third parties. Keep attribution, don't reformat blindly.
- **`lisp/seq-25.el`** — vendored from GNU ELPA for Emacs 25 compat.
  Delete it (work stream 1); don't "modernize" it.

## Pi workflow conventions

This is the canonical playbook for pi subagents in this project.

- **Session start**: dispatch `scout` to refresh context. Scout output is the working map.
- **Decisions** (e.g. "should we modernize the minibuffer or not"):
  `oracle` with `context: fork`. The user is the final authority; the
  oracle's job is to surface trade-offs crisply.
- **Multi-file change plans**: `planner` → write plan to `plans/<stream>.md`
  → user approves → `worker` implements.
- **Implementation**: `worker` with `context: fork` after plan approval.
  Worker must run the verification protocol above before declaring done.
- **End of session**: append to `plans/STATUS.md`:
  - What changed
  - Branch state
  - What's queued for next session
  - Any user decisions needed

## Pointers

- `README.md` — install instructions (stale but still accurate for the
  curl-pipe-bash path; the git-symlink path it mentions is what we use).
- `TODO.md` — superseded by this file. Don't update it; the new
  work-in-progress is in `plans/STATUS.md` and per-stream plan files.
- `.pi-subagents/artifacts/` — scout / planner / worker outputs from
  past pi sessions. Useful as a session log.
- The first scout report for this project is at
  `.pi-subagents/artifacts/76c6d67f_scout_0_output.md` (full line-by-line
  map of the config as of 2026-07-07).
- `git log --oneline -20` for recent direction.
