# Project status

## Session log

### 2026-07-07 — Session 0: setup

- Scout recon at `.pi-subagents/artifacts/76c6d67f_scout_0_output.md`
- `AGENTS.md`, `plans/STATUS.md`, `plans/features.md` drafted
- `master` clean, in sync with `origin/master` at `f262dea`
- Two stale local branches (`fix/frame-geometry-wayland*`) at `1c09255`
  and `f262dea` — leftover from merged work. Flag in PRs, do not
  force-delete.
- Next: work stream 1 (bug fixes) after AGENTS.md is committed

### 2026-07-08 — Session 1: bug fixes

- Simplification loop on the 3 bootstrap docs: oracle `623d3602`,
  workers `e6548558` + `62556233`, reviewers `a21aa121` +
  `4a3c5877`. Closed in 2 rounds (target ~197 lines, landed 201).
- Setup commit `97dc7ca`: added `AGENTS.md` + `plans/`, ignored
  `.pi-subagents/`, **deleted the two stale local branches**.
- Committed `ff4a505` — bug #2: fix `split-width-treshold` typo.
- Committed `9dda767` — bug #1: `(require 'ob-php nil t)` so PHP
  babel blocks execute. Vendored `lisp/ob-php.el` (Tristan Huang,
  2014 gist), already on `load-path`.
- Bug #1 VERIFIED end-to-end: user confirmed `C-c C-c` on a PHP
  babel block executes (PHP on Fedora). Closed.
- Research done: `org-contrib` (NonGNU ELPA) ships `ob-php` but is
  legacy ("no guarantee of compatibility"); `Fuco1/ob-php` is the
  maintained fork, not packaged. **Adopting org-contrib is DEFERRED**
  to a separate planned decision (needs NonGNU ELPA archive + compat
  check) — not bundled into the require fix.
- Bug #3 (`lsp-ivy` in `package-selected-packages`) **DEFERRED** —
  part of the LSP-vs-eglot call; needs online research + plan.
- Next: dead-code purge (2nd half of work stream 1) — seq-25.el,
  guess-offset.el, org-learn.el, autopackage.el, sv-kalender.el,
  commented-out use-package blocks. lsp-ivy stays deferred to
  LSP-vs-eglot plan.
- `master` is 3 commits ahead of `origin/master` (setup + 2 fixes).

## Feature backlog

See `plans/features.md`.
