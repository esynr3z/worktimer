# AGENTS

This repository is a small Guile Scheme CLI for time tracking.
Primary entrypoint: `worktimer.scm` (installed or symlinked as `timer`).

## Key Paths
- `worktimer.scm` - main Guile Scheme program and CLI commands
- `timer-auto-stop.sh` - helper script to stop/start on screen power
- `bash-completion/timer` - bash completion
- `zsh-completion/_timer` - zsh completion
- `fish-completion/timer-complete.fish` - fish completion
- `Makefile` - developer tooling (pre-commit, commit message checks)

## Runtime Files
- `~/.timesheet` - primary data store (auto-backed up as `~/.timesheet.bak`)
- `~/.timesheet.bak` - backup created on write when writable

### Common Commands
- `make help` - list Makefile targets
- `make bootstrap` - install pre-commit hooks
- `make pre-commit` - run pre-commit hooks on all files
- `make check-commit` - validate commit message with commitizen
- `make test` - run all unit-tests

### Running the App
- `./worktimer.scm` - run directly (shebang uses Guile)
- `guile worktimer.scm` - explicit Guile invocation

## Code Style Guidelines

### Language and Runtime
- Language: Scheme (GNU Guile)
- Use R6RS imports via `(import ...)` and Guile modules via `(use-modules ...)`
- Date/time handling uses SRFI-19; list utilities use SRFI-1

### Formatting
- Indentation: 2 spaces, align with existing file
- Keep parentheses on the same line as the opening form
- Prefer `let` / `let*` / `let-values` over deep nested `define`s
- Use blank lines to separate logical sections and command groups
- Section headers use `;;;` comments for visibility
- Avoid tabs in Scheme files; tabs only appear in Makefile

### Naming
- Use kebab-case for functions and variables: `path-split`, `cmd-start-task`
- Predicates end with `?`: `date?`, `same-day?`, `path-prefix?`
- Mutating helpers end with `!`: `tree-add-duration!`, `add-deadlines-to-report!`
- Command handlers start with `cmd-` and accept `(sheet deadlines archives . args)`
- Constants or globals are still lowercase: `ts-file`, `date-format`

### Data Shapes
- Timesheet record: `(list path start-date end-date duration)`
- Path is a list of string components, e.g. `(list "proj" "task")`
- Deadlines: `(list path date-or-time)`
- Report tree nodes: `(list name duration deadline . children)`

### Timesheet File Format
- File contains sections delimited by headers like `--- TIMESHEET`
- Timesheet lines: `proj/task: [YYYY-mm-dd HH:MM:SS] - [YYYY-mm-dd HH:MM:SS] - HH:MM:SS`
- Deadline lines: `proj/task: YYYY-mm-dd` or `proj/task: HH:MM:SS`
- Archive lines: `proj/task/subtask`

### Imports and Modules
- Keep imports at top of `worktimer.scm`
- Add new SRFIs to the existing `(import ...)` block
- Add Guile-specific modules to `(use-modules ...)`
- Prefer SRFI helpers over custom utilities when already in use

### Error Handling
- Use `throw` for hard errors with explicit keys and messages
- Use `catch` to handle parse errors or invalid dates
- Print user-facing errors via `(current-error-port)`
- Return `#f` or empty list to signal no-op cases where appropriate
- Avoid terminating the process on recoverable user input errors

### I/O and Side Effects
- Read files with `call-with-input-file`
- Write files with `with-output-to-file`
- Preserve the `.timesheet` backup behavior (`.bak` copy)
- Output uses `format` to standard output; errors to error port
- Keep user-facing output stable because completions and scripts parse it

### Sorting and Filtering
- Use `sort` with stable comparison helpers (`path<?`, `date<?`)
- Keep filtering logic in helper functions like `filter-sheet`
- Use `remove-dup` to normalize and de-duplicate lists
- When filtering by date, keep the SRFI-19 date math helpers

### CLI Command Conventions
- Use `cmd-` functions that return `(values sheet deadlines archives)`
- Keep help text in the `else` branch in `main`
- Stick to the existing command vocabulary: `start`, `stop`, `report`, etc.
- Report and timesheet commands must honor archive filtering
- Keep `tasklist`, `deadlist`, `archlist` outputs space-separated for completions

## Tips for Changes
- Favor minimal, localized edits to `worktimer.scm`
- When adding a new command, update:
  - the `main` command dispatch
  - help text in the usage output
  - completion scripts (bash/zsh/fish) if user-facing
- Keep the README in sync if user-visible behavior changes

## Performance and Safety Notes
- Prefer simple list operations; the data set is usually small
- Avoid introducing new background loops or timers in the main CLI
- Be careful with destructive file operations; keep `.timesheet` backups intact
