# AGENTS.md

Agent guidance for `teal.widgets`. `CLAUDE.md` points here.

## Overview

`teal.widgets` is a **foundational UI-component library** in the `teal` ecosystem:
a flat set of reusable `shiny` widgets — plot/table output with resize + download,
layout helpers, and custom inputs — that `teal` modules consume. It does **not**
build apps, define `teal` modules, or touch the `teal_data`/`qenv` reproducibility
layer.

Widgets come in two forms, both exported. **Full modules** pair `<name>_ui(id)` with
`<name>_srv(id, ...)` (e.g. `plot_with_settings`, `table_with_settings`,
`verbatim_popup`) — note the `_ui`/`_srv` **suffix**, unlike the `ui_`/`srv_` prefix
used by `teal` modules. **Plain helpers** are single functions (`standard_layout`,
`optionalSelectInput`, `draggable_buckets`, `panel_group`, `white_small_well`,
`get_dt_rows`). Each widget is independent — there is no central app or router.

## Gotchas

- **Tests skip silently by depth.** `skip_if_too_deep(depth)`
  (`tests/testthat/helpers-testing-depth.R`) skips a test when `TESTING_DEPTH`
  (option/env, default `3`) is below its `depth`. A green `devtools::test()` at the
  default has **not** run the heavy `shinytest2` tests — set `TESTING_DEPTH=5` to run
  everything. UI tests are split out: `test-<widget>_ui.R` holds the depth-gated
  `shinytest2` UI tests (helper in `tests/testthat/helpers-shinytest2.R`), while
  `test-<widget>.R` holds the server/unit tests.
- **Base/grid plots are first-class.** `plot_with_settings` supports
  `plot_type = "grob"` and base plots, so do not assume `ggplot2`. `plot_r` may be a
  `reactive` **or** a plain `function`; a plain function is needed to capture base
  plots for download but is less efficient, so prefer `reactive` otherwise.
- **PDF download of `gt` / `gtsummary` / `tbl_split` tables needs `webshot2`**
  (a `Suggests`). When absent the module emits a once-per-session warning
  (`.warning_gt_webshot2()` in `R/utils.R`); silence it with
  `DISABLE_GT_WEBSHOT2_WARNING=true`.
- **Supporting a new table type means adding S3 methods, not branching.**
  `export_table`, `render_table_to_html`, and `file_download_format`
  (`R/table_with_settings.R`) each have one method per class —
  `TableTree`/`ElementaryTable`, `gt_tbl`, `gtsummary`, `tbl_split`, `default`.
  Add a method to each generic; never `if/else` on class.
- **`*_args` objects flow constructor → `resolve_*` → `parse_*`.** `ggplot2_args()` /
  `basic_table_args()` capture options, `resolve_*` merges layered defaults (including
  the global `getOption("teal.ggplot2_args")` / `getOption("teal.basic_table_args")`),
  `parse_*` emits the applied args/code. Editing one stage means checking the other two
  (`R/ggplot2_args.R`, `R/basic_table_args.R`).
- **Front-end lives in `inst/<widget>/`** as paired `.css`/`.js`, wired via
  `htmltools::htmlDependency()` (e.g. `plot_with_settings_deps()`). Add JS/CSS as a
  file there and register it in the dependency function — not inline `tags$script`.

## Commands

```r
devtools::load_all()                             # interactive load
devtools::document()                             # regenerate man/ + NAMESPACE (roxygen2 8.0.0)
devtools::test()                                 # all tests
devtools::test(filter = "plot_with_settings")    # subset by name
lintr::lint_package()                            # lint (config in .lintr)
styler::style_pkg()                              # tidyverse style
```

```bash
pre-commit run --all-files    # style + roxygenize + spell-check; run before committing
```

## Conventions

- Never hand-edit `man/*.Rd` or `NAMESPACE` — they are generated; edit roxygen and run `devtools::document()`.
- Do not bump the package version — releases are handled by ecosystem tooling.
- Validate inputs with `checkmate::assert_*` at the top of each function.
- New technical words go in `inst/WORDLIST`; user-facing changes go in `NEWS.md`.
- Line length limit is 120 (`.lintr`).
