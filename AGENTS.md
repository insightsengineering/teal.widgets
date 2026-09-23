# teal.widgets Development Guide

Agent guidance for `teal.widgets`. `CLAUDE.md` points here.

## Package Overview

`teal.widgets` is a **foundational UI-component library** in the `teal` ecosystem:
a flat set of reusable `shiny` widgets — plot/table output with resize + download,
layout helpers, and custom inputs — that `teal` modules consume. It sits at the
bottom of the dependency stack: `teal`, `teal.modules.*`, and analysis-module
packages depend on it, but it depends on none of them.

It deliberately does **not** build apps, define `teal` modules, or touch the
`teal_data`/`qenv` reproducibility layer. Each widget is independent — there is no
central app, router, or shared state.

## Development Context

Widgets come in two forms, both exported:

- **Full modules** pair `<name>_ui(id)` with `<name>_srv(id, ...)` (e.g.
  `plot_with_settings`, `table_with_settings`, `verbatim_popup`). Note the
  `_ui`/`_srv` **suffix**, unlike the `ui_`/`srv_` **prefix** used by `teal`
  modules.
- **Plain helpers** are single functions (`standard_layout`, `optionalSelectInput`,
  `draggable_buckets`, `panel_group`, `white_small_well`, `get_dt_rows`).

Key code-level context to keep in mind before changing anything:

- **Base and grid plots are first-class.** `plot_with_settings` detects the plot
  class itself — `ggplot`, `trellis` (lattice), `grob`, and base plots are all
  supported — so do not assume `ggplot2`. Its `plot_r` argument may be a `reactive`
  **or** a plain `function`; a plain `function` is needed to capture base plots for
  download but is less efficient, so prefer `reactive` otherwise.
- **`*_args` objects flow constructor → `resolve_*` → `parse_*`.** `ggplot2_args()` /
  `basic_table_args()` capture options, `resolve_*` merges layered defaults
  (including the global `getOption("teal.ggplot2_args")` /
  `getOption("teal.basic_table_args")`), and `parse_*` emits the applied args/code.
  Editing one stage means checking the other two (`R/ggplot2_args.R`,
  `R/basic_table_args.R`).
- **Front-end lives in `inst/<widget>/`** as paired `.css`/`.js`, wired via
  `htmltools::htmlDependency()` (e.g. `plot_with_settings_deps()`). Add JS/CSS as a
  file there and register it in the dependency function — not as inline
  `tags$script`.
- **Adding a new table type.** Follow the S3 pattern: add a method to each of `export_table`,
  `render_table_to_html`, and `file_download_format` in `R/table_with_settings.R`
  for the new class. Do not add class checks or `if/else` branches — the existing
  methods (`TableTree`/`ElementaryTable`, `gt_tbl`, `gtsummary`, `tbl_split`,
  `default`) are the template.

### Supporting packages

- **`shiny`, `htmltools`, `bslib`** — the module/UI foundation; `bslib` for layout
  and theming.
- **`shinyWidgets`, `shinyjs`** — richer inputs and client-side behavior for the
  custom widgets.
- **`rtables`, `gt`, `gtsummary`** — the table classes `table_with_settings`
  renders and exports (see the S3 dispatch note above).
- **`ggplot2`, `grid`/`grDevices`/`graphics`** — plot rendering across ggplot,
  grob, and base/grid plots.
- **`rvest`, `xml2`** — HTML parsing/manipulation for table rendering and export.
- **`checkmate`** — input validation (`assert_*`) at the top of every function.
- **`styler`** — used at runtime to format emitted reproducibility code.
- **`webshot2`** (a `Suggests`) — required for PDF download of
  `gt` / `gtsummary` / `tbl_split` tables. When absent the module emits a
  once-per-session warning (`.warning_gt_webshot2()` in `R/utils.R`); silence it
  with `DISABLE_GT_WEBSHOT2_WARNING=true`.

### Workflows

Common commands:

```r
devtools::load_all()                             # interactive load
devtools::document()                             # regenerate man/ + NAMESPACE (roxygen2 8.1.0)
devtools::test()                                 # all tests
devtools::test(filter = "plot_with_settings")    # subset by name
lintr::lint_package()                            # lint (config in .lintr)
styler::style_pkg()                              # tidyverse style
```

```bash
pre-commit run --all-files    # style + roxygenize + spell-check; run before committing
```

- **Run the full test suite before trusting green.** `skip_if_too_deep(depth)`
  (`tests/testthat/helpers-testing-depth.R`) skips a test when `TESTING_DEPTH`
  (option/env, default `3`) is below its `depth`. A green `devtools::test()` at the
  default has **not** run the heavy `shinytest2` tests — set `TESTING_DEPTH=5` to
  run everything. UI tests are split out: `test-<widget>_ui.R` holds the
  depth-gated `shinytest2` UI tests (helper in
  `tests/testthat/helpers-shinytest2.R`), while `test-<widget>.R` holds the
  server/unit tests.
- **Consider the framework before fixing a bug.** `teal.widgets` is foundational,
  so a bug that surfaces here may originate downstream (a `teal` module calling a
  widget incorrectly) or upstream (`rtables`/`gt`/`ggplot2` behavior). Confirm the
  fault is in this package before changing its code.
- **Add a regression test when fixing an issue.** Put server/unit coverage in
  `test-<widget>.R` and `shinytest2` UI coverage in `test-<widget>_ui.R`, gating
  heavy tests with `skip_if_too_deep()`.

### Conventions

- Never hand-edit `man/*.Rd` or `NAMESPACE` — they are generated; edit roxygen and
  run `devtools::document()`.
- Do not bump the package version — releases are handled by ecosystem tooling.
- Validate inputs with `checkmate::assert_*` at the top of each function.
- New technical words go in `inst/WORDLIST`; user-facing changes go in `NEWS.md`.
- Line length limit is 120 (`.lintr`).
