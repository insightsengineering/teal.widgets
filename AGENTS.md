---
output: github_document
---
# teal.widgets R Package Development Guide

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
- Validate inputs with `checkmate::assert_*` at the top of each function.
- New technical words go in `inst/WORDLIST`.
- Line length limit is 120 (`.lintr`).

This package is part of the teal framework. The following configuration applies to all packages within the teal framework:

## Package Structure and Organization

### Key Directories

Follow the standard R package structure with teal-specific conventions:

```text
package_name/
├── .gitlab-ci.yml    # CI/CD workflows
├── R/                # R source code
├── tests/testthat/   # Unit tests using testthat
├── vignettes/        # Long-form documentation
├── inst/             # Package assets
├── CLAUDE.md         # Development guide for AI agents (this file)
├── DESCRIPTION       # Package metadata
├── NAMESPACE         # Exports and imports automa
├── NEWS.md           # Change log
├── README.md         # Package overview
├── _pkgdown.yml      # Documentation website config
├── .lintr            # Linting configuration
└── .Rbuildignore     # Build exclusions
```

### Naming Conventions

- **Function names**: Use `snake_case` consistently
- **Class names**: Use `PascalCase` (e.g., `TealAppDriver`)
- **Module functions**: Prefix UI functions with `ui_` and server functions with `srv_`
- **Internal functions**: Use descriptive names without export

### File Organization

- **One main function per file** when the function is substantial
- **Group related utilities** in shared files (e.g., `utils.R`, `validations.R`)
- **Module files**: Use pattern `tm_<name>.R` for teal modules
- **Helper functions**: Prefix with the main function they support

## Code Style and Standards

### Code Quality

- **Run `pre-commit` hooks**: Always run `pre-commit run --all-files` before committing. Fix any issues it reports - the error messages are informative and will guide you. It automatically checks code style, documentation, linting, and other quality issues.
- **Follow `tidyverse` style**: General R code style follows the `tidyverse` style guide.
- **Documentation**: All exported functions must have `roxygen2` documentation. Run `devtools::document()` to update documentation.
- **Formatting** rules are configured in the `.lintr` file.

## Dependencies and Imports

### Dependency Management

- **Minimize dependencies**: Only add dependencies that provide significant value
- **Version constraints**: Specify minimum versions for critical dependencies
- **Ecosystem coherence**: Prefer packages already used within teal ecosystem

### Import Best Practices

Avoid importing package functions via roxygen2 (`#' @import pkg`)tags in favor of explicit namespacing for clarity when appropriate.
When needed prefer specific imports over full package imports.

### Code Style for Modules

- **Use `tidyverse` style**: Write clear, readable code using `dplyr`, `ggplot2` patterns
- **Use `magrittr` pipes in reproducible execution**: For code executed for `teal_data`/`qenv` data objects with `eval_code()` and `within()`
- **Use crane and gtsummary**: For statistical tables and summaries
- **Error handling**: Implement proper validation using `checkmate` and `shiny::validate(teal::need_input(...))`

## Testing Framework

### Testing Philosophy

- **Test public functions only**: Internal utilities should be tested through public interfaces
- **Precise, focused tests**: Each test should verify one specific behavior
- **High coverage**: Maintain at least 80% test coverage as measured by `covr`
- **Integration over units**: Test realistic usage patterns
- **Test Dependencies**.: Add `testthat::skip_if_not_installed(package_name)` only for dependencies in SUGGESTS or related to tests cases

### Shiny Module Testing

- **Server functions**: Test with `shiny::testServer()`
- **UI functions**: Test basic usage with regular testing (class checks, error generation, snapshots, regexp search). Test UI scenarios and interactions with `teal::TealAppDriver` (based on `shinytest2::AppDriver`) for integration testing
- **Reactive behavior**: Test reactive chains and side effects

### Test Organization and Naming

- **One test file per R file**: `test-module_example.R` for `module_example.R`
- **Descriptive test names**: Clearly describe what is being tested
- **End to end test names**: `test-shinytest2-module_example.R` for `module_example.R`
- **Logical grouping**: Group related tests using `describe()` when beneficial
- **Test data**: Create minimal test datasets, avoid external dependencies

## Documentation and Communication

### Package Documentation

- **`README.md`**: Clear overview, installation, basic usage examples
- **Vignettes**: Comprehensive guides for complex functionality
- **Function documentation**: All exported functions must have `roxygen2` documentation
- **`NEWS.md`**: Detailed changelog following semantic versioning

### Package Version Management

Do not change versions on your own.
There is a CI/CD workflow that manages the versions automatically on the `main` branch.

## CI/CD and Development Workflow

### GitHub Workflows

Use r.pkg.template workflows for consistency:

- `check.yaml`: R CMD check, unit tests, coverage
- `docs.yaml`: Documentation building and deployment
- `audit.yaml`: Security and dependency auditing
- `pkgdown.yaml`: Website generation

## Quality Assurance

### Code Quality Metrics

- **Test Coverage**: ≥80% line coverage
- **Linting**: No lint violations using configured `.lintr`
- **Documentation**: 100% of exports documented
- **Dependencies**: Minimal and justified dependencies only

### Code Review Process

- **Pull Request Reviews**: All changes require review
- **Automated Checks**: CI must pass before merging
- **Breaking Changes**: Require special consideration and communication
- **Documentation Updates**: Must accompany functional changes

### Performance Considerations

- **Shiny Reactivity**: Minimize unnecessary reactive computations
- **Data Processing**: Use efficient data manipulation patterns
- **Memory Usage**: Consider memory implications for large datasets
- **Loading Time**: Optimize package loading and module initialization

## Maintenance Guidelines

- **Long-term Support**: Maintain backward compatibility when possible
- **Deprecation**: Use `lifecycle` package for function deprecation
