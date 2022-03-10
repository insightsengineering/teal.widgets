# teal.widgets

This package provides various 'widgets' for `teal` applications. Examples include:

- `standard_layout` for a standard UI layout with output on the right and an encoding panel on the left
- `plot_with_settings` for a standard plot output UI with resizing and downloading options
- `table_with_setting` for a standard `rtables` output UI with download and pagination options

## Installation

This repository requires a personal access token to install see here [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token). Once this is set up, to install the latest released version of the package run:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("insightsengineering/teal.widgets@*release", dependencies = FALSE)
```

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.

See package vignettes `browseVignettes(package = "teal.widgets")` for usage of this package.
