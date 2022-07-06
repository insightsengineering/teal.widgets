# teal.widgets

This package provides various 'widgets' for `teal` applications. Examples include:

- `standard_layout` for a standard UI layout with output on the right and an encoding panel on the left
- `plot_with_settings` for a standard plot output UI with resizing and downloading options
- `table_with_setting` for a standard `rtables` output UI with download and pagination options
- `ggplot2_args` for a standard `ggplot2` output plot with graphic options
- `basic_table_args` for a standard `rtables` output table with graphic options

## Installation

It is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.widgets@*release")
```

A stable release of all `NEST` packages is also available [here](https://github.com/insightsengineering/depository#readme).

See package vignettes `browseVignettes(package = "teal.widgets")` for usage of this package.
