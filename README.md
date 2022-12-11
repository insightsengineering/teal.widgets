# teal.widgets

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering//actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering//actions/workflows/check.yaml)
[![Docs 📚](https://github.com/insightsengineering//actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io//)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/coverage.xml)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/insightsengineering/?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering//main?color=purple\&label=package%20version)](https://github.com/insightsengineering//tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/?color=red\&label=open%20issues)](https://github.com/insightsengineering//issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

# teal.widgets

<!-- start badges -->
[![Code Coverage](https://raw.githubusercontent.com/insightsengineering/teal.widgets/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/teal.widgets/_xml_coverage_reports/data/main/coverage.xml)
<!-- end badges -->

This package provides various 'widgets' for `teal` applications. Examples include:

- `standard_layout` for a standard UI layout with output on the right and an encoding panel on the left
- `plot_with_settings` for a standard plot output UI with resizing and downloading options
- `table_with_setting` for a standard `rtables` output UI with download and pagination options
- `ggplot2_args` for a standard `ggplot2` output plot with graphic options
- `basic_table_args` for a standard `rtables` output table with graphic options

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.widgets@*release")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

See package vignettes `browseVignettes(package = "teal.widgets")` for usage of this package.

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.widgets.svg)](https://starchart.cc/insightsengineering/teal.widgets)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.widgets](https://reporoster.com/stars/insightsengineering/teal.widgets)](https://github.com/insightsengineering/teal.widgets/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.widgets](https://reporoster.com/forks/insightsengineering/teal.widgets)](https://github.com/insightsengineering/teal.widgets/network/members)
