
<!-- README.md is generated from README.Rmd. Please edit that file -->

# episoap: A Store of Outbreak Analytics Pipelines

<!-- badges: start -->

[![R-CMD-check](https://github.com/epiverse-trace/episoap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/data_pipelines/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/data_pipelines/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/data_pipelines?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package provides a store of curated outbreak analytics pipelines as
rmarkdown reports.

## Target audience

The analyses are largely automated, and should be of use to any outbreak
analyst. A basic R literacy will be required to adapt the report to
other datasets.

## Installation

You can install the development version of soap from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("epiverse-trace/episoap")
```

## Usage

Reports are provided a [rmarkdown
templates](https://rstudio.github.io/rstudio-extensions/rmarkdown_templates.html).
You can load them either via RStudio graphical interface

![Graphical interface to open a soap rmarkdown template in
RStudio](man/figures/template_rstudio.png)

or by running:

``` r
rmarkdown::draft(file = "myreport.Rmd", template = "transmissibility-report", package = "episoap")
```

To get a list of the template reports available in this package, you can
run:

``` r
episoap::list_templates()
#> [1] "transmissibility"
```

## Related projects

This projects has some overlap with other R packages:

- `{sitrep}` from the Applied Epi organisation. While the stated goals
  and approaches can appear similar, `{episoap}` and `{sitrep}` are
  actually two very different projects. The `{sitrep}` reports are more
  specific (providing, e.g., reports for a specific disease), and thus
  more detailed. They are also more opinionated in the sense that they
  provide a single analysis path for each situation, based on the
  extensive experience of MSF. `{episoap}` on the other hand offers a
  more generic approach, with the emphasis on alternative paths you can
  take within a single analysis.
