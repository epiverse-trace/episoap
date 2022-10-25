
<!-- README.md is generated from README.Rmd. Please edit that file -->

# soap: A Store of Outbreak Analytics Pipelines

<!-- badges: start -->
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
devtools::install_github("epiverse-trace/data_pipelines")
```

## Usage

Reports are provided a [rmarkdown
templates](https://rstudio.github.io/rstudio-extensions/rmarkdown_templates.html).
You can load them either via RStudio graphical interface

![Graphical interface to open a soap rmarkdown template in
RStudio](man/figures/template_rstudio.png)

or by running:

``` r
rmarkdown::draft(file = "myreport.Rmd", template = "transmissibility-report", package = "soap")
```
