
<!-- README.md is generated from README.Rmd. Please edit that file -->

# episoap: A Store of Outbreak Analytics Pipelines <img src="man/figures/logo.svg" align="right" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/epiverse-trace/episoap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/episoap/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/episoap/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/episoap?branch=main)
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

<figure>
<img src="man/figures/template_rstudio.png"
alt="Graphical interface to open a soap rmarkdown template in RStudio" />
<figcaption aria-hidden="true">Graphical interface to open a soap
rmarkdown template in RStudio</figcaption>
</figure>

or by running:

``` r
rmarkdown::draft(file = "myreport.Rmd", template = "transmissibility", package = "episoap")
```

To get a list of the template reports available in this package, you can
run:

``` r
episoap::list_templates()
#> [1] "transmissibility"
```

### System dependencies

You may need to install system dependencies:

    # macOS
    brew install libsodium cmake

    # Linux (Debian based)
    apt install libsodium-dev cmake

## Related projects

This project has some overlap with other R packages:

- [`{sitrep}`](https://github.com/R4EPI/sitrep) from the Applied Epi
  organisation. While the stated goals and approaches can appear
  similar, `{episoap}` and `{sitrep}` are actually two very different
  projects. The `{sitrep}` reports are more specific (providing, e.g.,
  reports for a specific disease although a generic template is in
  development), and thus more detailed. They are also more opinionated
  in the sense that they provide a single analysis path for each
  situation, based on the extensive experience of MSF. `{episoap}` on
  the other hand offers a more generic approach, with the emphasis on
  alternative paths you can take within a single analysis.

## Acknowledgements

- Thanks to Sam Abbott for pointing out issues with the way EpiNow2 was
  used in the transmissibility pipeline (#35)
- The package logo is a derivative from a [pipeline
  logo](https://www.flaticon.com/free-icon/pipeline_2082696), provided
  by [flaticon user “Eucalyp”](https://www.flaticon.com/authors/eucalyp)
  for free for personal and commercial use with attribution.
