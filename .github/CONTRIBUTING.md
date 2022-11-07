# Contributing to soap

This outlines how to propose a change to soap. 
For more detailed info about contributing to this, and other epiverse-trace packages, please see the
[**development contributing guide**](https://github.com/epiverse-trace/.github/blob/main/CONTRIBUTING.md). 

## Updating or creating Rmarkdown templates

The main value of the soap package is the Rmarkdown templates it provides. This section details how to proceed if you wish to update one of the existing templates or to add a new one.

When adding new code, whether in the form of a new template, or by updating an existing template, please try to hardcode as few settings and parameters as possible. We want the reports to be usable out-of-the-box in as many contexts as possible.

## Template location

[As documented in the relevant RStudio documentation page](https://rstudio.github.io/rstudio-extensions/rmarkdown_templates.html), Rmarkdown templates are located in a specific folder: [`inst/rmarkdown/templates/`](inst/rmarkdown/templates/).

### Adding a new template

If you have a self-contained generic pipeline that can be used in many contexts, for any kind of epidemic and pathogen, you could contribute a new template. Follow the instructions in [the relevant RStudio documentation page](https://rstudio.github.io/rstudio-extensions/rmarkdown_templates.html) to do so.

Some specific conventions about the templates provided in this package:

- all packages used in the report should be cited at the end of the document
- data should be read from a folder named `data/`
- all templates should include a diagram indicating the workflow provided by this package ([example for the transmissibility report](inst/rmarkdown/templates/transmissibility-report/skeleton/transmissibility_pipeline.svg))
- alternative branches in the pipeline should be provided as child Rmarkdown documents contained in an `rmdchunks` folder. The child documents can then be called in the main document with the following chunk:

````r
```{r, child=paste0("rmdchunks/", params$alternative_branch, ".Rmd")}
```
````

### Editing an existing template

In the same way that we welcome new templates, you can fix, improve or refine any existing template by editing files in the relevant folder under [`inst/rmarkdown/templates/`](inst/rmarkdown/templates/).

## Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("epiverse-trace/data_pipelines", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 

*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

## Code style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

## Code of Conduct

Please note that the soap project is released with a
[Contributor Code of Conduct](https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
