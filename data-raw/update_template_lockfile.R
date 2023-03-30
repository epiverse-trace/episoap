#' Update the embedded lockfile within a given template
#'
#' @param template The name of the template containing the lockfile to update
#'
#' @returns A character with an `renv::use()` call to copy/paste in place of the
#'   old one
#'
#' @examples
#' update_template_lockfile("transmissibility")
#'
#' @internal
#' @noRd
#'
update_template_lockfile <- function(template) {

  sk <- file.path("inst", "rmarkdown", "templates", template, "skeleton")

  lf_loc <- tempfile(fileext = ".lock")

  sk |>
    renv::dependencies(progress = FALSE) |>
    subset(select = "Package", drop = TRUE) |>
    setdiff(c("episoap", "renv", "rmarkdown")) |>
    renv::snapshot(
      packages = _,
      lockfile = NULL,
      repos = c(CRAN = "https://cloud.r-project.org"),
      prompt = FALSE
    ) |>
    renv:::renv_lockfile_compact() |> # nolint
    cat()

}
