# This unexported function adds a custom checklist item to
# `usethis::use_release_issue()`
release_bullets <- function() {

  c(
    "Run `goodpractice::gp()`",
    "Update lockfiles embedded in templates",
    "Bump episoap version number is lockfiles",
    "Test templates with renv::use(isolate = TRUE)"
  )

}
