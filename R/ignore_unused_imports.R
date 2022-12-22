# nolint start
# https://r-pkgs.org/dependencies-in-practice.html#how-to-not-use-a-package-in-imports
# nolint end
ignore_unused_imports <- function() {
  rmarkdown::render
}
