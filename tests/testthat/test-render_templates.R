test_that("all templates render (at least with default params)", {
  skip_on_cran()
  skip_on_ci()

  for (tp in list_templates()) {
    expect_no_error(
      suppressMessages(suppressWarnings(
        rmarkdown::render(
          system.file("rmarkdown", "templates", tp, "skeleton", "skeleton.Rmd",
            package = "episoap"
          ),
          output_file = tempfile(),
          quiet = TRUE
        )
      ))
    )
  }
})
