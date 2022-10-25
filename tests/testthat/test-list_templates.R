test_that("list_templates() works", {

  templates <- list_templates()

  expect_type(templates, "character")
  expect_true(endsWith(templates, "-report"))

})
