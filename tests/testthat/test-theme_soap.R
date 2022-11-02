test_that("theme_soap() works", {

  th <- theme_soap()

  expect_s3_class(th, "theme")
  expect_true(attr(th, "complete"))

})
