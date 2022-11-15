test_that("theme_episoap() works", {

  th <- theme_episoap()

  expect_s3_class(th, "theme")
  expect_true(attr(th, "complete"))

})
