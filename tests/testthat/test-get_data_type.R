test_that("get_data_type correctly identifies count data inputs", {
  # Basic count data
  expect_equal(get_data_type(total_count = 100, total_death = 5), "count_data")

  # Zero values allowed
  expect_equal(get_data_type(total_count = 0, total_death = 0), "count_data")

  # Partial count inputs should error
  expect_error(get_data_type(total_count = 100))
  expect_error(get_data_type(total_death = 5))
})
