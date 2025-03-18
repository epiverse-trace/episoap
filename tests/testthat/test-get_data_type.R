test_that("get_data_type correctly identifies count data inputs", {
  # Basic count data
  expect_equal(get_data_type(total_count = 100, total_death = 5), "count_data")

  # Zero values allowed
  expect_equal(get_data_type(total_count = 0, total_death = 0), "count_data")

  # Partial count inputs should error
  expect_error(get_data_type(total_count = 100))
  expect_error(get_data_type(total_death = 5))
})

test_that("get_data_type correctly handles incidence objects", {
  # Pure incidence object
  inc_obj <- structure(list(date = Sys.Date(), cases = 10, dead = 2),
                       class = "incidence")
  expect_equal(get_data_type(data = inc_obj), "incidence")

  # Data frame that should be recognized as incidence
  inc_df <- data.frame(
    DATE = seq.Date(Sys.Date(), length.out = 5, by = "day"),
    CASES = c(10, 15, 20, 18, 12),
    DEAD = c(1, 2, 3, 1, 2)
  )
  expect_equal(get_data_type(data = inc_df), "incidence")

  # Incidence data with extra non-linelist columns
  inc_extra <- cbind(inc_df, notes = c("a", "b", "c", "d", "e"))
  expect_equal(get_data_type(data = inc_extra), "incidence")
})

test_that("get_data_type correctly identifies linelist data", {
  # Full linelist structure
  linelist_df <- data.frame(
    case_id = 1:10,
    onset_date = Sys.Date() - 9:0,
    report_date = Sys.Date(),
    age = sample(5:80, 10),
    outcome = sample(c("fatal", "recovered"), 10, replace = TRUE),
    hospital = sample(c("A", "B", "C"), 10, replace = TRUE)
  )
  expect_equal(get_data_type(data = linelist_df), "linelist")

  # Minimal linelist with 4 identifiers
  minimal_linelist <- data.frame(
    ID = 1:5,
    ONSET = Sys.Date() - 4:0,
    GENDER = sample(c("M", "F"), 5, replace = TRUE),
    OUTCOME = rep("fatal", 5)
  )
  expect_equal(get_data_type(data = minimal_linelist), "linelist")

  # Hybrid data with incidence columns + linelist features
  hybrid_df <- data.frame(
    date = Sys.Date(),
    cases = 10,
    dead = 2,
    patient_age = 35,
    facility = "Hospital X"
  )
  expect_equal(get_data_type(data = hybrid_df), "linelist")
})
