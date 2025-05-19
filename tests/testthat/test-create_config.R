test_that("create_config creates file with correct structure", {
  # Create temp directory for testing
  test_dir <- tempfile() # generates temp dir path
  dir.create(test_dir) # create path
  withr::defer(unlink(test_dir, recursive = TRUE), teardown_env())

  test_path <- file.path(test_dir, "test_config.yaml")

  # Test file creation
  expect_false(file.exists(test_path))
  result_path <- create_config(path = test_path)
  expect_true(file.exists(test_path))

  # Verify return value
  expect_identical(result_path, test_path)
  expect_invisible(create_config(test_path))

  # Verify template structure
  config <- yaml::read_yaml(test_path)

  # Check top-level structure
  expect_named(config, c("data", "disease_name", "severity"))
  expect_named(config$severity,
               c( "total_cases", "total_deaths",
                 "death_in_confirmed", "account_for_delay", "interval", "epidist", "epidist_params" ))

  # Check epidist_params structure
  expect_named(config$severity$epidist_params,
               c("type", "distribution", "parameters"))
  expect_named(config$severity$epidist_params$parameters,
               c("meanlog", "sdlog", "shape", "scale"))


  # Check NA placeholders
  expect_identical(config$data, NA_character_)
  expect_identical(config$severity$total_cases, NA_real_)



})

test_that("create_config doesn't overwrite existing files", {
  test_dir <- tempfile()
  dir.create(test_dir)
  withr::defer(unlink(test_dir, recursive = TRUE), teardown_env())

  test_path <- file.path(test_dir, "existing_config.yaml")

  # Create dummy file first
  writeLines("existing content", test_path)
  original_mtime <- file.mtime(test_path)

  # Run function and check mtime remains the same
  Sys.sleep(0.1)  # Ensure mtime would change if modified
  create_config(path = test_path)
  expect_equal(file.mtime(test_path), original_mtime)

  # Verify content wasn't changed
  expect_identical(readLines(test_path), "existing content")
})

test_that("default path uses tempdir", {
  default_path <- create_config()
  expect_true(grepl(tempdir(), default_path, fixed = TRUE))
  expect_identical(basename(default_path), "config.yaml")
})

 test_that("handles paths with spaces", {
 test_dir <- file.path(tempdir(), "test dir with spaces")
 dir.create(test_dir, recursive = TRUE)
 withr::defer(unlink(test_dir, recursive = TRUE), teardown_env())

  test_path <- file.path(test_dir, "test config.yaml")

  expect_silent(create_config(test_path))
  expect_true(file.exists(test_path))
})

test_that("file opening command works on current OS", {
  test_path <- create_config()

  # This test just checks that the command runs without errors
  # Doesn't validate actual file opening
  expect_silent({
    if (Sys.info()["sysname"] == "Darwin") {
      system(paste("open", shQuote(test_path)), wait = FALSE)
    } else if (Sys.info()["sysname"] == "Windows") {
      shell.exec(test_path)
    } else {
      system(paste("xdg-open", shQuote(test_path)), wait = FALSE)
    }
  })
})
