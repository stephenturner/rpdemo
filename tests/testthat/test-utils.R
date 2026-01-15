test_that("suppress_counts works with default threshold", {
  # Correct usage with default threshold (5)
  expect_silent(res <- suppress_counts(c(1, 3, 5, 7, 9)))
  expect_equal(res, c(NA, NA, 5, 7, 9))

  # All values below threshold
  expect_silent(res <- suppress_counts(c(1, 2, 3, 4)))
  expect_true(all(is.na(res)))

  # All values above threshold
  expect_silent(res <- suppress_counts(c(10, 20, 30)))
  expect_equal(res, c(10, 20, 30))
})

test_that("suppress_counts works with custom threshold", {
  expect_silent(res <- suppress_counts(c(10, 20, 3, 4, 5), threshold = 10))
  expect_true(all(is.na(res[3:5])))
  expect_equal(res[1:2], c(10, 20))
})

test_that("suppress_counts works with custom symbol", {
  expect_silent(res <- suppress_counts(1:10, threshold = 5, symbol = "*"))
  # When using a character symbol, the vector becomes character type
  expect_true(all(res[1:4] == "*"))
  expect_equal(res[5:10], as.character(5:10))
})

test_that("suppress_counts handles edge cases", {
  # Empty vector
  expect_silent(res <- suppress_counts(numeric(0)))
  expect_equal(length(res), 0)

  # Single value
  expect_silent(res <- suppress_counts(3))
  expect_true(is.na(res))

  # Value exactly at threshold
  expect_silent(res <- suppress_counts(5, threshold = 5))
  expect_equal(res, 5)
})

test_that("suppress_counts errors on non-numeric input", {
  expect_error(suppress_counts("not numeric"), "Input x must be numeric")
  expect_error(suppress_counts(c("a", "b", "c")), "Input x must be numeric")
  expect_error(suppress_counts(factor(1:5)), "Input x must be numeric")
})

test_that("standardize_sex works with male variants", {
  expect_equal(standardize_sex("Male"), "M")
  expect_equal(standardize_sex("male"), "M")
  expect_equal(standardize_sex("M"), "M")
  expect_equal(standardize_sex("m"), "M")
  expect_equal(standardize_sex(" Male "), "M") # with whitespace
})

test_that("standardize_sex works with female variants", {
  expect_equal(standardize_sex("Female"), "F")
  expect_equal(standardize_sex("female"), "F")
  expect_equal(standardize_sex("F"), "F")
  expect_equal(standardize_sex("f"), "F")
  expect_equal(standardize_sex(" Female "), "F") # with whitespace
})

test_that("standardize_sex works with other/non-binary variants", {
  expect_equal(standardize_sex("Trans female"), "O")
  expect_equal(standardize_sex("Trans male"), "O")
  expect_equal(standardize_sex("transgender"), "O")
  expect_equal(standardize_sex("nonbinary"), "O")
  expect_equal(standardize_sex("non-binary"), "O")
  expect_equal(standardize_sex("binary"), "O")
})

test_that("standardize_sex returns U for unknown values", {
  expect_equal(standardize_sex("unknown"), "U")
  expect_equal(standardize_sex("other"), "U")
  expect_equal(standardize_sex("xyz"), "U")
  expect_equal(standardize_sex(""), "U")
  expect_equal(standardize_sex(NA_character_), "U")
})

test_that("standardize_sex handles vectors", {
  input <- c("Male", "f", "Trans female", "nonbinary", "unknown", NA)
  expected <- c("M", "F", "O", "O", "U", "U")
  expect_equal(standardize_sex(input), expected)
})

test_that("standardize_sex errors on non-character input", {
  expect_error(standardize_sex(123), "Input x must be a character vector")
  expect_error(
    standardize_sex(c(1, 2, 3)),
    "Input x must be a character vector"
  )
  expect_error(
    standardize_sex(factor(c("M", "F"))),
    "Input x must be a character vector"
  )
})

test_that("read_labdata reads CSV files correctly", {
  fp <- system.file("extdata/rped.csv", package = "rpdemo", mustWork = TRUE)
  expect_silent(data <- read_labdata(fp))

  # Check that it returns a data frame
  expect_s3_class(data, "data.frame")

  # Check expected columns exist
  expect_true("sex" %in% names(data))
  expect_true("age" %in% names(data))
  expect_true("zip" %in% names(data))

  # Check column types
  expect_type(data$sex, "character")
  expect_type(data$age, "integer")
  expect_type(data$zip, "character")
})

test_that("prep_lab_data standardizes sex variable", {
  expect_silent(result <- prep_lab_data(rped, warnings = FALSE))

  # Check that sex is standardized
  expect_true(all(result$sex %in% c("M", "F", "O", "U")))
})

test_that("prep_lab_data joins zipcodes data", {
  expect_silent(result <- prep_lab_data(rped, warnings = FALSE))

  # Check that city and state columns are added
  expect_true("city" %in% names(result))
  expect_true("state" %in% names(result))
})

test_that("prep_lab_data scales lab values", {
  expect_silent(result <- prep_lab_data(rped, warnings = FALSE))

  # Check that lab columns exist
  lab_cols <- grep("^lab", names(result), value = TRUE)
  expect_true(length(lab_cols) > 0)

  # Check that lab values are scaled (mean should be close to 0, sd close to 1)
  for (col in lab_cols) {
    col_mean <- mean(result[[col]], na.rm = TRUE)
    col_sd <- sd(result[[col]], na.rm = TRUE)
    expect_true(abs(col_mean) < 1e-10) # essentially zero
    expect_equal(col_sd, 1, tolerance = 1e-10)
  }
})

test_that("prep_lab_data issues warnings when warnings = TRUE", {
  # Create test data with issues
  test_data <- data.frame(
    id = 1:4,
    sex = c("M", "F", "M", "F"),
    age = c(15, 25, -5, 120),
    zip = c("99999", "20108", "20109", "20110"),
    lab1 = c(1, 2, 3, 4)
  )

  # Expect warnings for bad zip, minor, and unrealistic age
  # Use expect_warning to capture each warning in sequence
  suppressWarnings({
    result <- prep_lab_data(test_data, warnings = TRUE)
  })

  # Test that the function does produce warnings by running it again
  expect_warning(prep_lab_data(test_data, warnings = TRUE))
})

test_that("prep_lab_data does not issue warnings when warnings = FALSE", {
  # Create test data with issues
  test_data <- data.frame(
    id = 1:4,
    sex = c("M", "F", "M", "F"),
    age = c(15, 25, -5, 120),
    zip = c("99999", "20108", "20109", "20110"),
    lab1 = c(1, 2, 3, 4)
  )

  expect_silent(prep_lab_data(test_data, warnings = FALSE))
})

test_that("prep_lab_data handles edge cases in age checks", {
  # Test boundary values for age
  test_data <- data.frame(
    id = 1:5,
    sex = c("M", "F", "M", "F", "M"),
    age = c(0, 17, 18, 110, 111),
    zip = rep("20108", 5),
    lab1 = 1:5
  )

  # Age 0 is not a minor (< 18 means minors)
  # Age 17 is a minor
  # Age 18 is not a minor
  # Age 110 is not unrealistic (<0 or >110 is unrealistic)
  # Age 111 is unrealistic
  suppressWarnings({
    result <- prep_lab_data(test_data, warnings = TRUE)
  })

  # Just verify warnings are produced
  expect_warning(prep_lab_data(test_data, warnings = TRUE))
})

test_that("prep_lab_data returns correct structure", {
  expect_silent(result <- prep_lab_data(rped, warnings = FALSE))

  # Check it returns a data frame
  expect_s3_class(result, "data.frame")

  # Check original columns are preserved
  expect_true("id" %in% names(result))
  expect_true("sex" %in% names(result))
  expect_true("age" %in% names(result))
  expect_true("zip" %in% names(result))
})
