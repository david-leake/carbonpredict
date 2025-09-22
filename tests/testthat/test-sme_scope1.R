library(testthat)
library(carbonpredict)

# Load validation data
validation_data <- read.csv(
  system.file("intdata", "validation_data.csv", package = "carbonpredict"),
  stringsAsFactors = FALSE
)

# Test: sme_scope1 predictions match expected values in validation sample
test_that("sme_scope1 predictions match expected values", {
  n <- nrow(validation_data)
  for (i in seq_len(n)) {
    sic_code <- validation_data$SIC_2_2007[i]
    turnover <- validation_data$lbg_turnover[i]

    scope1_pred <- sme_scope1(sic_code, turnover)
    expected_value <- as.numeric(validation_data$predicted_scope1[i] / 1000) # convert kg to t
    expected_value <- round(expected_value, 2) # round to 2 decimal places
    expect_equal(
      as.numeric(scope1_pred[["Predicted Emissions (tCO2e)"]]),
      expected_value,
      tolerance = 0.01
    )
  }
})

## Error handling tests

# Test: invalid SIC code
test_that("errors for invalid SIC code", {
  expect_error(sme_scope1(999, 1000000), "Please provide a valid 2-digit SIC code")
})

# Test: invalid turnover (too low)
test_that("errors for turnover below range", {
  expect_error(sme_scope1(10, -1), "Please provide a turnover value between 0 and 36,000,000")
})

# Test: invalid turnover (too high)
test_that("errors for turnover above range", {
  expect_error(sme_scope1(10, 40000000), "Please provide a turnover value between 0 and 36,000,000")
})