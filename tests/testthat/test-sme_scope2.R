library(testthat)
library(carbonpredict)

# Load validation data
validation_data <- read.csv(
  system.file("intdata", "sme_validation_data.csv", package = "carbonpredict"),
  stringsAsFactors = FALSE
)

# Test: sme_scope2 predictions match expected values in validation sample
test_that("sme_scope2 predictions match expected values", {
  n <- nrow(validation_data)
  for (i in seq_len(n)) {
    sic_code <- validation_data$SIC_2_2007[i]
    turnover <- validation_data$lbg_turnover[i]

    scope2_pred <- sme_scope2(sic_code, turnover)
    expected_value <- as.numeric(validation_data$predicted_scope2[i] / 1000) # convert kg to t
    expected_value <- round(expected_value, 2) # round to 2 decimal places
    expect_equal(
      as.numeric(scope2_pred[["Predicted Emissions (tCO2e)"]]),
      expected_value,
      tolerance = 0.01
    )
  }
})

## Error handling tests

# Test: invalid SIC code
test_that("errors for invalid SIC code", {
  expect_error(sme_scope2(999, 1000000), "Please provide a valid 2-digit SIC code")
})

# Test: invalid turnover (too low)
test_that("errors for turnover below range", {
  expect_error(sme_scope2(10, -1), "Please provide a turnover value between 0 and 36,000,000")
})

# Test: invalid turnover (too high)
test_that("errors for turnover above range", {
  expect_error(sme_scope2(10, 40000000), "Please provide a turnover value between 0 and 36,000,000")
})