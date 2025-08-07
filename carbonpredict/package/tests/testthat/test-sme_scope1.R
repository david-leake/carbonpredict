library(testthat)
library(carbonpredict)

# Test: valid SIC code and turnover
test_that("returns prediction for valid SIC and turnover", {
  # Replace with a real SIC code from your data
  expect_silent(result <- sme_scope1(85, 12000000))
  expect_true("predicted_emissions" %in% names(result))
  expect_equal(result$predicted_emissions, 376646, tolerance = 1e-6)
})

# Test: another valid input
test_that("returns prediction for another valid SIC and turnover", {
  expect_silent(result <- sme_scope1(16, 500000))
  expect_equal(result$predicted_emissions, 34272.42, tolerance = 1e-6)
})

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