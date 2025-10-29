library(testthat)
library(carbonpredict)

# Load validation data
validation_data <- read.csv(
  system.file("intdata", "farms_validation_data.csv", package = "carbonpredict"),
  stringsAsFactors = FALSE
)

# Test: farms_scope1 predictions match expected values in validation sample
test_that("farms_scope1 produces correct predictions for validation data", {
  for (i in seq_len(nrow(validation_data))) {
    farms_scope1_pred <- farms_scope1(
      sic_code = as.numeric(validation_data$sic_code[i]),
      farm_area = validation_data$farm_area[i],
      no_beef_cows = validation_data$no_beef_cows[i],
      no_dairy_cows = validation_data$no_dairy_cows[i],
      no_pigs = validation_data$no_pigs[i],
      no_sheep = validation_data$no_sheep[i],
      annual_revenue = validation_data$annual_revenue[i],
      annual_fuel_spend = validation_data$annual_fuel_spend[i]
    )
    expect_equal(
      farms_scope1_pred[["Predicted Emissions (tCO2e)"]],
      round(validation_data$predicted_emissions[i], 2)
    )
  }
})

## Error handling tests

# Test: invalid types and ranges

test_that("errors for invalid sic_code", {
  expect_error(farms_scope1("abcd", 1113, 25, 8, 18, 29, 2986511, 209055), "Please provide a valid 4-digit SIC code")
  expect_error(farms_scope1(111, 1113, 25, 8, 18, 29, 2986511, 209055), "Please provide a valid 4-digit SIC code")
  expect_error(farms_scope1(999, 1113, 25, 8, 18, 29, 2986511, 209055), "Please provide a valid 4-digit SIC code")
  expect_error(farms_scope1(NULL, 1113, 25, 8, 18, 29, 2986511, 209055), "Please provide a valid 4-digit SIC code")
})

test_that("errors for invalid farm_area", {
  expect_error(farms_scope1(1110, "abc", 25, 8, 18, 29, 2986511, 209055), "Please provide a valid farm_area value")
  expect_error(farms_scope1(1110, -1, 25, 8, 18, 29, 2986511, 209055), "Please provide a valid farm_area value")
  expect_error(farms_scope1(1110, 0, 25, 8, 18, 29, 2986511, 209055), "Please provide a valid farm_area value")
  expect_error(farms_scope1(1110, NULL, 25, 8, 18, 29, 2986511, 209055), "Please provide a valid farm_area value")
})

test_that("errors for invalid no_beef_cows", {
  expect_error(farms_scope1(1110, 1113, "abc", 8, 18, 29, 2986511, 209055), "Please provide a valid number of beef cows")
  expect_error(farms_scope1(1110, 1113, -1, 8, 18, 29, 2986511, 209055), "Please provide a valid number of beef cows")
  expect_error(farms_scope1(1110, 1113, NULL, 8, 18, 29, 2986511, 209055), "Please provide a valid number of beef cows")
})

test_that("errors for invalid no_dairy_cows", {
  expect_error(farms_scope1(1110, 1113, 25, "abc", 18, 29, 2986511, 209055), "Please provide a valid number of dairy cows")
  expect_error(farms_scope1(1110, 1113, 25, -1, 18, 29, 2986511, 209055), "Please provide a valid number of dairy cows")
  expect_error(farms_scope1(1110, 1113, 25, NULL, 18, 29, 2986511, 209055), "Please provide a valid number of dairy cows")
})

test_that("errors for invalid no_pigs", {
  expect_error(farms_scope1(1110, 1113, 25, 8, "abc", 29, 2986511, 209055), "Please provide a valid number of pigs")
  expect_error(farms_scope1(1110, 1113, 25, 8, -1, 29, 2986511, 209055), "Please provide a valid number of pigs")
  expect_error(farms_scope1(1110, 1113, 25, 8, NULL, 29, 2986511, 209055), "Please provide a valid number of pigs")
})

test_that("errors for invalid no_sheep", {
  expect_error(farms_scope1(1110, 1113, 25, 8, 18, "abc", 2986511, 209055), "Please provide a valid number of sheep")
  expect_error(farms_scope1(1110, 1113, 25, 8, 18, -1, 2986511, 209055), "Please provide a valid number of sheep")
  expect_error(farms_scope1(1110, 1113, 25, 8, 18, NULL, 2986511, 209055), "Please provide a valid number of sheep")
})

test_that("errors for invalid annual_revenue", {
  expect_error(farms_scope1(1110, 1113, 25, 8, 18, 29, "abc", 209055), "Please provide an annual revenue value between 0 and 36,000,000")
  expect_error(farms_scope1(1110, 1113, 25, 8, 18, 29, -1, 209055), "Please provide an annual revenue value between 0 and 36,000,000")
  expect_error(farms_scope1(1110, 1113, 25, 8, 18, 29, 0, 209055), "Please provide an annual revenue value between 0 and 36,000,000")
  expect_error(farms_scope1(1110, 1113, 25, 8, 18, 29, 40000000, 209055), "Please provide an annual revenue value between 0 and 36,000,000")
  expect_error(farms_scope1(1110, 1113, 25, 8, 18, 29, NULL, 209055), "Please provide an annual revenue value between 0 and 36,000,000")
})

test_that("errors for invalid annual_fuel_spend", {
  expect_error(farms_scope1(1110, 1113, 25, 8, 18, 29, 2986511, "abc"), "Please provide a valid annual fuel spend")
  expect_error(farms_scope1(1110, 1113, 25, 8, 18, 29, 2986511, -1), "Please provide a valid annual fuel spend")
  expect_error(farms_scope1(1110, 1113, 25, 8, 18, 29, 2986511, NULL), "Please provide a valid annual fuel spend")
})