library(testthat)
library(carbonpredict)
library(mockery)

# Helper data
sme_data <- data.frame(sic_code = "12345", turnover = 100000)

# Test: SME batch prediction returns expected columns
test_that("SME batch prediction returns expected columns", {
  res <- batch_predict_emissions(sme_data, company_type = "sme")
  expect_true(all(c("sic_code", "turnover", "sme_scope1", "sme_scope2", "sme_scope3") %in% colnames(res)))
  expect_equal(nrow(res), 1)
})

# Test: SME batch prediction works with list input
test_that("SME batch prediction works with list input", {
  res <- batch_predict_emissions(list(sic_code = "12345", turnover = 100000), company_type = "sme")
  expect_true(all(c("sic_code", "turnover", "sme_scope1", "sme_scope2", "sme_scope3") %in% colnames(res)))
})

# Test: Error for missing columns (sme)
test_that("Error for missing columns", {
  bad_data <- data.frame(foo = 1, bar = 2)
  expect_error(batch_predict_emissions(bad_data, company_type = "sme"))
})

# Test: Error for missing columns (farm)
test_that("Error for missing columns", {
  bad_data <- data.frame(foo = 1, bar = 2)
  expect_error(batch_predict_emissions(bad_data, company_type = "farm"))
})

# Test: Error for invalid company_type
test_that("Error for invalid company_type", {
  expect_error(batch_predict_emissions(sme_data, company_type = "other"))
})

# Test: loads CSV file in batch_predict_emissions (sme)
test_that("loads CSV file in batch_predict_emissions", {
  tmp_csv <- tempfile(fileext = ".csv")
  df <- data.frame(sic_code = c("85", "10"), turnover = c(12000000, 5000000))
  write.csv(df, tmp_csv, row.names = FALSE)
  res <- batch_predict_emissions(tmp_csv, company_type = "sme")
  expect_true(all(c("sic_code", "turnover", "sme_scope1", "sme_scope2", "sme_scope3") %in% colnames(res)))
  expect_equal(nrow(res), 2)
  unlink(tmp_csv)
})

# Test: loads CSV file in batch_predict_emissions (farm)
test_that("loads CSV file in batch_predict_emissions", {
  tmp_csv <- tempfile(fileext = ".csv")
  df <- data.frame(
    sic_code = 1110,
    farm_area = 1113,
    no_beef_cows = 25,
    no_dairy_cows = 8,
    no_pigs = 18,
    no_sheep = 29,
    annual_revenue = 2986511,
    annual_fuel_spend = 209055
  )
  write.csv(df, tmp_csv, row.names = FALSE)
  res <- batch_predict_emissions(tmp_csv, company_type = "farm")
  expect_true(all(c("sic_code", "farm_area", "no_beef_cows", "no_dairy_cows", "no_pigs", "no_sheep", "annual_revenue", "annual_fuel_spend", "farms_scope1") %in% colnames(res)))
  expect_equal(nrow(res), 1)
  unlink(tmp_csv)
})

# Test: writes CSV output
test_that("writes CSV output", {
  tmp_csv <- tempfile(fileext = ".csv")
  df <- data.frame(sic_code = "85", turnover = 12000000)
  batch_predict_emissions(df, company_type = "sme", output_path = tmp_csv)
  expect_true(file.exists(tmp_csv))
  unlink(tmp_csv)
})

# Test: calls all three scope functions
test_that("calls all three scope functions", {
  called <- list(scope1 = FALSE, scope2 = FALSE, scope3 = FALSE)
  stub(batch_predict_emissions, "sme_scope1", function(sic, turnover) { called$scope1 <<- TRUE; list(`Predicted Emissions (tCO2e)` = 12345) })
  stub(batch_predict_emissions, "sme_scope2", function(sic, turnover) { called$scope2 <<- TRUE; list(`Predicted Emissions (tCO2e)` = 67890) })
  stub(batch_predict_emissions, "sme_scope3", function(sic, turnover) { called$scope3 <<- TRUE; data.frame(Category = "Total", `Predicted Emissions (tCO2e)` = 11111, stringsAsFactors = FALSE) })
  df <- data.frame(sic_code = "85", turnover = 12000000)
  batch_predict_emissions(df, company_type = "sme")
  expect_true(called$scope1)
  expect_true(called$scope2)
  expect_true(called$scope3)
})

# Test: farms_scope1 gets called and returns expected output from CSV input
test_that("farms_scope1 gets called and returns expected output", {
  # Create a temp CSV with farm data
  tmp_csv <- tempfile(fileext = ".csv")
  df <- data.frame(
    farm_name = "Yield & Harvest LLP",
    sic_code = 1110,
    sic_label = "Cereals ex. rice",
    farm_area = 1113,
    no_beef_cows = 25,
    no_dairy_cows = 8,
    no_pigs = 18,
    no_sheep = 29,
    annual_revenue = 2986511,
    annual_fuel_spend = 209055
  )
  write.csv(df, tmp_csv, row.names = FALSE)
  # Run batch_predict_emissions with company_type = 'farm'
  res <- batch_predict_emissions(tmp_csv, company_type = "farm")
  # Check that farms_scope1 column exists and is numeric
  expect_true("farms_scope1" %in% colnames(res))
  expect_type(res$farms_scope1, "double")
  expect_equal(nrow(res), 1)
  unlink(tmp_csv)
})

# Test: farms_scope1 gets called and returns expected output from dataframe input
test_that("farms_scope1 gets called and returns expected output with dataframe input", {
  df <- data.frame(
    farm_name = "Yield & Harvest LLP",
    sic_code = 1110,
    sic_label = "Cereals ex. rice",
    farm_area = 1113,
    no_beef_cows = 25,
    no_dairy_cows = 8,
    no_pigs = 18,
    no_sheep = 29,
    annual_revenue = 2986511,
    annual_fuel_spend = 209055
  )
  res <- batch_predict_emissions(df, company_type = "farm")
  expect_true("farms_scope1" %in% colnames(res))
  expect_type(res$farms_scope1, "double")
  expect_equal(nrow(res), 1)
})