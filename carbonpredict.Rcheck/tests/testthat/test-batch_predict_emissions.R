library(testthat)
library(carbonpredict)
library(mockery)

# Helper data
sme_data <- data.frame(sic_code = "12345", turnover = 100000)

# Test: SME batch prediction returns expected columns
test_that("SME batch prediction returns expected columns", {
  res <- batch_predict_emissions(sme_data, company_type = "sme")
  expect_true(all(c("sic_code", "turnover", "sme_scope1", "sme_scope2") %in% colnames(res)))
  expect_equal(nrow(res), 1)
})

# Test: SME batch prediction works with list input
test_that("SME batch prediction works with list input", {
  res <- batch_predict_emissions(list(sic_code = "12345", turnover = 100000), company_type = "sme")
  expect_true(all(c("sic_code", "turnover", "sme_scope1", "sme_scope2") %in% colnames(res)))
})

# Test: Error for missing columns
test_that("Error for missing columns", {
  bad_data <- data.frame(foo = 1, bar = 2)
  expect_error(batch_predict_emissions(bad_data, company_type = "sme"))
})

# Test: Error for invalid company_type
test_that("Error for invalid company_type", {
  expect_error(batch_predict_emissions(sme_data, company_type = "other"))
})

# Test: Error for farm company_type (not implemented)
test_that("Error for farm company_type (not implemented)", {
  expect_error(batch_predict_emissions(sme_data, company_type = "farm"),
               "Agriculture emissions predictions coming soon!")
})

# Test: loads CSV file in batch_predict_emissions
test_that("loads CSV file in batch_predict_emissions", {
  tmp_csv <- tempfile(fileext = ".csv")
  df <- data.frame(sic_code = c("85", "10"), turnover = c(12000000, 5000000))
  write.csv(df, tmp_csv, row.names = FALSE)
  res <- batch_predict_emissions(tmp_csv, company_type = "sme")
  expect_true(all(c("sic_code", "turnover", "sme_scope1", "sme_scope2") %in% colnames(res)))
  expect_equal(nrow(res), 2)
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

# Test: handles predicted emissions list output
test_that("handles predicted emissions list output", {
  stub(batch_predict_emissions, "sme_scope1", function(sic, turnover) list(predicted_emissions = 12345))
  stub(batch_predict_emissions, "sme_scope2", function(sic, turnover) 67890)
  df <- data.frame(sic_code = "85", turnover = 12000000)
  res <- batch_predict_emissions(df, company_type = "sme")
  expect_equal(res$sme_scope1, 12345)
  expect_equal(res$sme_scope2, 67890)
})