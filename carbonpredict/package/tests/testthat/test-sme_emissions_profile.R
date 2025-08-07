library(testthat)
library(carbonpredict)
library(mockery)

# Test: returns correct structure and values for valid input
test_that("returns correct structure and values for valid input", {
  stub(sme_emissions_profile, "plot_sme_emissions", function(...) invisible(NULL))
  stub(sme_emissions_profile, "print", function(x) invisible(NULL))
  result <- sme_emissions_profile(85, 12000000)
  expect_type(result, "list")
  expect_true(all(c("scope1", "scope2") %in% names(result)))
  expect_s3_class(result$scope1, "data.frame")
  expect_s3_class(result$scope2, "data.frame")
  expect_true("predicted_emissions" %in% names(result$scope1))
  expect_true("predicted_emissions" %in% names(result$scope2))
})

# Test: predicted emissions match sme_scope1 and sme_scope2
test_that("predicted emissions match sme_scope1 and sme_scope2", {
  stub(sme_emissions_profile, "plot_sme_emissions", function(...) invisible(NULL))
  stub(sme_emissions_profile, "print", function(x) invisible(NULL))
  profile <- sme_emissions_profile(85, 12000000)
  s1 <- sme_scope1(85, 12000000)
  s2 <- sme_scope2(85, 12000000)
  expect_equal(profile$scope1$predicted_emissions, s1$predicted_emissions)
  expect_equal(profile$scope2$predicted_emissions, s2$predicted_emissions)
})

# Test: errors for invalid SIC code
test_that("errors for invalid SIC code", {
  stub(sme_emissions_profile, "plot_sme_emissions", function(...) invisible(NULL))
  stub(sme_emissions_profile, "print", function(x) invisible(NULL))
  expect_error(sme_emissions_profile(999, 1000000), "Please provide a valid 2-digit SIC code")
})

# Test: errors for invalid turnover (too low)
test_that("errors for turnover below range", {
  stub(sme_emissions_profile, "plot_sme_emissions", function(...) invisible(NULL))
  stub(sme_emissions_profile, "print", function(x) invisible(NULL))
  expect_error(sme_emissions_profile(10, -1), "Please provide a turnover value between 0 and 36,000,000")
})

# Test: errors for invalid turnover (too high)
test_that("errors for turnover above range", {
  stub(sme_emissions_profile, "plot_sme_emissions", function(...) invisible(NULL))
  stub(sme_emissions_profile, "print", function(x) invisible(NULL))
  expect_error(sme_emissions_profile(10, 40000000), "Please provide a turnover value between 0 and 36,000,000")
})

# Test: plot_sme_emissions is called
test_that("plot_sme_emissions is called", {
  called <- FALSE
  stub(sme_emissions_profile, "plot_sme_emissions", function(...) {
    called <<- TRUE
    invisible(NULL)
  })
  sme_emissions_profile(85, 12000000)
  expect_true(called)
})
