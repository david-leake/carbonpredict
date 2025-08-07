library(testthat)
library(carbonpredict)
library(ggplot2)

# Test: returns a ggplot object for valid emissions
test_that("returns ggplot for valid emissions", {
  p <- plot_sme_emissions(10000, 20000, company_name = "TestCo")
  expect_s3_class(p, "ggplot")
})

# Test: runs silently for valid input
test_that("runs silently for valid input", {
  expect_silent(plot_sme_emissions(5000, 10000))
})

# Test: handles zero emissions
test_that("handles zero emissions", {
  p <- plot_sme_emissions(0, 0)
  expect_s3_class(p, "ggplot")
})

# Test: handles NA emissions
test_that("handles NA emissions", {
  p <- plot_sme_emissions(NA, NA)
  expect_s3_class(p, "ggplot")
})

# Test: company_name is reflected in title

test_that("company_name is reflected in title", {
  p <- plot_sme_emissions(1000, 2000, company_name = "Acme Corp")
  expect_true(grepl("Acme Corp", p$labels$title))
})