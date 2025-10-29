library(testthat)
library(carbonpredict)
library(mockery)

# Helper data
sme_data <- data.frame(
  sme_scope1 = c(1000, 2000),
  sme_scope2 = c(1500, 2500),
  company_name = c("Alpha", "Beta")
)

# Test: runs silently and calls plot_sme_emissions for each row
test_that("runs silently and calls plot_sme_emissions for each row", {
  called <- 0
  stub(batch_sme_plots, "plot_sme_emissions", function(...) { called <<- called + 1; invisible(NULL) })
  stub(batch_sme_plots, "print", function(x) invisible(NULL))
  expect_silent(batch_sme_plots(sme_data))
  expect_equal(called, nrow(sme_data))
})

# Test: errors for missing columns
test_that("errors for missing columns", {
  bad_data <- data.frame(foo = 1, bar = 2)
  expect_error(batch_sme_plots(bad_data), "Input must have columns 'sme_scope1' and 'sme_scope2' with precomputed emissions")
})

# Test: creates output directory and saves plots
test_that("creates output directory and saves plots", {
  tmpdir <- tempfile("plots_")
  dir.create(tmpdir)
  stub(batch_sme_plots, "plot_sme_emissions", function(...) invisible(NULL))
  stub(batch_sme_plots, "print", function(x) invisible(NULL))
  called <- 0
  stub(batch_sme_plots, "ggsave", function(filename, plot, ...) { called <<- called + 1; invisible(NULL) })
  batch_sme_plots(sme_data, output_path = tmpdir)
  expect_equal(called, nrow(sme_data))
  unlink(tmpdir, recursive = TRUE)
})

# Test: loads CSV file in batch_sme_plots
test_that("loads CSV file in batch_sme_plots", {
  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(sme_data, tmp_csv, row.names = FALSE)
  called <- 0
  stub(batch_sme_plots, "plot_sme_emissions", function(...) { called <<- called + 1; invisible(NULL) })
  stub(batch_sme_plots, "print", function(x) invisible(NULL))
  expect_silent(batch_sme_plots(tmp_csv))
  expect_equal(called, nrow(sme_data))
  unlink(tmp_csv)
})

test_that("creates output_path directory if it does not exist", {
  tmpdir <- tempfile("newplots_")
  # Ensure the directory does not exist
  if (dir.exists(tmpdir)) unlink(tmpdir, recursive = TRUE)
  stub(batch_sme_plots, "plot_sme_emissions", function(...) invisible(NULL))
  stub(batch_sme_plots, "print", function(x) invisible(NULL))
  stub(batch_sme_plots, "ggsave", function(filename, plot, ...) invisible(NULL))
  batch_sme_plots(sme_data, output_path = tmpdir)
  expect_true(dir.exists(tmpdir))
  unlink(tmpdir, recursive = TRUE)
})