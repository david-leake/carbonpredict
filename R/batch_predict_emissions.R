# Load required libraries
library(progress)

#' Batch Predict Emissions
#'
#' Prediction entry point for batch SME and Farms emissions

#' @importFrom utils read.csv write.csv
#' @param data A single entry (list or named vector), a data frame, or a path to a CSV file. The data should contain company_name, 2-digit UK sic_code, and annual turnover columns.
#' @param output_path Optional file path to save the results as a CSV. If NULL, results are not saved to a file.
#' @param company_type A single parameter "sme" or "farm" to determine which emission prediction functions to call (defaults to "sme").
#' @return A data frame with input columns and predicted emissions for each scope (in tCo2e). Optionally saved to a CSV file.
#' @export
#' @examples
#' sample_data <- read.csv(system.file("extdata", "sme_examples.csv", package = "carbonpredict"))
#' sample_data <- head(sample_data, 3)
#' batch_predict_emissions(data = sample_data, output_path = NULL, company_type = "sme")
batch_predict_emissions <- function(data, output_path = NULL, company_type = "sme") {

  # Handle input types
  if (is.character(data) && length(data) == 1 && file.exists(data)) {
    data <- read.csv(data)
  } else if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame(list(data), stringsAsFactors = FALSE)
  } 

  if (company_type == "sme") {
    if (!all(c("sic_code", "turnover") %in% colnames(data))) {
    stop("Input must have columns 'sic_code' and 'turnover'")
    }
    emission_functions <- c("sme_scope1", "sme_scope2", "sme_scope3")
  } else if (company_type == "farm") {
    if (!all(c("sic_code", "farm_area", "no_beef_cows", "no_dairy_cows", "no_pigs", "no_sheep", "annual_revenue", "annual_fuel_spend") %in% colnames(data))) {
    stop("Input must have columns 'sic_code', 'farm_area', 'no_beef_cows', 'no_dairy_cows', 'no_pigs', 'no_sheep', 'annual_revenue', 'annual_fuel_spend'")
    }
    emission_functions <- c("farms_scope1")
  } else {
    stop("Please enter a valid company type ('sme' or 'farm')")
  }

  n <- nrow(data)
  results <- list()
  pb <- progress::progress_bar$new(
    format = "  Predicting [:bar] :percent eta: :eta",
    total = n, clear = FALSE, width = 60
  )
  for (i in seq_len(n)) {
    row <- data[i, ]
    res <- as.list(row)
    for (fn in emission_functions) {
      pred <- tryCatch({
        if (company_type == "farm") {
          out <- get(fn)(row$sic_code, row$farm_area, row$no_beef_cows, row$no_dairy_cows, row$no_pigs, row$no_sheep, row$annual_revenue, row$annual_fuel_spend)
          if ("Predicted Emissions (tCO2e)" %in% names(out)) as.numeric(out[["Predicted Emissions (tCO2e)"]][[1]]) else as.numeric(out)
        }
        else if (fn == "sme_scope3") {
          scope3 <- get(fn)(row$sic_code, row$turnover)
          val <- NA
          if (is.data.frame(scope3) && "Category" %in% names(scope3) && any(scope3$Category == "Total")) {
            val <- scope3[scope3$Category == "Total", "Predicted Emissions (tCO2e)"]
            if (length(val) > 0) val <- as.numeric(val[[1]])
          }
        } else {
          out <- get(fn)(row$sic_code, row$turnover)
          if ("Predicted Emissions (tCO2e)" %in% names(out)) as.numeric(out[["Predicted Emissions (tCO2e)"]][[1]]) else as.numeric(out)
        }
      }, error = function(e) NA)
      res[[fn]] <- pred
    }
    results[[i]] <- res
    pb$tick()
  }
  results_df <- as.data.frame(do.call(rbind, results))
  # Ensure all columns are atomic vectors before saving
  results_df[] <- lapply(results_df, function(col) {
    if (is.list(col)) unlist(col, recursive = FALSE) else col
  })
  if (!is.null(output_path)) {
    write.csv(results_df, output_path, row.names = FALSE)
  }
  return(results_df)
}