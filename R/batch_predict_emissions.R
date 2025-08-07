# Load required libraries
library(progress)

#' Batch Predict Emissions
#'
#' Prediction entry point for batch SME and agriculture emissions

#' @importFrom utils read.csv write.csv
#' @param data A single entry (list or named vector), a data frame, or a path to a CSV file. The data should contain company_name, 2-digit UK sic_code, and annual turnover columns.
#' @param company_type A single entry "sme" or "farm" to determine which emission prediction funtions to call.
#' @param output_path Optional file path to save the results as a CSV. If NULL, results are not saved to a file.
#' @return A data frame with input columns and predicted emissions for each scope. Optionally saved to a CSV file.
#' @export
#' @examples
#' sample_data <- read.csv(system.file("extdata", "sme_examples.csv", package = "carbonpredict"))
#' sample_data <- head(sample_data, 3)
#' batch_predict_emissions(data = sample_data, company_type = "sme", output_path = NULL)
batch_predict_emissions <- function(data, company_type, output_path = NULL) {

  # Handle input types
  if (is.character(data) && length(data) == 1 && file.exists(data)) {
    data <- read.csv(data)
  } else if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame(list(data), stringsAsFactors = FALSE)
  } 

  if (!all(c("sic_code", "turnover") %in% colnames(data))) {
    stop("Input must have columns 'sic_code' and 'turnover'")
  }

  if (company_type == "sme") {
    emission_functions <- c("sme_scope1", "sme_scope2") # , "sme_scope3")
  } else if (company_type == "farm") {
    # emission_functions <- c("agri_scope1")
    stop("Agriculture emissions predictions coming soon!")
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
        out <- get(fn)(row$sic_code, row$turnover)
        if ("predicted_emissions" %in% names(out)) as.numeric(out$predicted_emissions[[1]]) else as.numeric(out)
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