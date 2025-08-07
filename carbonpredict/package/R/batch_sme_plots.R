# Load required libraries
library(progress)

#' Batch SME Plots
#'
#' Batch plot SME Scope 1 & 2 emissions
#'
#' @importFrom utils read.csv
#' @importFrom ggplot2 ggsave
#' @param data A data frame or path to a CSV file with columns "sic_code", "turnover", and optionally "company_name".
#' @param output_path Optional directory to save plots. If NULL, plots are not saved.
#' @return Donut chart plots for each row in the data. Optionally saved to a directory as PNG files.
#' @export
#' @examples
#' sample_data <- read.csv(system.file("extdata", "sme_examples.csv", package = "carbonpredict"))
#' sample_data <- head(sample_data, 3)
#' batch_sme_emissions <- batch_predict_emissions(data = sample_data, company_type = "sme", output_path = NULL)
#' batch_sme_plots(data = batch_sme_emissions, output_path = NULL)
batch_sme_plots <- function(data, output_path = NULL) {
  if (is.character(data) && length(data) == 1 && file.exists(data)) {
    data <- read.csv(data)
  }
  if (!all(c("sme_scope1", "sme_scope2") %in% colnames(data))) {
    stop("Input must have columns 'sme_scope1' and 'sme_scope2' with precomputed emissions")
  }
  if (!is.null(output_path) && !dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  n <- nrow(data)
  pb <- progress::progress_bar$new(
    format = "  Plotting [:bar] :percent eta: :eta",
    total = n, clear = FALSE, width = 60
  )
  for (i in seq_len(n)) {
    row <- data[i, ]
    scope1_val <- as.numeric(row$sme_scope1)
    scope2_val <- as.numeric(row$sme_scope2)
    p <- plot_sme_emissions(scope1_val, scope2_val, ifelse(!is.null(row$company_name) && !is.na(row$company_name), as.character(row$company_name), NULL))
    print(p)
    if (!is.null(output_path)) {
      company_name <- ifelse(!is.null(row$company_name) && !is.na(row$company_name), as.character(row$company_name), paste0("company_", i))
      safe_name <- gsub("[^A-Za-z0-9_]", "_", company_name)
      plot_file <- file.path(output_path, paste0(safe_name, "_plot.png"))
      ggsave(plot_file, p, width = 6, height = 6)
    }
    pb$tick()
  }
}
