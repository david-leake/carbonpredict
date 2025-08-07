# Load required libraries
library(dplyr)

#' Predict SME Scope 2 Emissions
#'
#' This function loads a pre-trained emission model to predict scope 2 carbon emissions for a given SIC code and turnover.
#'
#' @importFrom utils read.csv
#' @importFrom stats predict
#' @param sic_code A 2-digit SIC code (numeric).
#' @param turnover Annual turnover value (numeric).
#' @return A data frame with predicted emissions and input variables.
#' @export
#' @examples
#' sme_scope2(sic_code = 85, turnover = 12000000)
sme_scope2 <- function(sic_code, turnover) {

    # Load Scope 2 model
    scope2_model <- readRDS(system.file("models", "Scope_2_Model.rds", package = "carbonpredict"))

    # Read industry variables from CSV
    industry_variables <- read.csv(system.file("extdata", "industry_variables.csv", package = "carbonpredict"), stringsAsFactors = FALSE)

    # Check user input
    if (!(sic_code %in% industry_variables$sic_code)) {
        stop("Please provide a valid 2-digit SIC code")
    }
    if (turnover <= 0 || turnover > 36000000) {
        stop("Please provide a turnover value between 0 and 36,000,000")
    }

    # Find row matching SIC code from industry variables
    matched_row <- industry_variables[industry_variables$sic_code == as.numeric(sic_code), ]
    mac_intensity_scope2 <- as.numeric(matched_row$mac_intensity_scope2)
    skew <- as.numeric(matched_row$skew)

    # Prepare new data for Scope 2
    new_data_scope2 <- data.frame(
    SIC_2_2007 = sic_code,
    lbg_turnover = turnover,
    mac_scope2_intensity = mac_intensity_scope2,
    tno_1M_firms = skew
    )
    new_data_scope2$log_turnover <- log(new_data_scope2$lbg_turnover)
    new_data_scope2$log_mac_intensity <- log(new_data_scope2$mac_scope2_intensity)
    new_data_scope2$log_skew <- log(new_data_scope2$tno_1M_firms)

    # Predict emissions using model's predict() function
    new_data_scope2$predicted_log_emissions <- predict(scope2_model, newdata = new_data_scope2, allow.new.levels = TRUE)
    new_data_scope2$predicted_emissions <- exp(new_data_scope2$predicted_log_emissions)

    return(new_data_scope2[, c("SIC_2_2007", "lbg_turnover", "mac_scope2_intensity", "tno_1M_firms", "predicted_emissions")])
}