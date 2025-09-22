# Load required libraries
library(dplyr)
library(ggplot2)

#' Predict SME Scope 3 Emissions
#'
#' This function loads pre-trained emissions models to predict scope 3 carbon emissions for a given SIC code and turnover.
#'
#' @importFrom utils read.csv
#' @importFrom stats predict
#' @importFrom lmerTest lmer
#' @param sic_code A 2-digit UK SIC code (numeric).
#' @param turnover Annual turnover value (numeric).
#' @return A data frame with predicted emissions (in tCo2e) for each scope 3 category.
#' @export
#' @examples
#' sme_scope3(sic_code = 85, turnover = 12000000)
sme_scope3 <- function(sic_code, turnover) {

    # Read industry variables from CSV
    industry_variables <- read.csv(system.file("extdata", "industry_variables.csv", package = "carbonpredict"), stringsAsFactors = FALSE)
    category_breakdown <- read.csv(system.file("extdata", "sme_category_breakdown.csv", package = "carbonpredict"), stringsAsFactors = FALSE)

    # Check user input
    if (!(sic_code %in% industry_variables$sic_code)) {
        stop("Please provide a valid 2-digit SIC code")
    }
    if (turnover <= 0 || turnover > 36000000) {
        stop("Please provide a turnover value between 0 and 36,000,000")
    }

    matched_row <- industry_variables[industry_variables$sic_code == as.numeric(sic_code), ]
    business_sector <- as.character(matched_row$business_sector[1])

    # Purchased Goods & Services (Scope 3 Cat 1+4)
    new_data_scope3 <- data.frame(
        SIC_2_2007 = sic_code,
        log_turnover = log(turnover)
    )
    coef_path_3 <- system.file("models", "Scope_3_Cat_1_4_Model.rds", package = "carbonpredict")
    if (file.exists(coef_path_3)) {
        coef_list_3 <- readRDS(coef_path_3)
        fixed_slope <- coef_list_3$fixed["log_turnover"]
        sic_char <- as.character(sic_code)
        if (sic_char %in% rownames(coef_list_3$random)) {
            random_slope <- coef_list_3$random[sic_char, "log_turnover"]
        } else {
            random_slope <- 0
        }
        log_pred <- (fixed_slope + random_slope) * new_data_scope3$log_turnover
        pred_scope3_cat1_4 <- exp(log_pred) / 1000
    } else {
        pred_scope3_cat1_4 <- NA
    }

    # Cat 3 Scope 1
    mac_intensity_scope1 <- as.numeric(matched_row$mac_intensity_scope1)
    skew <- as.numeric(matched_row$skew)
    log_turnover <- log(turnover)
    log_mac_intensity_scope1 <- log(mac_intensity_scope1)
    log_skew <- log(skew)
    # Prepare new data for Scope 1 Cat 3
    new_data_s1 <- data.frame(
        log_turnover = log(turnover),
        log_mac_intensity = log(mac_intensity_scope1),
        log_skew = log(skew)
    )
    new_data_s1$log_mac_intensity_log_skew <- new_data_s1$log_mac_intensity * new_data_s1$log_skew
    coef_path_1 <- system.file("models", "Scope_1_Cat_3_Model.rds", package = "carbonpredict")
    if (file.exists(coef_path_1)) {
        coef_list_1 <- readRDS(coef_path_1)
        fixed_1 <- coef_list_1$fixed
        if (sic_char %in% rownames(coef_list_1$random)) {
            random_1_value <- as.numeric(coef_list_1$random[sic_char, 1])
        } else {
            random_1_value <- 0
        }
        log_pred_1 <- (fixed_1["log_turnover"] + random_1_value) * new_data_s1$log_turnover +
                      fixed_1["log_mac_intensity"] * new_data_s1$log_mac_intensity +
                      fixed_1["log_skew"] * new_data_s1$log_skew +
                      fixed_1["log_mac_intensity:log_skew"] * new_data_s1$log_mac_intensity_log_skew
        pred_scope1_cat3 <- exp(log_pred_1) / 1000
    } else {
        pred_scope1_cat3 <- NA
    }

    # Cat 3 Scope 2
    mac_intensity_scope2 <- as.numeric(matched_row$mac_intensity_scope2)
    log_mac_intensity_scope2 <- log(mac_intensity_scope2)
    # Prepare new data for Scope 2 Cat 3
    new_data_s2 <- data.frame(
        log_turnover = log(turnover),
        log_mac_intensity = log(mac_intensity_scope2),
        log_skew = log(skew)
    )
    new_data_s2$log_mac_intensity_log_skew <- new_data_s2$log_mac_intensity * new_data_s2$log_skew
    coef_path_2 <- system.file("models", "Scope_2_Cat_3_Model.rds", package = "carbonpredict")
    if (file.exists(coef_path_2)) {
        coef_list_2 <- readRDS(coef_path_2)
        fixed_2 <- coef_list_2$fixed
        if (sic_char %in% rownames(coef_list_2$random)) {
            random_2_value <- as.numeric(coef_list_2$random[sic_char, 1])
        } else {
            random_2_value <- 0
        }
        log_pred_2 <- (fixed_2["log_turnover"] + random_2_value) * new_data_s2$log_turnover +
                      fixed_2["log_mac_intensity"] * new_data_s2$log_mac_intensity +
                      fixed_2["log_skew"] * new_data_s2$log_skew +
                      fixed_2["log_mac_intensity:log_skew"] * new_data_s2$log_mac_intensity_log_skew
        pred_scope2_cat3 <- exp(log_pred_2) / 1000
    } else {
        pred_scope2_cat3 <- NA
    }

    # Calculate Categories
    category_1_4_share <- 
    category_breakdown[category_breakdown$category == 1, business_sector, drop = TRUE] +
    category_breakdown[category_breakdown$category == 4, business_sector, drop = TRUE]

    scope3_total_exc_cat3 = pred_scope3_cat1_4 / category_1_4_share
    scope3_total = scope3_total_exc_cat3 + pred_scope1_cat3 + pred_scope2_cat3

    # Calculate category emissions
    category_emissions <- sapply(1:13, function(i) {
        if (i == 3) {
            pred_scope1_cat3 + pred_scope2_cat3
        } else {
            scope3_total_exc_cat3 * category_breakdown[category_breakdown$category == i, business_sector, drop = TRUE]
        }
    })
    # Build final results table
    results_table <- data.frame(
        Category = c(1:13, "Total"),
        Description = c(category_breakdown[match(1:13, as.numeric(category_breakdown$category)), "descriptions"], ""),
        "Predicted Emissions (tCO2e)" = c(round(category_emissions, 2), round(sum(category_emissions, na.rm = TRUE), 2)),
        check.names = FALSE
    )
    return(results_table)
}