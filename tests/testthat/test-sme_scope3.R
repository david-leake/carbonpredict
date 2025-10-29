library(carbonpredict)

# Load validation data
validation_data <- read.csv(
  system.file("intdata", "sme_validation_data.csv", package = "carbonpredict"),
  stringsAsFactors = FALSE
)

# Test: sme_scope3 predictions for Category 1+4 match expected values in validation sample
test_that("sme_scope3 predictions for Category 1+4 match expected values", {
  n <- nrow(validation_data)
  for (i in seq_len(n)) {
    sic_code <- validation_data$SIC_2_2007[i]
    turnover <- validation_data$lbg_turnover[i]

    scope3_pred <- sme_scope3(sic_code, turnover)
    # Category 1 + 4 sum
    cat1_4_sum <- round(sum(scope3_pred[scope3_pred$Category %in% c(1, 4), "Predicted Emissions (tCO2e)"]), 2)
    expected_value <- as.numeric(validation_data$predicted_purchased_goods[i] / 1000)
    expected_value <- round(expected_value, 2)
    expect_equal(
      as.numeric(cat1_4_sum),
      expected_value,
      tolerance = 0.01
    )
  }
})

# Test: sme_scope3 predictions for Category 3 match expected values in validation sample
test_that("sme_scope3 predictions for Category 3 match expected values", {
  n <- nrow(validation_data)
  for (i in seq_len(n)) {
    sic_code <- validation_data$SIC_2_2007[i]
    turnover <- validation_data$lbg_turnover[i]

    scope3_pred <- sme_scope3(sic_code, turnover)
    # Category 3 sum
    cat3_pred <- scope3_pred[scope3_pred$Category == 3, "Predicted Emissions (tCO2e)"]
    expected_cat3 <- as.numeric(validation_data$predicted_elec_WTT[i] / 1000) + as.numeric(validation_data$predicted_fuel_WTT[i] / 1000)
    expected_cat3 <- round(expected_cat3, 2)
    expect_equal(
      as.numeric(cat3_pred),
      expected_cat3,
      tolerance = 0.01
    )
  }
})