# Load required libraries
library(lme4)

#' Predict Farm Scope 1 Emissions
#'
#' This function loads a pre-trained emission model to predict scope 1 carbon emissions for a British farm.
#' The function predicts emissions for the following farm types: "Cereals ex. rice", "Dairy", "Mixed farming", "Sheep and goats", "Cattle & buffaloes", "Poultry", "Animal production", "Support for crop production".
#'
#' @param sic_code A 4-digit UK SIC code (numeric).
#' @param farm_area Farm area in hectares.
#' @param no_beef_cows Number of beef cows.
#' @param no_dairy_cows Number of dairy cows.
#' @param no_pigs Number of pigs.
#' @param no_sheep Number of sheep.
#' @param annual_revenue Annual revenue (£)
#' @param annual_fuel_spend Annual fuel spend (£)
#' @return A dataframe with predicted emissions (tCO2e)
#' @export
#' @examples
#' farms_scope1(
#' sic_code = 1110,
#' farm_area = 1113,
#' no_beef_cows = 25,
#' no_dairy_cows = 8,
#' no_pigs = 18,
#' no_sheep = 29,
#' annual_revenue = 2986511,
#' annual_fuel_spend = 209055)
farms_scope1 <- function(sic_code, farm_area, no_beef_cows, no_dairy_cows, no_pigs, no_sheep, annual_revenue, annual_fuel_spend) {
  
  # Load farms scope 1 model
  farms_model <- readRDS(system.file("models", "Farms_Scope_1_Model.rds", package = "carbonpredict"))

  # Check input
  allowed_sic_codes <- c(1110, 1500, 1410, 1400, 1450, 1460, 1470, 1610, 1420)
  if (!is.numeric(sic_code) || !(sic_code %in% allowed_sic_codes)) {
    stop("Please provide a valid 4-digit SIC code")
  }
  if (!is.numeric(farm_area) || farm_area <= 0) {
    stop("Please provide a valid farm_area value")
  }
  if (!is.numeric(no_beef_cows) || no_beef_cows < 0) {
    stop("Please provide a valid number of beef cows")
  }
  if (!is.numeric(no_dairy_cows) || no_dairy_cows < 0) {
    stop("Please provide a valid number of dairy cows")
  }
  if (!is.numeric(no_pigs) || no_pigs < 0) {
    stop("Please provide a valid number of pigs")
  }
  if (!is.numeric(no_sheep) || no_sheep < 0) {
    stop("Please provide a valid number of sheep")
  }
  if (!is.numeric(annual_revenue) || annual_revenue <= 0 || annual_revenue > 36000000) {
    stop("Please provide an annual revenue value between 0 and 36,000,000")
  }
  if (!is.numeric(annual_fuel_spend) || annual_fuel_spend < 0) {
    stop("Please provide a valid annual fuel spend")
  }

  # Calculate intensities
  productivity <- farm_area / annual_revenue
  cattle_intensity <- no_beef_cows / annual_revenue
  dairy_intensity <- no_dairy_cows / annual_revenue
  sheep_intensity <- no_sheep / annual_revenue
  pigs_intensity <- no_pigs / annual_revenue
  fuel_intensity <- annual_fuel_spend / annual_revenue

  # Prepare input for prediction
  input_df <- data.frame(
    productivity = productivity,
    cattle_intensity = cattle_intensity,
    dairy_intensity = dairy_intensity,
    sheep_intensity = sheep_intensity,
    pigs_intensity = pigs_intensity,
    fuel_intensity = fuel_intensity,
    sic = as.factor(sic_code)
  )

  # Predict emission intensity
  pred <- predict(farms_model, newdata = input_df, allow.new.levels = TRUE)

  # convert to carbon emissions
  emissions = pred * annual_revenue  # already in tCO2e
  results <- data.frame("Predicted Emissions (tCO2e)" = round(emissions, 2), check.names = FALSE)

  return(results)
}