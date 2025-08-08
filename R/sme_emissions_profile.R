#' SME Emissions Profile
#'
#' Calls the Scope 1 and Scope 2 emissions prediction functions and returns their results as a list and plots a donut chart
#'
#' @param sic_code A 2-digit UK SIC code (numeric).
#' @param turnover Annual turnover value (numeric).
#' @param company_name Optional company name for labeling plots (string).
#' @return A list with two elements: \code{scope1} and \code{scope2}, each containing the predicted emissions data frame, as well as a donut chart.
#' @export
#' @examples
#' sme_emissions_profile(sic_code = 85, turnover = 12000000, company_name = "ABC")
sme_emissions_profile <- function(sic_code, turnover, company_name = NULL) {
  scope1 <- sme_scope1(sic_code, turnover)
  scope2 <- sme_scope2(sic_code, turnover)
  list_result <- list(scope1 = scope1, scope2 = scope2)
  
  print(plot_sme_emissions(scope1$predicted_emissions[[1]], scope2$predicted_emissions[[1]], company_name))
  return(list_result)
}