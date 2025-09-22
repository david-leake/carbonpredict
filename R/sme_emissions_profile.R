# Load required libraries
library(grid)

#' SME Emissions Profile
#'
#' Calls the Scope 1, 2 and 3 emissions prediction functions and returns their results as a list and plots a donut chart
#'
#' @param sic_code A 2-digit UK SIC code (numeric).
#' @param turnover Annual turnover value (numeric).
#' @param company_name Optional company name for labeling plots (character string).
#' @return A list with three elements: \code{scope1}, \code{scope2} \code{scope3}, each containing the predicted emissions data frame (in tCo2e), as well as a donut chart.
#' @export
#' @examples
#' sme_emissions_profile(sic_code = 85, turnover = 12000000, company_name = "ABC")
sme_emissions_profile <- function(sic_code, turnover, company_name = NULL) {
	scope1 <- sme_scope1(sic_code, turnover)
	scope2 <- sme_scope2(sic_code, turnover)
	scope3 <- sme_scope3(sic_code, turnover)
	list_result <- list(scope1 = scope1, scope2 = scope2, scope3 = scope3)

	print(plot_sme_emissions(scope1$`Predicted Emissions (tCO2e)`, 
													 scope2$`Predicted Emissions (tCO2e)`,
													 scope3[scope3$Category == "Total", "Predicted Emissions (tCO2e)"],
													 company_name))

	grid::grid.newpage()

	plot_scope3_emissions(scope3, company_name)

	return(list_result)
}