# Load required libraries
library(grid)

#' SME Emissions Profile
#'
#' Calls the Scope 1, 2 and 3 emissions prediction functions and returns their results as a list and plots a donut chart
#'
#' @param sic_code A 2-digit UK SIC code (numeric).
#' @param turnover Annual turnover value (numeric).
#' @param company_name Optional company name for labeling plots (character string).
#' @return A list with four elements: \code{scope1}, \code{scope2} \code{scope3}, \code{scope3_hotspots}, each containing the predicted carbon emissions data frame (in tCo2e), the top 5 scope 3 emissions hotspots, as well as a donut chart and Sankey diagram showing the emissions breakdowns.
#' @export
#' @examples
#' sme_emissions_profile(sic_code = 85, turnover = 12000000, company_name = "Carbon Predict LTD")
sme_emissions_profile <- function(sic_code, turnover, company_name = NULL) {

	# Read hotspots from CSV to get company industry
    hotspots <- read.csv(system.file("extdata", "sme_top5_hotspots.csv", package = "carbonpredict"), stringsAsFactors = FALSE)
    
	# Check user input
    if (!(sic_code %in% hotspots$sic_code)) {
        stop("Please provide a valid 2-digit SIC code")
    }
    matched_row <- hotspots[hotspots$sic_code == as.numeric(sic_code), ]

	message(paste0("SME Carbon Emissions Profile for ", company_name))
	message(paste0("Industry: ", matched_row$sic_name[1]))

	scope1 <- sme_scope1(sic_code, turnover)
	scope2 <- sme_scope2(sic_code, turnover)
	scope3 <- sme_scope3(sic_code, turnover)
	scope3_hotspots <- sme_scope3_hotspots(sic_code)
	list_result <- list(scope1 = scope1, scope2 = scope2, scope3 = scope3, scope3_hotspots = scope3_hotspots)

	print(plot_sme_emissions(scope1$`Predicted Emissions (tCO2e)`,
							 scope2$`Predicted Emissions (tCO2e)`,
							 scope3[scope3$Category == "Total", "Predicted Emissions (tCO2e)"],
							 company_name))

	if (!interactive()) {
		grid::grid.newpage()
	}

	print(plot_scope3_emissions(scope3, company_name))

	return(list_result)
}