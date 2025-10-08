# Load required libraries

#' Predict Top 5 SME Scope 3 Emissions Hotspots
#'
#' This function uses pre-computed results to predict the top 5 scope 3 carbon emissions hotspots for a given SIC code.
#'
#' @importFrom utils read.csv
#' @param sic_code A 2-digit UK SIC code (numeric).
#' @return A data frame with the top 5 emissions hotspots for scope 3.
#' @export
#' @examples
#' sme_scope3_hotspots(sic_code = 85)
sme_scope3_hotspots <- function(sic_code) {

    # Read hotspots from CSV
    hotspots <- read.csv(system.file("extdata", "sme_top5_hotspots.csv", package = "carbonpredict"), stringsAsFactors = FALSE)

    # Check user input
    if (!(sic_code %in% hotspots$sic_code)) {
        stop("Please provide a valid 2-digit SIC code")
    }

    matched_row <- hotspots[hotspots$sic_code == as.numeric(sic_code), ]

    results <- data.frame(
        Hotspot = c(1:5),
        Description = as.character(c(
            matched_row$hotspot_1[1],
            matched_row$hotspot_2[1],
            matched_row$hotspot_3[1],
            matched_row$hotspot_4[1],
            matched_row$hotspot_5[1]
        )),
        stringsAsFactors = FALSE
    )
    return(results)
}
