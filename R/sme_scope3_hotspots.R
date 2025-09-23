# Load required libraries

#' Predict Top 5 SME Scope 3 Emissions Hotspots
#'
#' This function uses pre-computed results to predict the top 5 scope 3 carbon emissions hotspots for a given SIC code.
#'
#' @importFrom utils read.csv
#' @param sic_code A 2-digit UK SIC code (numeric).
#' @return A data frame with the top 5 emissions hostposts for scope 3.
#' @export
#' @examples
#' sme_scope3_hotspots(sic_code = 85)
sme_scope3_hotspots <- function(sic_code) {

    # Read industry variables from CSV
    hotspots <- read.csv(system.file("extdata", "sme_top5_hotspots.csv", package = "carbonpredict"), stringsAsFactors = FALSE)

    # Check user input
    if (!(sic_code %in% hotspots$sic_code)) {
        stop("Please provide a valid 2-digit SIC code")
    }

    matched_row <- hotspots[hotspots$sic_code == as.numeric(sic_code), ]

    result <- c(
        paste0("Industry: ", matched_row$sic_name[1]),
        paste0("Top 5 Hotspots:"),
        paste0("Hotspot 1: ", matched_row$hotspot_1[1]),
        paste0("Hotspot 2: ", matched_row$hotspot_2[1]),
        paste0("Hotspot 3: ", matched_row$hotspot_3[1]),
        paste0("Hotspot 4: ", matched_row$hotspot_4[1]),
        paste0("Hotspot 5: ", matched_row$hotspot_5[1])
    )
    return(result)

}
