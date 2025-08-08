# Declare variables
utils::globalVariables(c("Emissions", "Scope", "fraction", "label", "label_pos", "ymax", "ymin"))

# Load required libraries
library(ggplot2)
library(dplyr)

#' Plot SME Emissions
#'
#' Plot a donut chart of Scope 1 and Scope 2 emissions
#'
#' @importFrom dplyr mutate %>%
#' @importFrom utils head
#' @importFrom ggplot2 ggplot aes geom_rect scale_fill_manual geom_text coord_polar xlim theme_void theme element_rect element_text annotate labs
#' @param scope1_emissions Numeric value for Scope 1 emissions.
#' @param scope2_emissions Numeric value for Scope 2 emissions.
#' @param company_name Optional character string for the company name to include in the chart title.
#' @return A ggplot2 donut chart.
#' @export
#' @examples
#' scope_1 = sme_scope1(85, 12000000)
#' scope_2 = sme_scope2(85, 12000000)
#' plot_sme_emissions(scope1_emissions = scope_1$predicted_emissions, scope2_emissions = scope_2$predicted_emissions, company_name = "ABC")
plot_sme_emissions <- function(scope1_emissions, scope2_emissions, company_name = NULL) {

  emissions <- data.frame(
    Scope = c("Scope 1", "Scope 2"),
    Emissions = c(as.numeric(scope1_emissions), as.numeric(scope2_emissions))
  )

  emissions <- emissions %>%
    mutate(
      fraction = Emissions / sum(Emissions),
      ymax = cumsum(fraction),
      ymin = c(0, head(ymax, n = -1)),
      label_pos = (ymax + ymin) / 2,
      label = paste0(Scope, "\n", format(round(Emissions, 0), big.mark = ","))
    )

  total_emissions <- sum(emissions$Emissions)

  chart_title <- if (!is.null(company_name) && nzchar(company_name)) {
    paste0("SME Emission Profile Chart: ", company_name)
  } else {
    "SME Emission Profile Chart"
  }

  ggplot(emissions, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Scope)) +
    geom_rect() +
    scale_fill_manual(values = c("Scope 1" = "#2ecc40", "Scope 2" = "#b6eabf")) + # Green and light green
    geom_text(
      aes(
        x = 3.5,
        y = label_pos,
        label = paste0(label, " Co2e")
      ),
      size = 3,
      fontface = "bold"
    ) +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.5)
    ) +
    annotate(
      "text",
      x = 3,  # Center of the donut
      y = 0,
      label = paste("Total\n", format(round(total_emissions, 0), big.mark = ","), "Co2e"),
      size = 3,
      fontface = "bold",
      vjust = 3
    ) +
    labs(title = chart_title, fill = "Emission Scope (Co2e)")
}