# Load required libraries
library(networkD3)

#' Plot Scope 3 Emissions Breakdown
#'
#' Plots a Sankey diagram showing the breakdown of Scope 3 emissions by category.
#'
#' @param scope3_df Data frame output from sme_scope3 (must contain 'Category', 'Description', and 'Predicted Emissions (tCO2e)').
#' @param company_name Optional company name to include in the chart title (character string).
#' @return A Sankey plot showing a breakdown for predicted emissions of each Scope 3 category.
#' @export
#' @examples
#' scope3_df <- sme_scope3(85, 12000000)
#' plot_scope3_emissions(scope3_df, company_name = "ABC")
plot_scope3_emissions <- function(scope3_df, company_name = NULL) {

  # Extract total emissions from the 'Total' row
  total_row <- scope3_df[scope3_df$Category == "Total", ]
  total_emissions <- if (nrow(total_row) > 0) total_row$`Predicted Emissions (tCO2e)` else sum(scope3_df$`Predicted Emissions (tCO2e)`, na.rm = TRUE)

  # Remove the 'Total' row and filter valid rows
  plot_data <- scope3_df[scope3_df$Category != "Total", ]
  plot_data <- plot_data[!is.na(plot_data$`Predicted Emissions (tCO2e)`) & plot_data$`Predicted Emissions (tCO2e)` > 0, ]

  # Create nodes and links with values in labels, trimming whitespace
  nodes <- data.frame(
    name = paste0(trimws(plot_data$Description), " (", trimws(format(round(plot_data$`Predicted Emissions (tCO2e)`, 2), big.mark = ",")), ")"),
    stringsAsFactors = FALSE
  )
  # Add total to Scope 3 node, with unit
  nodes <- rbind(
    data.frame(name = paste0("Scope 3 (", trimws(format(round(total_emissions, 2), big.mark = ",")), " tCO2e)"), stringsAsFactors = FALSE),
    nodes
  )
  links <- data.frame(
    source = 0, # All from a single source node
    target = seq_len(nrow(plot_data)),
    value = plot_data$`Predicted Emissions (tCO2e)`
  )
  chart_title <- if (!is.null(company_name) && nzchar(company_name)) {
    paste0("Scope 3 Emissions Breakdown: ", company_name)
  } else {
    "Scope 3 Emissions Breakdown"
  }

  # Non-interactive: show static image
  if (!interactive()) {
    library(png)
    library(grid)
    img_path <- system.file("intdata", "scope3_sankey_example.png", package = "carbonpredict")
    if (file.exists(img_path)) {
      img <- png::readPNG(img_path)
      grid::grid.raster(img)
      return(invisible(NULL))
    } else {
      warning("Static Sankey image not found.")
      return(invisible(NULL))
    }
  }

  sankey <- sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    fontSize = 12,
    nodeWidth = 30,
    sinksRight = FALSE
  )
  sankey <- htmlwidgets::prependContent(sankey, htmltools::tags$h3(chart_title))
  return(sankey)
}
