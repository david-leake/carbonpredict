
       ______           __                   ____                ___      __ 
      / ____/___ ______/ /_  ____  ____     / __ \________  ____/ (_)____/ /_
     / /   / __ `/ ___/ __ \/ __ \/ __ \   / /_/ / ___/ _ \/ __  / / ___/ __/
    / /___/ /_/ / /  / /_/ / /_/ / / / /  / ____/ /  /  __/ /_/ / / /__/ /_  
    \____/\__,_/_/  /_.___/\____/_/ /_/  /_/   /_/   \___/\__,_/_/\___/\__/  
                                                           ~ Hamza Suleman

# Carbon Predict

Carbon Predict is an R package for predicting Scope 1 and Scope 2 carbon
emissions for UK SMEs using SIC codes and turnover data. It provides
batch prediction, plotting, and workflow tools for carbon accounting and
reporting. The package utilises pre-trained models, leveraging rich
classified transaction data to accurately predict Scope 1 and 2
emissions for UK SMEs.

The methodology used to produce the estimates in this package is fully
detailed in the following peer-reviewed publication:

Phillpotts, A., Owen. A., Norman, J., Trendl, A., Gathergood, J., Jobst,
Norbert., Leake, D., 2025. Bridging the SME Reporting Gap: A New Model
for Predicting Scope 1 and 2 Emissions. Journal of Industrial Ecology.

## Installation

You can install the package from CRAN *(coming soon!)*:

``` r
install.packages("carbonpredict")
```

Or install the development version from GitHub:

``` r
# Clone the repository
# git clone https://github.com/david-leake/carbonpredict.git
# Then install locally
install.packages("devtools")
devtools::install_local("carbonpredict")
```

## Usage

### Predict emissions for a single SME

``` r
library(carbonpredict)
sme_scope1(85, 12000000)
sme_scope2(85, 12000000)
# Note: all predicted emissions values are in tonnes of Co2e (tCo2e).
```

### Predict SME scope 3 emissions categories and hotspots

``` r
sme_scope3(85, 12000000)
sme_scope3_hotspots(85)
```

### Plot emissions for a single SME

``` r
# Scope 1 and 2
scp1 <- sme_scope1(85, 12000000)
scp2 <- sme_scope2(85, 12000000)
plot_sme_emissions(scp1$`Predicted Emissions (tCO2e)`, scp2$`Predicted Emissions (tCO2e)`, "Carbon Predict LTD")

# Scope 3
scp3 <- sme_scope(85, 12000000)
plot_scope3_emissions(scp3, "Carbon Predict LTD")
```

### Get a full emissions profile and plots

``` r
sme_emissions_profile(85, 12000000, "Carbon Predict LTD")
```

### Batch prediction from CSV

``` r
# Some sample SME data is included in the package for demonstration purposes.
sample_data <- system.file("extdata", "sme_examples.csv", package = "carbonpredict")
results <- batch_predict_emissions(data = sample_data, company_type = "sme", output_path = "temp/results.csv")
```

### Batch plotting

``` r
sample_data <- system.file("extdata", "sme_examples.csv", package = "carbonpredict")
batch_sme_plots(data = sample_data, output_path = "temp/plots")
```

## Documentation

Full documentation is available on [our GitHub Pages
site](https://david-leake.github.io/carbonpredict/index.html), in the
[package manual
(PDF)](https://github.com/david-leake/carbonpredict/blob/main/carbonpredict_documentation.pdf),
and via R help pages (e.g., `?sme_scope1`).

## Contributing

Pull requests and issues are welcome!

## License

MIT
