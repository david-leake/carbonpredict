
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
<https://doi.org/>

## Installation

You can install the package from CRAN:

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
```

### Plot emissions for a single SME

``` r
scp1 <- sme_scope1(85, 12000000)
scp2 <- sme_scope2(85, 12000000)
plot_sme_emissions(scp1$predicted_emissions, scp2$predicted_emissions, "Carbon Predict LTD")
```

### Get a full emissions profile and plot

``` r
sme_emissions_profile(85, 12000000, "Carbon Predict LTD")
```

### Batch prediction from CSV

``` r
# Some example SME data is included in the package
sample_data <- system.file("extdata", "sme_examples.csv", package = "carbonpredict")
results <- batch_predict_emissions(data = sample_data, company_type = "sme", output_path = "temp/results.csv")
```

### Batch plotting

``` r
sample_data <- system.file("extdata", "sme_examples.csv", package = "carbonpredict")
batch_sme_plots(data = sample_data, output_path = "temp/plots")
```

## Documentation

Full documentation is available [here](reference/index.html) and via R
help pages (e.g., `?sme_scope1`).

## Contributing

Pull requests and issues are welcome!

## License

MIT
