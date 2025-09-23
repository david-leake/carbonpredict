library(testthat)
library(carbonpredict)

# Test: errors for invalid SIC code
test_that("errors for invalid SIC code", {
	expect_error(sme_scope3_hotspots(999), "Please provide a valid 2-digit SIC code")
})

# Test: correct output for SIC code 2 (Forestry and logging)
test_that("returns correct hotspots for SIC code 2", {
	result <- sme_scope3_hotspots(2)
	expected <- c(
		"Industry: Forestry and logging",
		"Top 5 Hotspots:",
		"Hotspot 1: Business & Industrial > Agriculture & Forestry > Forestry",
		"Hotspot 2: Business & Industrial > Agriculture & Forestry",
		"Hotspot 3: Travel > Bus & Rail",
		"Hotspot 4: Business & Industrial > Business Services > Office Supplies",
		"Hotspot 5: Autos & Vehicles > Vehicle Parts & Services > Gas Prices & Vehicle Fueling"
	)
	expect_equal(trimws(result), trimws(expected))
})

# Test: correct output for SIC code 85 (Education)
test_that("returns correct hotspots for SIC code 85", {
	result <- sme_scope3_hotspots(85)
	expected <- c(
		"Industry: Education",
		"Top 5 Hotspots:",
		"Hotspot 1: Jobs & Education > Education > Primary & Secondary Schooling (K-12)",
		"Hotspot 2: Jobs & Education > Education",
		"Hotspot 3: Shopping",
		"Hotspot 4: Food & Drink > Food & Grocery Retailers",
		"Hotspot 5: Travel > Bus & Rail"
	)
	expect_equal(trimws(result), trimws(expected))
})

# Test: correct output for SIC code 10 (Manufacture of food products)
test_that("returns correct hotspots for SIC code 10", {
	result <- sme_scope3_hotspots(10)
	expected <- c(
		"Industry: Manufacture of food products",
		"Top 5 Hotspots:",
		"Hotspot 1: Food & Drink > Food & Grocery Retailers",
		"Hotspot 2: Food & Drink > Food > Baked Goods",
		"Hotspot 3: Food & Drink > Food",
		"Hotspot 4: Food & Drink > Food > Meat & Seafood",
		"Hotspot 5: Food & Drink > Food > Candy & Sweets"
	)
	expect_equal(trimws(result), trimws(expected))
})
