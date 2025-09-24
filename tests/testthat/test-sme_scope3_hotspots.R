library(testthat)
library(carbonpredict)

test_that("errors for invalid SIC code", {
	expect_error(sme_scope3_hotspots(999), "Please provide a valid 2-digit SIC code")
})

test_that("returns correct hotspots for SIC code 2", {
	result <- sme_scope3_hotspots(2)
	expected <- data.frame(
		Hotspot = 1:5,
		Description = c(
			"Business & Industrial > Agriculture & Forestry > Forestry",
			"Business & Industrial > Agriculture & Forestry",
			"Travel > Bus & Rail",
			"Business & Industrial > Business Services > Office Supplies",
			"Autos & Vehicles > Vehicle Parts & Services > Gas Prices & Vehicle Fueling"
		),
		stringsAsFactors = FALSE
	)
	expect_equal(result, expected)
})

test_that("returns correct hotspots for SIC code 85", {
	result <- sme_scope3_hotspots(85)
	expected <- data.frame(
		Hotspot = 1:5,
		Description = c(
			"Jobs & Education > Education > Primary & Secondary Schooling (K-12)",
			"Jobs & Education > Education",
			"Shopping",
			"Food & Drink > Food & Grocery Retailers",
			"Travel > Bus & Rail"
		),
		stringsAsFactors = FALSE
	)
	expect_equal(result, expected)
})

test_that("returns correct hotspots for SIC code 10", {
	result <- sme_scope3_hotspots(10)
	expected <- data.frame(
		Hotspot = 1:5,
		Description = c(
			"Food & Drink > Food & Grocery Retailers",
			"Food & Drink > Food > Baked Goods",
			"Food & Drink > Food",
			"Food & Drink > Food > Meat & Seafood",
			"Food & Drink > Food > Candy & Sweets"
		),
		stringsAsFactors = FALSE
	)
	expect_equal(result, expected)
})
