test_that("get_unit_abbreviations returns named vector", {
  abbrevs <- get_unit_abbreviations()

  expect_type(abbrevs, "character")
  expect_true(length(abbrevs) > 0)
  expect_false(is.null(names(abbrevs)))
})

test_that("get_unit_abbreviations includes key UEF units", {
  abbrevs <- get_unit_abbreviations()

  expect_true("School of Computing" %in% names(abbrevs))
  expect_true("Business School" %in% names(abbrevs))
  expect_true("University of Eastern Finland" %in% names(abbrevs))
})

test_that("get_unit_abbreviations accepts custom abbreviations", {
  custom <- c("My Unit" = "MU", "School of Computing" = "SoC")
  abbrevs <- get_unit_abbreviations(custom = custom)

  expect_equal(abbrevs["My Unit"], c("My Unit" = "MU"))
  expect_equal(abbrevs["School of Computing"], c("School of Computing" = "SoC"))
})

test_that("abbreviate_units works correctly", {
  units <- c("School of Computing", "Business School")
  result <- abbreviate_units(units)

  expect_equal(result[1], "Computing")
  expect_equal(result[2], "Business")
})

test_that("abbreviate_units handles unknown units", {
  units <- c("School of Computing", "Unknown Unit XYZ")

  # Keep unknown
  result <- abbreviate_units(units, keep_unknown = TRUE)
  expect_equal(result[2], "Unknown Unit XYZ")

  # Don't keep unknown
  result <- abbreviate_units(units, keep_unknown = FALSE)
  expect_true(is.na(result[2]))
})

test_that("add_unit_column adds new column", {
  df <- data.frame(
    first_authors_unit = c("School of Computing", "Business School"),
    stringsAsFactors = FALSE
  )

  result <- add_unit_column(df)

  expect_true("unit_abbrev" %in% names(result))
  expect_equal(result$unit_abbrev[1], "Computing")
})
