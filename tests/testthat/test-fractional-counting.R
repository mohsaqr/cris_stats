test_that("compute_fractional_counts requires correct columns", {
  bad_data <- data.frame(x = 1:3, y = 4:6)

  expect_error(
    compute_fractional_counts(bad_data),
    "missing required columns"
  )
})

test_that("compute_fractional_counts calculates standard correctly", {
  # Simple case: 2 authors, each should get 0.5
  data <- data.frame(
    publication_id = c(1, 1),
    Authorsclean = c("Author A", "Author B"),
    stringsAsFactors = FALSE
  )

  result <- compute_fractional_counts(data, counting_methods = "standard")

  expect_true("Standard_Fractional" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_equal(sum(result$Standard_Fractional), 1, tolerance = 0.001)
})

test_that("compute_fractional_counts calculates harmonic correctly", {
  # 2 authors: first gets 1/(1/(1)+1/(2)) * 1 = 2/3, second gets 1/3
  data <- data.frame(
    publication_id = c(1, 1),
    Authorsclean = c("Author A", "Author B"),
    stringsAsFactors = FALSE
  )

  result <- compute_fractional_counts(data, counting_methods = "harmonic")

  expect_true("Harmonic_Fractional" %in% names(result))
  # First author should get more than second
  author_a <- result$Harmonic_Fractional[result$Authorsclean == "Author A"]
  author_b <- result$Harmonic_Fractional[result$Authorsclean == "Author B"]
  expect_true(author_a > author_b)
})

test_that("compute_fractional_counts handles multiple methods", {
  data <- data.frame(
    publication_id = c(1, 1, 2, 2),
    Authorsclean = c("A", "B", "A", "C"),
    stringsAsFactors = FALSE
  )

  result <- compute_fractional_counts(
    data,
    counting_methods = c("standard", "harmonic", "geometric")
  )

  expect_true("Standard_Fractional" %in% names(result))
  expect_true("Harmonic_Fractional" %in% names(result))
  expect_true("Geometric_Fractional" %in% names(result))
})

test_that("compute_fractional_counts sums to number of publications", {
  data <- data.frame(
    publication_id = c(1, 1, 1, 2, 2),
    Authorsclean = c("A", "B", "C", "A", "D"),
    stringsAsFactors = FALSE
  )

  result <- compute_fractional_counts(data, counting_methods = "standard")

  # Sum should equal number of unique publications (2)
  expect_equal(sum(result$Standard_Fractional), 2, tolerance = 0.001)
})
