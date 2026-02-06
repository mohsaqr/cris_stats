test_that("clean_author_data requires authors column", {
  bad_data <- data.frame(x = 1:3, y = 4:6)

  expect_error(
    clean_author_data(bad_data),
    "missing required columns"
  )
})

test_that("clean_author_data splits semicolon-separated authors", {
  data <- data.frame(
    authors = c("Smith, John; Doe, Jane"),
    stringsAsFactors = FALSE
  )

  result <- clean_author_data(data)

  expect_true(nrow(result) >= 2)
})

test_that("clean_author_data creates Authorsclean column", {
  data <- data.frame(
    authors = c("Smith, John"),
    stringsAsFactors = FALSE
  )

  result <- clean_author_data(data)

  expect_true("Authorsclean" %in% names(result))
})

test_that("clean_author_data removes Jr. suffix", {
  data <- data.frame(
    authors = c("Smith Jr., John"),
    stringsAsFactors = FALSE
  )

  result <- clean_author_data(data)

  expect_false(grepl("Jr\\.", result$Authorsclean[1], ignore.case = TRUE))
})

test_that("clean_author_data removes year patterns", {
  data <- data.frame(
    authors = c("(2024). Smith, John"),
    stringsAsFactors = FALSE
  )

  result <- clean_author_data(data)

  expect_false(grepl("2024", result$Authorsclean[1]))
})

test_that("clean_author_data excludes ET AL by default", {
  data <- data.frame(
    authors = c("Smith, John; ET AL"),
    stringsAsFactors = FALSE
  )

  result <- clean_author_data(data, exclude_et_al = TRUE)

  expect_false("ET AL" %in% result$Authorsclean)
})

test_that("clean_author_data can keep ET AL", {
  data <- data.frame(
    authors = c("Smith, John; ET AL"),
    stringsAsFactors = FALSE
  )

  result <- clean_author_data(data, exclude_et_al = FALSE)

  expect_true("ET AL" %in% result$Authorsclean)
})
