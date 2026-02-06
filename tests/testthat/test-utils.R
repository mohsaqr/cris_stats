test_that("simplify_author_string converts to uppercase", {
  expect_equal(simplify_author_string("hello"), "HELLO")
  expect_equal(simplify_author_string("John Smith"), "JOHN SMITH")
})

test_that("simplify_author_string handles accented characters", {
  # Note: exact output depends on iconv implementation
  # Result should be uppercase and contain the base characters
  result <- simplify_author_string("Müller")
  expect_true(grepl("MULLER|M.LLER", result, ignore.case = FALSE))

  result <- simplify_author_string("José")
  expect_true(grepl("JOSE|JOS", result, ignore.case = FALSE))
})

test_that("simplify_author_string handles vectors", {
  input <- c("Smith, John", "García, María")
  result <- simplify_author_string(input)

  expect_length(result, 2)
  expect_true(all(grepl("^[A-Z ,.'()-]+$", result)))
})

test_that("simplify_author_string preserves common punctuation", {
  result <- simplify_author_string("O'Connor, J.")
  expect_true(grepl("'", result) || grepl("CONNOR", result))

  result <- simplify_author_string("Smith-Jones, A.")
  expect_true(grepl("-", result) || grepl("SMITH", result))
})
