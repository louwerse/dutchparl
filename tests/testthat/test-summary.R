test_that("summary.voteList returns a summary.voteList object", {
  result <- summary(examplevotes)
  expect_s3_class(result, "summary.voteList")
})

test_that("summary.voteList nVotes matches metaList rows", {
  result <- summary(examplevotes)
  expect_equal(result$nVotes, nrow(examplevotes$metaList))
})

test_that("summary.voteList categories is a table", {
  result <- summary(examplevotes)
  expect_true(inherits(result$categories, "table"))
})

test_that("summary.voteList parties is a character vector", {
  result <- summary(examplevotes)
  expect_type(result$parties, "character")
})

test_that("print.summary.voteList produces output without error", {
  result <- summary(examplevotes)
  expect_output(print(result))
})

test_that("print.voteList does not error", {
  expect_no_error(print(examplevotes))
})
