test_that("rice returns a data frame", {
  result <- rice(examplevotes)
  expect_s3_class(result, "data.frame")
})

test_that("rice has party and rice_mean columns", {
  result <- rice(examplevotes)
  expect_true(all(c("party", "rice_mean") %in% names(result)))
})

test_that("rice_mean is between 0 and 1", {
  result <- rice(examplevotes)
  expect_true(all(result$rice_mean >= 0 & result$rice_mean <= 1, na.rm = TRUE))
})

test_that("rice excludes parties below minvotes threshold", {
  result_low  <- rice(examplevotes, minvotes = 1)
  result_high <- rice(examplevotes, minvotes = 999)
  expect_gte(nrow(result_low), nrow(result_high))
})

test_that("rice one row per party", {
  result <- rice(examplevotes)
  expect_equal(nrow(result), length(unique(result$party)))
})

test_that("rice gives the hand-computed score when a party splits", {
  x <- examplevotes
  # Let CDA split 30 against 11 on the first vote; all other CDA votes are unanimous
  cda <- which(x$votePerParty$party == "CDA")
  x$votePerParty[cda[1], c("1", "0")] <- c(30, 11)
  expected <- mean(c(abs(30 - 11) / (30 + 11), rep(1, length(cda) - 1)))
  result <- rice(x)
  expect_equal(result$rice_mean[result$party == "CDA"], expected)
  expect_equal(result$rice_mean[result$party == "PvdA"], 1)
})

test_that("rice accepts vote_0/vote_1/vote_8 column names", {
  x <- examplevotes
  cda <- which(x$votePerParty$party == "CDA")
  x$votePerParty[cda[1], c("1", "0")] <- c(30, 11)
  y <- x
  names(y$votePerParty)[match(c("1", "0", "8"), names(y$votePerParty))] <-
    c("vote_1", "vote_0", "vote_8")
  expect_equal(rice(y), rice(x))
})
