test_that("votesame returns a square matrix for voteList", {
  result <- votesame(examplevotes)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), ncol(result))
})

test_that("votesame diagonal is 1 (party always votes same as itself)", {
  result <- votesame(examplevotes)
  expect_true(all(diag(result) == 1))
})

test_that("votesame values are between 0 and 1", {
  result <- votesame(examplevotes)
  expect_true(all(result >= 0 & result <= 1, na.rm = TRUE))
})

test_that("votesame matrix is symmetric", {
  result <- votesame(examplevotes)
  expect_equal(result, t(result))
})

test_that("votesame.rollcall works on a rollcall object", {
  rc <- as.rollcall(examplevotes)
  result <- votesame(rc)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), ncol(result))
})

test_that("votesame.voteList and votesame.rollcall give same result", {
  result_vl <- votesame(examplevotes)
  result_rc <- votesame(as.rollcall(examplevotes))
  expect_equal(result_vl, result_rc)
})

test_that("votesame order.x reorders columns", {
  rc <- as.rollcall(examplevotes)
  # After transposing: rows = legislators, cols = votes
  # order.x must have length == ncol(t(rc$votes)) = n_legislators
  n_leg <- ncol(t(rc$votes))
  result_default    <- votesame(rc)
  result_reordered  <- votesame(rc, order.x = seq(n_leg, 1))
  expect_equal(dim(result_default), dim(result_reordered))
})
