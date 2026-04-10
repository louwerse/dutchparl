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
