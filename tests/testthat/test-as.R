test_that("number of rollcall votes correct", {
  expect_equal(as.rollcall(examplevotes)$m == nrow(examplevotes$metaList), TRUE)
})

test_that("can create rollcall object", {
  expect_equal(class(as.rollcall(examplevotes)), "rollcall")
})
