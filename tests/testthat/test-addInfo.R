test_that("addInfo votes complete", {
  expect_equal(nrow(addInfo(examplevotes)$voteList) == nrow(examplevotes$voteList), TRUE)
})

test_that("addInfo questions complete", {
  expect_equal(nrow(addInfo(examplequestions)$questionerList) == nrow(examplequestions$questionerList), TRUE)
})
