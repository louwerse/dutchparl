# subset.voteList ---------------------------------------------------------

test_that("subset.voteList returns a voteList", {
  result <- subset(examplevotes, examplevotes$metaList, date > as.Date("2010-01-15"))
  expect_s3_class(result, "voteList")
})

test_that("subset.voteList reduces rows correctly", {
  result <- subset(examplevotes, examplevotes$metaList, date > as.Date("2010-01-19"))
  expect_true(all(result$metaList$date > as.Date("2010-01-19")))
  expect_lt(nrow(result$metaList), nrow(examplevotes$metaList))
})

test_that("subset.voteList keeps sub-tables consistent", {
  result <- subset(examplevotes, examplevotes$metaList, date > as.Date("2010-01-15"))
  expect_true(all(result$voteList$id     %in% result$metaList$id))
  expect_true(all(result$votePerParty$id %in% result$metaList$id))
  expect_true(all(result$sponsorList$id  %in% result$metaList$id))
  expect_true(all(result$categoryList$id %in% result$metaList$id))
})

test_that("subset.voteList without subset arg returns full voteList", {
  result <- subset(examplevotes, examplevotes$metaList)
  expect_equal(nrow(result$metaList), nrow(examplevotes$metaList))
})

test_that("subset.voteList errors on non-logical subset expression", {
  expect_error(
    subset(examplevotes, examplevotes$metaList, subset = "bad"),
    regexp = "must evaluate to logical"
  )
})


# filter.questionList -----------------------------------------------------

test_that("filter.questionList returns a questionList", {
  result <- dplyr::filter(examplequestions, dateQuestion > as.Date("2010-01-04"))
  expect_s3_class(result, "questionList")
})

test_that("filter.questionList reduces rows on metaList", {
  result <- dplyr::filter(examplequestions, dateQuestion > as.Date("2010-01-04"))
  expect_true(all(result$metaList$dateQuestion > as.Date("2010-01-04")))
  expect_lt(nrow(result$metaList), nrow(examplequestions$metaList))
})

test_that("filter.questionList sub-tables are consistent", {
  result <- dplyr::filter(examplequestions, dateQuestion > as.Date("2010-01-04"))
  expect_true(all(result$questionerList$dcIdentifier %in% result$metaList$dcIdentifier))
  expect_true(all(result$responderList$dcIdentifier  %in% result$metaList$dcIdentifier))
  expect_true(all(result$categoryList$dcIdentifier   %in% result$metaList$dcIdentifier))
})

test_that("filter.questionList on responderList keeps correct ids", {
  parties <- unique(examplequestions$responderList$responderParty)
  party1 <- parties[1]
  result <- dplyr::filter(examplequestions, responderParty == party1, .table = "responderList")
  expected <- unique(examplequestions$responderList$dcIdentifier[
    examplequestions$responderList$responderParty == party1
  ])
  expect_setequal(result$metaList$dcIdentifier, expected)
})

test_that("filter.questionList errors on invalid .table", {
  expect_error(
    dplyr::filter(examplequestions, dateQuestion > as.Date("2010-01-04"), .table = "badTable"),
    regexp = "must be one of"
  )
})


# subset.questionList -----------------------------------------------------

test_that("subset.questionList returns a questionList", {
  result <- subset(examplequestions, examplequestions$metaList,
                   dateQuestion > as.Date("2010-01-04"))
  expect_s3_class(result, "questionList")
})

test_that("subset.questionList reduces rows correctly", {
  result <- subset(examplequestions, examplequestions$metaList,
                   dateQuestion > as.Date("2010-01-04"))
  expect_true(all(result$metaList$dateQuestion > as.Date("2010-01-04")))
  expect_lt(nrow(result$metaList), nrow(examplequestions$metaList))
})

test_that("subset.questionList sub-tables are consistent", {
  result <- subset(examplequestions, examplequestions$metaList,
                   dateQuestion > as.Date("2010-01-04"))
  expect_true(all(result$questionerList$dcIdentifier %in% result$metaList$dcIdentifier))
  expect_true(all(result$responderList$dcIdentifier  %in% result$metaList$dcIdentifier))
  expect_true(all(result$categoryList$dcIdentifier   %in% result$metaList$dcIdentifier))
})


# randomvotes -------------------------------------------------------------

test_that("randomvotes returns a voteList", {
  result <- randomvotes(examplevotes)
  expect_s3_class(result, "voteList")
})

test_that("randomvotes returns correct number of votes", {
  result <- randomvotes(examplevotes, size = 5)
  expect_equal(nrow(result$metaList), 5)
})

test_that("randomvotes returned ids are a subset of original", {
  result <- randomvotes(examplevotes, size = 5)
  expect_true(all(result$metaList$id %in% examplevotes$metaList$id))
})

test_that("randomvotes sub-tables are consistent", {
  result <- randomvotes(examplevotes, size = 5)
  expect_true(all(result$voteList$id     %in% result$metaList$id))
  expect_true(all(result$votePerParty$id %in% result$metaList$id))
})
