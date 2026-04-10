test_that("filter.voteList returns a voteList", {
  f <- dplyr::filter(examplevotes, file == "31813")
  expect_s3_class(f, "voteList")
})

test_that("filter.voteList reduces rows on metaList", {
  f <- dplyr::filter(examplevotes, file == "31813")
  expect_lt(nrow(f$metaList), nrow(examplevotes$metaList))
  expect_true(all(f$metaList$file == "31813"))
})

test_that("filter.voteList keeps all sub-tables consistent with metaList ids", {
  f <- dplyr::filter(examplevotes, file == "31813")
  expect_true(all(f$voteList$id     %in% f$metaList$id))
  expect_true(all(f$votePerParty$id %in% f$metaList$id))
  expect_true(all(f$sponsorList$id  %in% f$metaList$id))
  expect_true(all(f$categoryList$id %in% f$metaList$id))
})

test_that("filter.voteList on sponsorList keeps correct ids", {
  f        <- dplyr::filter(examplevotes, sponsorParty == "VVD", .table = "sponsorList")
  expected <- unique(examplevotes$sponsorList$id[examplevotes$sponsorList$sponsorParty == "VVD"])
  expect_setequal(f$metaList$id, expected)
})

test_that("filter.voteList on categoryList keeps correct ids", {
  f        <- dplyr::filter(examplevotes, category == "Werk", .table = "categoryList")
  expected <- unique(examplevotes$categoryList$id[examplevotes$categoryList$category == "Werk"])
  expect_setequal(f$metaList$id, expected)
})

test_that("filter.voteList works with the native pipe", {
  f <- examplevotes |> dplyr::filter(file == "31989")
  expect_s3_class(f, "voteList")
  expect_true(all(f$metaList$file == "31989"))
})

test_that("filter.voteList errors clearly on invalid .table", {
  expect_error(
    dplyr::filter(examplevotes, file == "31813", .table = "badTable"),
    regexp = "must be one of"
  )
})
