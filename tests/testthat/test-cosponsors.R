test_that("cosponsors returns a data frame", {
  result <- cosponsors(examplevotes)
  expect_s3_class(result, "data.frame")
})

test_that("cosponsors has expected columns at MP level", {
  result <- cosponsors(examplevotes)
  expect_named(result, c(
    "MP1.id", "MP1.name", "MP1.party",
    "MP2.id", "MP2.name", "MP2.party",
    "nCosponsor", "totalSponsor", "percCosponsor"
  ), ignore.order = TRUE)
})

test_that("cosponsors diagonal equals totalSponsor at MP level", {
  result <- cosponsors(examplevotes)
  self <- result[result$MP1.id == result$MP2.id & result$MP1.party == result$MP2.party, ]
  expect_true(all(self$nCosponsor == self$totalSponsor))
  expect_true(all(self$percCosponsor == 1))
})

test_that("cosponsors percCosponsor is between 0 and 1", {
  result <- cosponsors(examplevotes)
  expect_true(all(result$percCosponsor >= 0 & result$percCosponsor <= 1, na.rm = TRUE))
})

test_that("cosponsors nCosponsor values are non-negative", {
  result <- cosponsors(examplevotes)
  expect_true(all(result$nCosponsor >= 0))
})

test_that("cosponsors partylevel = TRUE returns data frame with party columns", {
  result <- cosponsors(examplevotes, partylevel = TRUE)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("Party1", "Party2", "nCosponsor", "totalSponsor", "percCosponsor"),
               ignore.order = TRUE)
})

test_that("cosponsors partylevel diagonal equals totalSponsor", {
  result <- cosponsors(examplevotes, partylevel = TRUE)
  self <- result[result$Party1 == result$Party2, ]
  expect_true(all(self$nCosponsor == self$totalSponsor))
  expect_true(all(self$percCosponsor == 1))
})

test_that("cosponsors partylevel percCosponsor is between 0 and 1", {
  result <- cosponsors(examplevotes, partylevel = TRUE)
  expect_true(all(result$percCosponsor >= 0 & result$percCosponsor <= 1, na.rm = TRUE))
})

test_that("cosponsors nCosponsor equals the number of shared proposals", {
  result <- cosponsors(examplevotes)
  sl <- examplevotes$sponsorList
  ids_of <- split(sl$id, paste(sl$sponsorId, sl$sponsorParty))
  key1 <- paste(result$MP1.id, result$MP1.party)
  key2 <- paste(result$MP2.id, result$MP2.party)
  shared <- mapply(\(a, b) length(intersect(ids_of[[a]], ids_of[[b]])), key1, key2)
  expect_equal(result$nCosponsor, unname(shared))
})

test_that("cosponsors partylevel nCosponsor equals the number of shared proposals", {
  result <- cosponsors(examplevotes, partylevel = TRUE)
  sl <- examplevotes$sponsorList
  ids_of <- split(sl$id, sl$sponsorParty)
  shared <- mapply(
    \(a, b) length(intersect(ids_of[[a]], ids_of[[b]])),
    result$Party1, result$Party2
  )
  expect_equal(result$nCosponsor, unname(shared))
})

test_that("cosponsors output is sorted by first MP/party name (C locale)", {
  result <- cosponsors(examplevotes)
  expect_equal(result$MP1.name, sort(result$MP1.name, method = "radix"))
  result_party <- cosponsors(examplevotes, partylevel = TRUE)
  expect_equal(result_party$Party1, sort(result_party$Party1, method = "radix"))
})
