test_that("number of rollcall votes correct", {
  expect_equal(as.rollcall(examplevotes)$m == nrow(examplevotes$metaList), TRUE)
})

test_that("can create rollcall object", {
  expect_equal(class(as.rollcall(examplevotes)), "rollcall")
})

test_that("rollcall legislators are the parties in voteMatrix", {
  rc <- as.rollcall(examplevotes)
  parties <- setdiff(names(examplevotes$voteMatrix), c("id", "(Unknown)"))
  expect_equal(rownames(rc$votes), parties)
  expect_equal(rc$n, length(parties))
})

test_that("rollcall vote names are the vote ids", {
  rc <- as.rollcall(examplevotes)
  expect_equal(colnames(rc$votes), examplevotes$voteMatrix$id)
})

test_that("rollcall codes follow the defaults and can be overridden", {
  rc <- as.rollcall(examplevotes)
  expect_equal(rc$codes$yea, 1)
  expect_equal(rc$codes$nay, 0)
  expect_equal(rc$codes$missing, c(3, 7, 8))
  expect_equal(rc$codes$notInLegis, 9)

  rc2 <- as.rollcall(examplevotes, missing = 8)
  expect_equal(rc2$codes$missing, 8)
})

test_that("rollcall vote data comes from metaList", {
  rc <- as.rollcall(examplevotes)
  expect_equal(nrow(rc$vote.data), nrow(examplevotes$metaList))
  expect_equal(rc$vote.data$id, examplevotes$metaList$id)
})
