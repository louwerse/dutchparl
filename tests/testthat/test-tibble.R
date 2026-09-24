# The full datasets may be stored with tibbles; results should not depend on it.

tb_votes <- as_tibble_object(examplevotes)
tb_questions <- as_tibble_object(examplequestions)

test_that("helper keeps object classes", {
  expect_s3_class(tb_votes, "voteList")
  expect_s3_class(tb_votes$metaList, "tbl_df")
  expect_s3_class(tb_questions, "questionList")
})

test_that("addCabinetInfo gives the same values on tibbles", {
  expect_equal(
    as.data.frame(addCabinetInfo(tb_votes)$metaList),
    as.data.frame(addCabinetInfo(examplevotes)$metaList)
  )
  expect_equal(
    as.data.frame(addCabinetInfo(tb_questions)$metaList),
    as.data.frame(addCabinetInfo(examplequestions)$metaList)
  )
})

test_that("addPartyInfo gives the same values on tibbles", {
  res_tb <- addPartyInfo(tb_votes)
  res_df <- addPartyInfo(examplevotes)
  for (tbl in c("voteList", "sponsorList", "votePerParty")) {
    expect_equal(as.data.frame(res_tb[[tbl]]), as.data.frame(res_df[[tbl]]),
      ignore_attr = TRUE, label = tbl
    )
  }
  res_tb <- addPartyInfo(tb_questions)
  res_df <- addPartyInfo(examplequestions)
  for (tbl in c("questionerList", "responderList")) {
    expect_equal(as.data.frame(res_tb[[tbl]]), as.data.frame(res_df[[tbl]]),
      ignore_attr = TRUE, label = tbl
    )
  }
})

test_that("rice gives the same values on tibbles", {
  expect_equal(rice(tb_votes), rice(examplevotes))
})

test_that("cosponsors gives the same values on tibbles", {
  expect_equal(
    as.data.frame(cosponsors(tb_votes)),
    as.data.frame(cosponsors(examplevotes)),
    ignore_attr = TRUE
  )
  expect_equal(
    as.data.frame(cosponsors(tb_votes, partylevel = TRUE)),
    as.data.frame(cosponsors(examplevotes, partylevel = TRUE)),
    ignore_attr = TRUE
  )
})

test_that("as.rollcall and votesame give the same values on tibbles", {
  expect_equal(as.rollcall(tb_votes)$votes, as.rollcall(examplevotes)$votes)
  expect_equal(votesame(tb_votes), votesame(examplevotes))
})

test_that("filter and subset give the same ids on tibbles", {
  expect_equal(
    dplyr::filter(tb_votes, file == "31813")$metaList$id,
    dplyr::filter(examplevotes, file == "31813")$metaList$id
  )
  expect_equal(
    subset(tb_votes, tb_votes$metaList, date > as.Date("2010-01-15"))$metaList$id,
    subset(examplevotes, examplevotes$metaList, date > as.Date("2010-01-15"))$metaList$id
  )
})

test_that("functions do not warn about unknown columns on tibbles", {
  expect_no_warning(addPartyInfo(tb_votes))
  expect_no_warning(addPartyInfo(tb_questions))
  expect_no_warning(addInfo(tb_votes))
  expect_no_warning(addInfo(tb_questions))
  expect_no_warning(rice(tb_votes))
  expect_no_warning(cosponsors(tb_votes))
})
