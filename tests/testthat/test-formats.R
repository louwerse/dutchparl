# Every exported function should work on both data layouts: REPDEM
# (examplevotes) and ParlGov (examplevotes_legacy). The votes themselves are
# identical in both objects, so vote-based results should be identical too.

for (fmt in names(example_votelists)) {
  ev <- example_votelists[[fmt]]
  tb <- as_tibble_object(ev)

  test_that(paste("vote-based functions run -", fmt), {
    expect_s3_class(summary(ev), "summary.voteList")
    expect_output(print(summary(ev)))
    expect_no_error(print(ev))
    expect_s3_class(rice(ev), "data.frame")
    expect_s3_class(as.rollcall(ev), "rollcall")
    expect_true(is.matrix(votesame(ev)))
    expect_s3_class(cosponsors(ev), "data.frame")
    expect_s3_class(cosponsors(ev, partylevel = TRUE), "data.frame")
    expect_equal(nrow(randomvotes(ev, size = 5)$metaList), 5)
  })

  test_that(paste("functions give the same values on tibbles -", fmt), {
    expect_equal(as.data.frame(addCabinetInfo(tb)$metaList),
      as.data.frame(addCabinetInfo(ev)$metaList),
      ignore_attr = TRUE
    )
    res_tb <- addInfo(tb)
    res_df <- addInfo(ev)
    for (tbl in c("voteList", "sponsorList", "votePerParty")) {
      expect_equal(as.data.frame(res_tb[[tbl]]), as.data.frame(res_df[[tbl]]),
        ignore_attr = TRUE, label = tbl
      )
    }
    expect_equal(rice(tb), rice(ev))
    expect_equal(votesame(tb), votesame(ev))
  })

  test_that(paste("filter and subset keep all sub-tables in sync -", fmt), {
    f <- dplyr::filter(ev, file == "31813")
    s <- subset(ev, ev$metaList, file == "31813")
    for (res in list(f, s)) {
      expect_s3_class(res, class(ev)[1])
      ids <- res$metaList$id
      expect_equal(nrow(res$metaList), 11)
      for (tbl in c("voteList", "voteMatrix", "votePerParty", "sponsorList", "categoryList", "voteCabinet")) {
        if (!is.null(ev[[tbl]])) {
          expect_true(all(res[[tbl]]$id %in% ids), label = tbl)
          expect_setequal(unique(res[[tbl]]$id), intersect(ids, ev[[tbl]]$id))
        }
      }
    }
    # cabinet information on a filtered object equals that of the full object
    full <- addCabinetInfo(ev)$metaList
    part <- addCabinetInfo(f)$metaList
    expect_equal(
      as.data.frame(part),
      as.data.frame(full[full$id %in% part$id, ]),
      ignore_attr = TRUE
    )
  })
}

test_that("vote-based results are identical across layouts", {
  expect_equal(rice(examplevotes), rice(examplevotes_legacy))
  expect_equal(votesame(examplevotes), votesame(examplevotes_legacy))
  expect_equal(as.rollcall(examplevotes)$votes, as.rollcall(examplevotes_legacy)$votes)
  expect_equal(cosponsors(examplevotes), cosponsors(examplevotes_legacy))
})

test_that("filter.voteList can filter on voteCabinet for REPDEM data", {
  f <- dplyr::filter(examplevotes, cabinet_period_id == "12026-1", .table = "voteCabinet")
  expect_equal(nrow(f$metaList), nrow(examplevotes$metaList))
  f0 <- dplyr::filter(examplevotes, caretaker == 1, .table = "voteCabinet")
  expect_equal(nrow(f0$metaList), 0)
  expect_error(
    dplyr::filter(examplevotes_legacy, caretaker == 1, .table = "voteCabinet"),
    regexp = "must be one of"
  )
})
