test_that("addInfo votes complete", {
  expect_equal(nrow(addInfo(examplevotes)$voteList) == nrow(examplevotes$voteList), TRUE)
})

test_that("addInfo questions complete", {
  expect_equal(nrow(addInfo(examplequestions)$questionerList) == nrow(examplequestions$questionerList), TRUE)
})

# addCabinetInfo ----------------------------------------------------------

test_that("addCabinetInfo adds cabinet and election columns to voteList metaList", {
  result <- addCabinetInfo(examplevotes)
  expect_true(all(c(
    "cabinet_name", "cabinet_name_parlementcom", "caretaker",
    "cabinet_resigned", "term_start", "election_date"
  ) %in% names(result$metaList)))
  expect_equal(nrow(result$metaList), nrow(examplevotes$metaList))
})

test_that("addCabinetInfo assigns the correct cabinet for January 2010 votes", {
  result <- addCabinetInfo(examplevotes)
  expect_equal(unique(as.character(result$metaList$cabinet_name)), "Balkenende V")
  expect_equal(unique(result$metaList$cabinet_name_parlementcom), "Balkenende IV")
  expect_equal(unique(result$metaList$caretaker), 0)
  expect_equal(unique(result$metaList$cabinet_resigned), 0)
  expect_equal(unique(result$metaList$term_start), as.Date("2006-11-30"))
  expect_equal(unique(result$metaList$election_date), as.Date("2006-11-22"))
})

test_that("addCabinetInfo returns an ordered cabinet_name factor", {
  result <- addCabinetInfo(examplevotes)
  expect_true(is.ordered(result$metaList$cabinet_name))
})

test_that("addCabinetInfo warns and does nothing when run twice", {
  once <- addCabinetInfo(examplevotes)
  expect_warning(twice <- addCabinetInfo(once), "already included")
  expect_identical(twice, once)
})

test_that("addCabinetInfo on questionList uses the response date", {
  result <- addCabinetInfo(examplequestions)
  expect_equal(nrow(result$metaList), nrow(examplequestions$metaList))
  expected <- ifelse(result$metaList$dateResponse >= as.Date("2010-02-23"),
    "Balkenende VI", "Balkenende V"
  )
  expect_equal(as.character(result$metaList$cabinet_name), expected)
  expect_equal(unique(result$metaList$term_start), as.Date("2006-11-30"))
})


# addPartyInfo ------------------------------------------------------------

party_cols <- c("cabinet_party", "prime_minister", "vote_share", "seat_share", "party_seats")

test_that("addPartyInfo adds party columns to all voteList sub-tables", {
  result <- addPartyInfo(examplevotes)
  for (tbl in c("voteList", "sponsorList", "votePerParty")) {
    expect_true(all(party_cols %in% names(result[[tbl]])), label = tbl)
    expect_equal(nrow(result[[tbl]]), nrow(examplevotes[[tbl]]), label = tbl)
  }
})

test_that("addPartyInfo gives correct party values for January 2010", {
  vpp <- addPartyInfo(examplevotes)$votePerParty
  cda <- unique(vpp[vpp$party == "CDA", party_cols])
  expect_equal(nrow(cda), 1)
  expect_equal(cda$cabinet_party, 1)
  expect_equal(cda$prime_minister, 1)
  expect_equal(cda$party_seats, 41)

  pvda <- unique(vpp[vpp$party == "PvdA", party_cols])
  expect_equal(pvda$cabinet_party, 1)
  expect_equal(pvda$prime_minister, 0)
  expect_equal(pvda$party_seats, 33)

  pvv <- unique(vpp[vpp$party == "PVV", party_cols])
  expect_equal(pvv$cabinet_party, 0)
  expect_equal(pvv$party_seats, 9)
})

test_that("addPartyInfo seats match partyElectionInfo", {
  vpp <- addPartyInfo(examplevotes)$votePerParty
  pei <- examplevotes$partyElectionInfo
  pei <- pei[pei$term_start == as.Date("2006-11-30"), c("party", "seats")]
  merged <- merge(unique(vpp[, c("party", "party_seats")]), pei, by = "party")
  expect_gt(nrow(merged), 0)
  expect_equal(merged$party_seats, merged$seats)
})

test_that("addPartyInfo sponsorList is matched on sponsorParty", {
  sl <- addPartyInfo(examplevotes)$sponsorList
  expect_true(all(sl$cabinet_party[sl$sponsorParty == "CDA"] == 1))
  expect_true(all(sl$cabinet_party[sl$sponsorParty == "SP"] == 0))
})

test_that("addPartyInfo includetype = 'all' adds partyInfo columns", {
  result <- addPartyInfo(examplevotes, includetype = "all")
  expect_true(all(c("left_right", "family_name") %in% names(result$voteList)))
  expect_false("left_right" %in% names(addPartyInfo(examplevotes)$voteList))
})

test_that("addPartyInfo addto only changes the requested sub-tables", {
  result <- addPartyInfo(examplevotes, addto = "votePerParty")
  expect_true(all(party_cols %in% names(result$votePerParty)))
  expect_false(any(party_cols %in% names(result$voteList)))
  expect_false(any(party_cols %in% names(result$sponsorList)))
})

test_that("addPartyInfo on questionList adds columns to questioner and responder lists", {
  result <- addPartyInfo(examplequestions)
  for (tbl in c("questionerList", "responderList")) {
    expect_true(all(party_cols %in% names(result[[tbl]])), label = tbl)
    expect_equal(nrow(result[[tbl]]), nrow(examplequestions[[tbl]]), label = tbl)
  }
  expect_true("dateQuestion" %in% names(result$questionerList))
  expect_true("dateResponse" %in% names(result$responderList))
})

test_that("addInfo equals addCabinetInfo followed by addPartyInfo", {
  expect_identical(addInfo(examplevotes), addPartyInfo(addCabinetInfo(examplevotes)))
  expect_identical(addInfo(examplequestions), addPartyInfo(addCabinetInfo(examplequestions)))
})
