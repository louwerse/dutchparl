# Structural tests, for both data layouts ----------------------------------

for (fmt in names(example_votelists)) {
  ev <- example_votelists[[fmt]]

  test_that(paste("addInfo keeps row counts -", fmt), {
    res <- addInfo(ev)
    for (tbl in c("metaList", "voteList", "sponsorList", "votePerParty")) {
      expect_equal(nrow(res[[tbl]]), nrow(ev[[tbl]]), label = tbl)
    }
  })

  test_that(paste("addCabinetInfo adds cabinet and election columns -", fmt), {
    result <- addCabinetInfo(ev)
    expect_true(all(c(
      "cabinet_name", "cabinet_name_parlementcom", "caretaker",
      "cabinet_resigned", "term_start", "election_date"
    ) %in% names(result$metaList)))
    expect_equal(nrow(result$metaList), nrow(ev$metaList))
    expect_true(is.ordered(result$metaList$cabinet_name))
    expect_false(anyNA(result$metaList$cabinet_name))
    expect_equal(unique(result$metaList$caretaker), 0)
    expect_equal(unique(result$metaList$cabinet_resigned), 0)
    expect_equal(unique(result$metaList$term_start), as.Date("2006-11-30"))
    expect_equal(unique(result$metaList$election_date), as.Date("2006-11-22"))
  })

  test_that(paste("addCabinetInfo warns and does nothing when run twice -", fmt), {
    once <- addCabinetInfo(ev)
    expect_warning(twice <- addCabinetInfo(once), "already included")
    expect_identical(twice, once)
  })

  test_that(paste("addPartyInfo adds party columns to all sub-tables -", fmt), {
    party_cols <- c("cabinet_party", "prime_minister", "vote_share", "seat_share", "party_seats")
    result <- addPartyInfo(ev)
    for (tbl in c("voteList", "sponsorList", "votePerParty")) {
      expect_true(all(party_cols %in% names(result[[tbl]])), label = tbl)
      expect_equal(nrow(result[[tbl]]), nrow(ev[[tbl]]), label = tbl)
    }
  })

  test_that(paste("addPartyInfo gives correct party values for January 2010 -", fmt), {
    vpp <- addPartyInfo(ev)$votePerParty
    party_cols <- c("cabinet_party", "prime_minister", "party_seats")
    val <- function(p) unique(as.data.frame(vpp)[vpp$party == p, party_cols])
    expect_equal(nrow(val("CDA")), 1)
    expect_equal(unlist(val("CDA"), use.names = FALSE), c(1, 1, 41))
    expect_equal(unlist(val("PvdA"), use.names = FALSE), c(1, 0, 33))
    expect_equal(unlist(val("ChristenUnie"), use.names = FALSE), c(1, 0, 6))
    expect_equal(unlist(val("PVV"), use.names = FALSE), c(0, 0, 9))
    expect_equal(unlist(val("PvdD"), use.names = FALSE), c(0, 0, 2))
  })

  test_that(paste("addPartyInfo seats match partyElectionInfo -", fmt), {
    vpp <- addPartyInfo(ev)$votePerParty
    pei <- as.data.frame(ev$partyElectionInfo)
    pei <- unique(pei[pei$term_start == as.Date("2006-11-30"), c("party", "seats")])
    merged <- merge(unique(as.data.frame(vpp)[, c("party", "party_seats")]), pei, by = "party")
    expect_gt(nrow(merged), 0)
    expect_equal(merged$party_seats, as.numeric(merged$seats))
  })

  test_that(paste("addPartyInfo sponsorList is matched on sponsorParty -", fmt), {
    sl <- addPartyInfo(ev)$sponsorList
    expect_true(all(sl$cabinet_party[sl$sponsorParty == "CDA"] == 1))
    expect_true(all(sl$cabinet_party[sl$sponsorParty == "SP"] == 0))
  })

  test_that(paste("addPartyInfo includetype = 'all' keeps row counts -", fmt), {
    result <- addPartyInfo(ev, includetype = "all")
    expect_true("family_name" %in% names(result$voteList))
    for (tbl in c("voteList", "sponsorList", "votePerParty")) {
      expect_equal(nrow(result[[tbl]]), nrow(ev[[tbl]]), label = tbl)
    }
    expect_false("family_name" %in% names(addPartyInfo(ev)$voteList))
  })

  test_that(paste("addPartyInfo addto only changes the requested sub-tables -", fmt), {
    party_cols <- c("cabinet_party", "prime_minister", "vote_share", "seat_share", "party_seats")
    result <- addPartyInfo(ev, addto = "votePerParty")
    expect_true(all(party_cols %in% names(result$votePerParty)))
    expect_false(any(party_cols %in% names(result$voteList)))
    expect_false(any(party_cols %in% names(result$sponsorList)))
  })

  test_that(paste("addPartyInfo warns when run twice -", fmt), {
    once <- addPartyInfo(ev, addto = "voteList")
    expect_warning(twice <- addPartyInfo(once, addto = "voteList"), "already in voteList")
    expect_identical(twice$voteList, once$voteList)
  })

  test_that(paste("addInfo equals addCabinetInfo followed by addPartyInfo -", fmt), {
    expect_identical(addInfo(ev), addPartyInfo(addCabinetInfo(ev)))
  })
}


# REPDEM-specific ------------------------------------------------------------

test_that("REPDEM: January 2010 votes fall under Balkenende IV, period 1", {
  result <- addCabinetInfo(examplevotes)$metaList
  expect_equal(unique(as.character(result$cabinet_name)), "Balkenende IV")
  expect_equal(unique(result$cabinet_name_parlementcom), "Balkenende IV")
  expect_equal(unique(result$cabinet_id), "12026")
  expect_equal(unique(result$cabinet_period_id), "12026-1")
  expect_true(all(is.na(result$caretaker_reason)))
  expect_equal(unique(result$caretaker_parlgov), 0)
})

test_that("REPDEM: date-based assignment agrees with voteCabinet", {
  x <- examplevotes
  x$voteCabinet <- NULL
  by_date <- addCabinetInfo(x)$metaList
  by_vote <- addCabinetInfo(examplevotes)$metaList
  expect_equal(by_date, by_vote)
})

test_that("REPDEM: caretaker comes from cabinetPeriods, not cabinetInfo", {
  x <- examplevotes
  x$voteCabinet <- NULL
  x$metaList$date[1] <- as.Date("2010-02-22") # Balkenende IV after resignation
  x$metaList$date[2] <- as.Date("2023-08-01") # Rutte IV after resignation
  x$metaList$date[3] <- as.Date("2010-03-15") # Balkenende V, interim cabinet
  x$metaList$date[4] <- as.Date("2017-04-01") # Rutte II after the 2017 election
  result <- addCabinetInfo(x)$metaList

  expect_equal(as.character(result$cabinet_name[1:4]),
    c("Balkenende IV", "Rutte IV", "Balkenende V", "Rutte II"))
  expect_equal(result$caretaker[1:4], c(1, 1, 1, 1))
  expect_equal(result$caretaker_reason[1:4],
    c("post_resignation", "post_resignation", "interim_cabinet", "post_election"))
  expect_equal(result$cabinet_period_id[1:4], c("12026-2", "12031-2", "12027-1", "12029-2"))
  expect_equal(result$cabinet_resigned[2], 1)
  # Rutte IV is not an interim cabinet, so cabinetInfo$caretaker is 0
  ci <- x$cabinetInfo
  expect_equal(ci$caretaker[as.character(ci$cabinet_name) == "Rutte IV"], 0)
  # Votes before the change are unaffected
  expect_true(all(result$caretaker[-(1:4)] == 0))
})

test_that("REPDEM: voteCabinet takes precedence over the date", {
  x <- examplevotes
  id1 <- x$metaList$id[1]
  i <- which(x$voteCabinet$id == id1)
  x$voteCabinet$cabinet_id[i] <- "12027"
  x$voteCabinet$cabinet_period_id[i] <- "12027-1"
  x$voteCabinet$caretaker[i] <- 1L
  x$voteCabinet$caretaker_reason[i] <- "interim_cabinet"
  result <- addCabinetInfo(x)$metaList
  r1 <- result[result$id == id1, ]
  expect_equal(as.character(r1$cabinet_name), "Balkenende V")
  expect_equal(r1$cabinet_period_id, "12027-1")
  expect_equal(r1$caretaker, 1)
  expect_equal(r1$caretaker_reason, "interim_cabinet")
  # other votes keep their date-based values
  expect_true(all(as.character(result$cabinet_name[result$id != id1]) == "Balkenende IV"))
})

test_that("REPDEM: addPartyInfo includetype = 'all' adds cabinet-specific party data", {
  result <- addPartyInfo(examplevotes, includetype = "all")
  expect_true(all(c("support_party", "ches_lrgen", "rile", "family_name") %in% names(result$voteList)))
})

test_that("REPDEM: party absent from partyCabinetInfo is not a cabinet party", {
  pci <- examplevotes$partyCabinetInfo
  expect_false("PvdD" %in% pci$party[pci$cabinet_id == "12026"])
  vpp <- addPartyInfo(examplevotes)$votePerParty
  expect_true(all(vpp$cabinet_party[vpp$party == "PvdD"] == 0))
})


# ParlGov-specific -----------------------------------------------------------

test_that("ParlGov: January 2010 votes fall under Balkenende V (Balkenende IV)", {
  result <- addCabinetInfo(examplevotes_legacy)$metaList
  expect_equal(unique(as.character(result$cabinet_name)), "Balkenende V")
  expect_equal(unique(result$cabinet_name_parlementcom), "Balkenende IV")
  expect_false(any(c("cabinet_period_id", "caretaker_reason") %in% names(result)))
})

test_that("ParlGov: caretaker comes from cabinetInfo", {
  x <- examplevotes_legacy
  x$metaList$date[1] <- as.Date("2010-03-15")
  result <- addCabinetInfo(x)$metaList
  expect_equal(as.character(result$cabinet_name[1]), "Balkenende VI")
  expect_equal(result$caretaker[1], 1)
})

test_that("ParlGov: includetype = 'all' adds partyInfo and survives duplicate rows", {
  expect_gt(sum(duplicated(examplevotes_legacy$partyInfo$party)), 0)
  result <- addPartyInfo(examplevotes_legacy, includetype = "all")
  expect_true(all(c("left_right", "family_name") %in% names(result$voteList)))
  expect_equal(nrow(result$votePerParty), nrow(examplevotes_legacy$votePerParty))
})


# questionList -----------------------------------------------------------------

for (fmt in names(example_questionlists)) {
  eq <- example_questionlists[[fmt]]

  test_that(paste("addInfo on questions keeps row counts -", fmt), {
    res <- addInfo(eq)
    for (tbl in c("metaList", "questionerList", "responderList")) {
      expect_equal(nrow(res[[tbl]]), nrow(eq[[tbl]]), label = tbl)
    }
    expect_true("dateQuestion" %in% names(res$questionerList))
    expect_true("dateResponse" %in% names(res$responderList))
    expect_equal(nrow(addPartyInfo(eq, includetype = "all")$questionerList), nrow(eq$questionerList))
  })

  test_that(paste("addCabinetInfo on questions uses the response date -", fmt), {
    result <- addCabinetInfo(eq)$metaList
    late <- result$dateResponse >= as.Date("2010-02-23")
    expect_true(any(late))
    expected <- if (fmt == "repdem") c("Balkenende IV", "Balkenende V") else c("Balkenende V", "Balkenende VI")
    expect_equal(as.character(result$cabinet_name), ifelse(late, expected[2], expected[1]))
    expect_equal(result$caretaker, as.integer(late), ignore_attr = TRUE)
    expect_equal(unique(result$term_start), as.Date("2006-11-30"))
  })

  test_that(paste("addPartyInfo on questions gives correct party values -", fmt), {
    result <- addPartyInfo(eq)
    rl <- result$responderList
    expect_true(all(rl$cabinet_party[rl$responderParty == "CDA"] == 1))
    expect_true(all(rl$prime_minister[rl$responderParty == "CDA"] == 1))
    ql <- result$questionerList
    expect_true(all(ql$cabinet_party[ql$questionerParty == "PVV"] == 0))
    expect_true(all(ql$party_seats[ql$questionerParty == "PVV"] == 9))
  })

  test_that(paste("addCabinetInfo on questions works without formal_resignation -", fmt), {
    x <- eq
    x$cabinetInfo$formal_resignation <- NULL
    result <- addCabinetInfo(x)
    expect_false("cabinet_resigned" %in% names(result$metaList))
    expect_true("cabinet_name" %in% names(result$metaList))
  })

  test_that(paste("addInfo equals addCabinetInfo followed by addPartyInfo for questions -", fmt), {
    expect_identical(addInfo(eq), addPartyInfo(addCabinetInfo(eq)))
  })
}

test_that("REPDEM questions: interim cabinet after 2010-02-23 and questionCabinet agree", {
  result <- addCabinetInfo(examplequestions)$metaList
  late <- result$dateResponse >= as.Date("2010-02-23")
  expect_true(all(result$caretaker_reason[late] == "interim_cabinet"))
  expect_true(all(result$cabinet_period_id[late] == "12027-1"))
  x <- examplequestions
  x$questionCabinet <- NULL
  expect_equal(addCabinetInfo(x)$metaList, result)
})

test_that("REPDEM questions: questionCabinet takes precedence over the date", {
  x <- examplequestions
  id1 <- x$metaList$dcIdentifier[1]
  i <- which(x$questionCabinet$dcIdentifier == id1)
  x$questionCabinet$cabinet_id[i] <- "12027"
  x$questionCabinet$cabinet_period_id[i] <- "12027-1"
  x$questionCabinet$caretaker[i] <- 1L
  x$questionCabinet$caretaker_reason[i] <- "interim_cabinet"
  result <- addCabinetInfo(x)$metaList
  r1 <- result[result$dcIdentifier == id1, ]
  expect_equal(as.character(r1$cabinet_name), "Balkenende V")
  expect_equal(r1$caretaker, 1)
  expect_equal(r1$caretaker_reason, "interim_cabinet")
})


# Party information follows the per-unit cabinet assignment ------------------

test_that("REPDEM: party info follows voteCabinet, distinct from votes on the same date", {
  x <- examplevotes
  id1 <- x$metaList$id[1]
  i <- which(x$voteCabinet$id == id1)
  x$voteCabinet$cabinet_id[i] <- "12027" # Balkenende V: CDA and ChristenUnie only
  x$voteCabinet$cabinet_period_id[i] <- "12027-1"
  res <- addInfo(x)

  expect_equal(as.character(res$metaList$cabinet_name[res$metaList$id == id1]), "Balkenende V")
  same_date <- res$metaList$id[res$metaList$date == res$metaList$date[res$metaList$id == id1] &
    res$metaList$id != id1]
  expect_gt(length(same_date), 0)

  for (tbl in c("voteList", "votePerParty")) {
    d <- res[[tbl]]
    expect_equal(nrow(d), nrow(x[[tbl]]), label = tbl)
    expect_true(all(d$cabinet_party[d$id == id1 & d$party == "PvdA"] == 0), label = tbl)
    expect_true(all(d$cabinet_party[d$id == id1 & d$party == "CDA"] == 1), label = tbl)
    expect_true(all(d$cabinet_party[d$id %in% same_date & d$party == "PvdA"] == 1), label = tbl)
  }
  sl <- res$sponsorList
  expect_true(all(sl$cabinet_party[sl$id == id1 & sl$sponsorParty == "PvdA"] == 0))
})

test_that("REPDEM: responders follow questionCabinet, questioners the question date", {
  x <- examplequestions
  id1 <- x$metaList$dcIdentifier[1]
  i <- which(x$questionCabinet$dcIdentifier == id1)
  x$questionCabinet$cabinet_id[i] <- "12027"
  res <- addInfo(x)

  rl <- res$responderList
  expect_true(any(rl$dcIdentifier == id1 & rl$responderParty == "PvdA"))
  expect_true(all(rl$cabinet_party[rl$dcIdentifier == id1 & rl$responderParty == "PvdA"] == 0))
  expect_true(all(rl$cabinet_party[rl$dcIdentifier != id1 & rl$responderParty == "PvdA"] == 1))

  # Questioners are tied to dateQuestion (Balkenende IV): unchanged
  expect_equal(res$questionerList, addInfo(examplequestions)$questionerList)
})

test_that("party info is consistent with metaList cabinet for every vote", {
  x <- examplevotes
  x$voteCabinet$cabinet_id[1:5] <- "12027"
  res <- addInfo(x)
  pci <- as.data.frame(x$partyCabinetInfo)
  cab <- as.character(res$metaList$cabinet_id)[match(res$votePerParty$id, res$metaList$id)]
  exp <- pci$cabinet_party[match(paste(cab, res$votePerParty$party), paste(pci$cabinet_id, pci$party))]
  exp[is.na(exp) & !is.na(res$votePerParty$party_seats)] <- 0L
  expect_equal(res$votePerParty$cabinet_party, exp)
})
