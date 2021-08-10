# addCabinetInfo ----------------------------------------------------------
addCabinetInfo.default <- function(x, ...) {}

#' Add information on cabinet composition and elections to metadata. This data
#' is obtained from ParlGov (http://www.parlgov.org/).
#'
#' @param x A voteList or questionList object
#' @return A voteList or questionList object
#' @param ... Other parameters passed on.
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @export
#' @examples
#' addCabinetInfo(examplevotes)
addCabinetInfo <- function(x, ...)
  UseMethod("addCabinetInfo")

#' @describeIn addCabinetInfo Cabinet Information for voteList object
#' @export
addCabinetInfo.voteList <- function(x, ...) {
  if ("cabinet_name" %in% colnames(x$metaList)) {
    warning("Cabinet names are already included in metaList. Nothing has been added.")
    return(x)
  }


  min_date = min(as.Date(x$metaList$date), na.rm = TRUE)
  max_date = max(as.Date(x$metaList$date), na.rm = TRUE)

  all_dates <- seq.Date(from = min_date, to = max_date, by = 1)


  if (max_date < max(as.Date(x$cabinetInfo$start_date), na.rm = TRUE)) {
    end_date_selection = max(as.Date(x$cabinetInfo$start_date, na.rm = TRUE)) + 1
  } else {
    end_date_selection = max_date + 1
  }

  cabinet_name <- cut(all_dates,
    breaks = c(
      x$cabinetInfo$start_date,
      end_date_selection + 1
    ),
    labels = x$cabinetInfo$cabinet_name,
    ordered_result = TRUE
  )

  term_start <- cut(all_dates,
    breaks = c(
      x$electionInfo$term_start,
      max(as.Date(x$metaList$date), na.rm = TRUE) + 1
    ),
    labels = x$electionInfo$term_start,
    ordered_result = FALSE
  )

  term_start <- as.Date(term_start)

  cabInfoAllDays <- dplyr::left_join(data.frame(
    date = all_dates,
    cabinet_name = as.character(cabinet_name),
    stringsAsFactors = FALSE
  ),
  x$cabinetInfo,
  by = "cabinet_name"
  )

  # formal_resignation is not part of parlgov data, so check if it is present
  if ("formal_resignation" %in% colnames(cabInfoAllDays)) {
    cabInfoAllDays <- cabInfoAllDays %>%
      dplyr::mutate(cabinet_resigned = as.numeric(.data$date > .data$formal_resignation))
  }

  cabInfoAllDays <- cabInfoAllDays %>%
    dplyr::select(dplyr::any_of(c(
      "date", "cabinet_name",
      "cabinet_name_parlementcom",
      "caretaker", "cabinet_resigned"
    )))


  cabInfoAllDays$cabinet_name <- factor(cabInfoAllDays$cabinet_name,
    levels = levels(cabinet_name), ordered = TRUE
  )


  electionInfoAllDays <- dplyr::left_join(data.frame(
    date = all_dates,
    term_start
  ),
  x$electionInfo,
  by = "term_start"
  )
  out <- x
  out$metaList <- dplyr::left_join(out$metaList, cabInfoAllDays, by = "date")
  out$metaList <- dplyr::left_join(out$metaList,
    electionInfoAllDays,
    by = c("date")
  )

  return(out)
}


# addCabinetInfo.questionList ---------------------------------------------
#' @describeIn addCabinetInfo Cabinet Information for questionList object. Information is valid for the date of the response to the question
#' @export
addCabinetInfo.questionList <- function(x, ...) {
  if ("cabinet_name" %in% colnames(x$metaList)) {
    warning("Cabinet names are already included in metaList. Nothing has been added.")
    return(x)
  }


  all_dates <- seq.Date(
    from = as.Date(min(x$metaList$dateResponse)),
    to = as.Date(as.Date(max(x$metaList$dateResponse))), by = 1
  )

  cabinet_name <- cut(all_dates,
    breaks = c(
      x$cabinetInfo$start_date,
      as.Date(max(x$metaList$dateResponse)) + 1
    ),
    labels = x$cabinetInfo$cabinet_name,
    ordered_result = TRUE
  )

  term_start <- cut(all_dates,
    breaks = c(
      x$electionInfo$term_start,
      as.Date(max(x$metaList$dateResponse)) + 1
    ),
    labels = x$electionInfo$term_start,
    ordered_result = FALSE
  )

  term_start <- as.Date(term_start)


  cabInfoAllDays <- dplyr::left_join(data.frame(
    date = all_dates,
    cabinet_name = as.character(cabinet_name),
    stringsAsFactors = FALSE
  ),
  x$cabinetInfo,
  by = "cabinet_name"
  ) %>%
    dplyr::mutate(cabinet_resigned = as.numeric(.data$date > .data$formal_resignation)) %>%
    dplyr::select(dplyr::any_of(c(
      "date", "cabinet_name",
      "cabinet_name_parlementcom",
      "caretaker", "cabinet_resigned"
    )))

  cabInfoAllDays$cabinet_name <- factor(cabInfoAllDays$cabinet_name,
    levels = levels(cabinet_name), ordered = TRUE
  )


  electionInfoAllDays <- dplyr::left_join(data.frame(
    date = all_dates,
    term_start
  ),
  x$electionInfo,
  by = "term_start"
  )
  out <- x
  out$metaList <- dplyr::left_join(out$metaList, cabInfoAllDays, by = c("dateResponse" = "date"))
  out$metaList <- dplyr::left_join(out$metaList,
    electionInfoAllDays,
    by = c("dateResponse" = "date")
  )

  return(out)
}

# addPartyInfo ------------------------------------------------------------

addPartyInfo.default <- function(x, ...) {}

#' Add information on party characteristics to voteList or questionerList object. This data
#' is obtained from ParlGov (http://www.parlgov.org/).
#'
#' @param x A voteList object
#' @param includetype A charcter value, "basic" or "all". Defaults to "basic". Basic only includes party characteristics that vary by date, while all includes all party characteristics.
#' @param addto Character vector including the subtables to which party information should be added. Defaults to c("voteList", "sponsorList","votePerParty") vote a voteList object or c("questionerList", "responderList") for a questionList object.
#' @return A voteList or questionList object.
#' @param ... Other parameters passed on.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' examplevotes_with_partyinfo = addPartyInfo(examplevotes)
addPartyInfo <- function(x, ...)
  UseMethod("addPartyInfo")

#' @describeIn addPartyInfo Party characteristics for voteList object
#' @export
addPartyInfo.voteList <- function(x, includetype = "basic",
                                  addto = c("voteList", "sponsorList", "votePerParty"),
                                  ...) {
  if (nrow(x$metaList) > 5e3) warning("This is a large voteList object. This operation will increase the object size significantly.")

  min_date = min(as.Date(x$metaList$date), na.rm = TRUE)
  max_date = max(as.Date(x$metaList$date), na.rm = TRUE)

  all_dates <- seq.Date(from = min_date, to = max_date, by = 1)

  term_starts_unique <- x$electionInfo %>%
    dplyr::select("term_start") %>%
    dplyr::arrange("term_start")

  term_start <- cut(all_dates,
    breaks = c(
      term_starts_unique$term_start,
      max(as.Date(x$metaList$date), na.rm = TRUE) + 1
    ),
    labels = term_starts_unique$term_start,
    ordered_result = FALSE
  )
  term_start <- as.Date(term_start)

  if (max_date < max(as.Date(x$cabinetInfo$start_date), na.rm = TRUE)) {
    end_date_selection = max(as.Date(x$cabinetInfo$start_date, na.rm = TRUE)) + 1
  } else {
    end_date_selection = max_date + 1
  }

  cabinet_name <- cut(all_dates,
    breaks = c(
      x$cabinetInfo$start_date,
      end_date_selection + 1
    ),
    labels = x$cabinetInfo$cabinet_name,
    ordered_result = TRUE
  )

  partyCabinetInfoAllDays <-
    dplyr::left_join(
      data.frame(
        date = all_dates,
        cabinet_name = as.character(cabinet_name),
        stringsAsFactors = FALSE
      ),
      x$partyCabinetInfo,
      by = "cabinet_name"
    ) %>%
    dplyr::select(dplyr::any_of(c("date", "party", "cabinet_party", "prime_minister")))


  partyElectionInfoAllDays <- dplyr::left_join(data.frame(date = all_dates, term_start),
    x$partyElectionInfo,
    by = "term_start"
  ) %>%
    dplyr::rename("party_seats" = "seats") %>%
    dplyr::select(dplyr::any_of(c("date", "party", "vote_share", "seat_share", "party_seats")))

  combinedInfo <- dplyr::full_join(partyCabinetInfoAllDays, partyElectionInfoAllDays,
    by = c("date", "party")
  )

  if (includetype == "all") {
    combinedInfo <- dplyr::left_join(combinedInfo, x$partyInfo, by = "party")
  }

  # # Force party names to characters, encoded as UTF-8 to prevent crashes in
  # # merging
  # combinedInfo$party <- as.character(combinedInfo$party)
  # Encoding(combinedInfo$party) <- "UTF-8"

  # Add data to object
  if ("voteList" %in% addto) {
    x$metaList$id <- as.character(x$metaList$id)
    x$voteList$id <- as.character(x$voteList$id)
    if (!("date" %in% names(x$voteList))) {
      x$voteList <- dplyr::left_join(x$voteList,
        dplyr::select(
          x$metaList,
          dplyr::any_of(c("id", "date"))
        ),
        by = "id"
      )
    }
    if (all(colnames(combinedInfo) %in% colnames(x$voteList))) {
      warning("All party information is already in voteList. Did not add.")
    } else {
      # Only include columns that are not yet in the data
      notYetPresent <- colnames(combinedInfo[-c(1:2)])[which(!colnames(combinedInfo[-c(1:2)]) %in% colnames(x$voteList))]
      combinedInfo1 <- combinedInfo[, c("date", "party", notYetPresent)]
      x$voteList <- dplyr::left_join(x$voteList,
        combinedInfo1,
        by = c("date", "party")
      )
    }
  }


  if ("sponsorList" %in% addto) {
    x$sponsorList$id <- as.character(x$sponsorList$id)

    if (is.null(x$sponsorList$date)) {
      x$sponsorList <- dplyr::left_join(x$sponsorList,
        dplyr::select(
          x$metaList,
          dplyr::any_of(c("id", "date"))
        ),
        by = "id"
      )
    }
    if (all(colnames(combinedInfo[-c(1:2)]) %in% colnames(x$sponsorList))) {
      warning("All party information is already in sponsorList. Did not add.")
    } else {
      # Only include columns that are not yet in the data
      notYetPresent <- colnames(combinedInfo)[-c(1:2)][which(!colnames(combinedInfo[-c(1:2)]) %in% colnames(x$sponsorList))]

      combinedInfo2 <- combinedInfo[, c("date", "party", notYetPresent)]
      x$sponsorList <- dplyr::left_join(x$sponsorList, combinedInfo2,
        by = c("date", "sponsorParty" = "party")
      )
    }
  }

  if ("votePerParty" %in% addto) {
    if (all(colnames(combinedInfo) %in% colnames(x$votePerParty))) {
      warning("All party information is already in votePerParty. Did not add.")
    } else {
      if (!("date" %in% names(x$votePerParty))) {
        x$votePerParty <- dplyr::left_join(x$votePerParty,
          dplyr::select(x$metaList, dplyr::any_of(c("id", "date"))),
          by = "id"
        )
      }
      # Only include columns that are not yet in the data
      notYetPresent <- colnames(combinedInfo)[-c(1:2)][which(!colnames(combinedInfo[-c(1:2)]) %in% colnames(x$votePerParty))]

      combinedInfo3 <- combinedInfo[, c("date", "party", notYetPresent)]
      x$votePerParty <- dplyr::left_join(x$votePerParty, combinedInfo3,
        by = c("date", "party")
      )
    }
  }

  return(x)
}



#' @describeIn addPartyInfo Party characteristics for questionList object. Information for questioners refers to the day the question was asked, for the responders it pertains to the day the response was given.
#' @export
addPartyInfo.questionList <- function(x, includetype = "basic",
                                      addto = c("questionerList", "responderList"),
                                      ...) {
  if (nrow(x$metaList) > 5e3) warning("This is a large questionList object. This operation will increase the object size significantly.")

  date.na.omit <- function(q) {
    return(subset(q, q > as.Date("0001-01-01")))
  }

  all_dates <- seq.Date(
    from = as.Date(min(
      c(date.na.omit(x$metaList$dateResponse)),
      date.na.omit(x$metaList$dateQuestion)
    )),
    to = as.Date(as.Date(max(c(
      date.na.omit(x$metaList$dateResponse),
      date.na.omit(x$metaList$dateQuestion)
    )))),
    by = 1
  )
  term_starts_unique <- x$electionInfo %>%
    dplyr::select("term_start") %>%
    dplyr::arrange("term_start")

  term_start <- cut(all_dates,
    breaks = c(
      term_starts_unique$term_start,
      as.Date(max(x$metaList$dateResponse)) + 1
    ),
    labels = term_starts_unique$term_start,
    ordered_result = FALSE
  )
  term_start <- as.Date(term_start)

  cabinet_name <- cut(all_dates,
    breaks = c(
      x$cabinetInfo$start_date,
      as.Date(max(x$metaList$dateResponse)) + 1
    ),
    labels = x$cabinetInfo$cabinet_name,
    ordered_result = TRUE
  )

  partyCabinetInfoAllDays <- dplyr::left_join(data.frame(date = all_dates, cabinet_name = as.character(cabinet_name), stringsAsFactors = FALSE),
    x$partyCabinetInfo,
    by = "cabinet_name"
  ) %>%
    dplyr::select(dplyr::any_of(c("date", "party", "cabinet_party", "prime_minister")))


  partyElectionInfoAllDays <- dplyr::left_join(data.frame(date = all_dates, term_start),
    x$partyElectionInfo,
    by = "term_start"
  ) %>%
    dplyr::rename("party_seats" = "seats") %>%
    dplyr::select(dplyr::any_of(c("date", "party", "vote_share", "seat_share", "party_seats")))

  combinedInfo <- dplyr::full_join(partyCabinetInfoAllDays, partyElectionInfoAllDays,
    by = c("date", "party")
  )

  if (includetype == "all") {
    combinedInfo <- dplyr::left_join(combinedInfo, x$partyInfo, by = "party")
  }

  # # Force party names to characters, encoded as UTF-8 to prevent crashes in
  # # merging
  # combinedInfo$party <- as.character(combinedInfo$party)
  # Encoding(combinedInfo$party) <- "UTF-8"

  # Add data to object
  if ("questionerList" %in% addto) {
    x$metaList$dcIdentifier <- as.character(x$metaList$dcIdentifier)
    x$questionerList$dcIdentifier <- as.character(x$questionerList$dcIdentifier)
    if (is.null(x$questionerList$dateQuestion)) {
      x$questionerList <- dplyr::left_join(x$questionerList,
        dplyr::select(
          x$metaList,
          dplyr::any_of(c("dcIdentifier", "dateQuestion"))
        ),
        by = "dcIdentifier"
      )
    }
    if (all(colnames(combinedInfo) %in% colnames(x$questionerList))) {
      warning("All party information is already in questionerList. Did not add.")
    } else {
      # Only include columns that are not yet in the data
      notYetPresent <- colnames(combinedInfo[-c(1:2)])[which(!colnames(combinedInfo[-c(1:2)]) %in% colnames(x$questionerList))]
      combinedInfo1 <- combinedInfo[, c("date", "party", notYetPresent)]
      x$questionerList <- dplyr::left_join(x$questionerList, combinedInfo1, by = c("dateQuestion" = "date", "questionerParty" = "party"))
    }
  }

  if ("responderList" %in% addto) {
    x$metaList$dcIdentifier <- as.character(x$metaList$dcIdentifier)
    x$responderList$dcIdentifier <- as.character(x$responderList$dcIdentifier)
    if (is.null(x$responderList$dateResponse)) {
      x$responderList <- dplyr::left_join(x$responderList,
        dplyr::select(
          x$metaList,
          dplyr::any_of(c("dcIdentifier", "dateResponse"))
        ),
        by = "dcIdentifier"
      )
    }
    if (all(colnames(combinedInfo) %in% colnames(x$responderList))) {
      warning("All party information is already in responderList. Did not add.")
    } else {
      # Only include columns that are not yet in the data
      notYetPresent <- colnames(combinedInfo[-c(1:2)])[which(!colnames(combinedInfo[-c(1:2)]) %in% colnames(x$responderList))]
      combinedInfo1 <- combinedInfo[, c("date", "party", notYetPresent)]
      x$responderList <- dplyr::left_join(x$responderList, combinedInfo1, by = c("dateResponse" = "date", "responderParty" = "party"))
    }
  }


  return(x)
}





# addInfo -----------------------------------------------------------------
addInfo.default <- function(x, ...) {}

#' Add information on cabinets, elections and parties to dataset. This data
#' is obtained from ParlGov (http://www.parlgov.org/).
#'
#' @param x A voteList object
#' @return A voteList object
#' @param ... Other parameters passed on.
#' @details This is a wrapper that runs both addCabinetInfo and addPartyInfo with default settings.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' examplevotes_with_info <- addInfo(examplevotes)
addInfo <- function(x, ...)
  UseMethod("addInfo")

#' @describeIn addInfo Add information to voteList object
#' @export
addInfo.voteList <- function(x, ...) {
  return(addPartyInfo.voteList(addCabinetInfo.voteList(x)))
}

#' @describeIn addInfo Add information to questionList object
#' @export
addInfo.questionList <- function(x, ...) {
  return(addPartyInfo.questionList(addCabinetInfo.questionList(x)))
}
