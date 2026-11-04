# Internal helpers ---------------------------------------------------------
#
# Cabinet, election and party information comes in two layouts:
#
# * REPDEM (datasetInfo$external_source == "repdem", class "voteListRepdem"):
#   cabinetInfo has one row per cabinet, identified by cabinet_id. Whether a
#   cabinet was caretaker (demissionair) is recorded per period in
#   cabinetPeriods, and per vote in voteCabinet. cabinetInfo$caretaker only
#   flags interim cabinets.
# * ParlGov (legacy and older layouts): cabinetInfo has a separate row for each
#   caretaker spell, and cabinetInfo$caretaker flags these rows. There are no
#   cabinetPeriods or voteCabinet tables.
#
# The helpers below take dates, look up the cabinet, period and parliamentary
# term in force on each date, and return one row per unique date.

# Unique, non-missing dates.
unique_dates <- function(dates) {
  dates <- unique(as.Date(dates))
  sort(dates[!is.na(dates)])
}

# Sort a reference table by a date column and drop rows without that date.
sort_by_date <- function(df, col) {
  df <- as.data.frame(df)
  df[[col]] <- as.Date(df[[col]])
  df <- df[!is.na(df[[col]]), , drop = FALSE]
  df[order(df[[col]]), , drop = FALSE]
}

# Index of the interval (defined by sorted start dates) that contains each
# date. The last interval is open-ended. Dates before the first start get NA.
interval_index <- function(dates, starts) {
  idx <- findInterval(as.numeric(as.Date(dates)), as.numeric(starts))
  idx[!is.na(idx) & idx == 0] <- NA_integer_
  idx
}

# Cabinet (and, for REPDEM, cabinet period) in office on each date.
cabinet_by_date <- function(x, dates) {
  dates <- unique_dates(dates)
  cab <- sort_by_date(x$cabinetInfo, "start_date")
  cab$cabinet_name <- as.character(cab$cabinet_name)
  idx <- interval_index(dates, cab$start_date)

  out <- data.frame(date = dates)
  if ("cabinet_id" %in% names(cab)) {
    out$cabinet_id <- as.character(cab$cabinet_id)[idx]
  }
  out$cabinet_name <- factor(cab$cabinet_name[idx],
    levels = unique(cab$cabinet_name), ordered = TRUE
  )
  if ("cabinet_name_parlementcom" %in% names(cab)) {
    out$cabinet_name_parlementcom <- as.character(cab$cabinet_name_parlementcom)[idx]
  }

  if (!is.null(x$cabinetPeriods)) {
    # REPDEM: caretaker status varies within a cabinet and is stored per period
    per <- sort_by_date(x$cabinetPeriods, "start_date")
    pidx <- interval_index(dates, per$start_date)
    out$cabinet_period_id <- as.character(per$cabinet_period_id)[pidx]
    out$caretaker <- per$caretaker[pidx]
    if ("caretaker_reason" %in% names(per)) {
      out$caretaker_reason <- as.character(per$caretaker_reason)[pidx]
    }
    if ("caretaker_parlgov" %in% names(per)) {
      out$caretaker_parlgov <- per$caretaker_parlgov[pidx]
    }
  } else if ("caretaker" %in% names(cab)) {
    # ParlGov: caretaker spells are separate cabinets
    out$caretaker <- cab$caretaker[idx]
  }

  # formal_resignation is not part of all data versions, so check if it is present
  if ("formal_resignation" %in% names(cab)) {
    out$cabinet_resigned <- as.numeric(out$date > as.Date(cab$formal_resignation[idx]))
  }

  out
}

# Parliamentary term (and election) in force on each date.
election_by_date <- function(x, dates) {
  dates <- unique_dates(dates)
  el <- sort_by_date(dplyr::distinct(as.data.frame(x$electionInfo)), "term_start")
  idx <- interval_index(dates, el$term_start)
  out <- cbind(data.frame(date = dates), el[idx, , drop = FALSE])
  rownames(out) <- NULL
  out
}

# Cabinet and election information for each date.
cabinet_info_by_date <- function(x, dates) {
  dplyr::left_join(cabinet_by_date(x, dates), election_by_date(x, dates), by = "date")
}

# Add the columns of `info` (one row per date) to `df`, matching `df[[date_col]]`
# to `info$date`. Only columns not yet present in `df` are added.
add_by_date <- function(df, info, date_col) {
  m <- match(as.Date(df[[date_col]]), info$date)
  for (col in setdiff(names(info), c("date", names(df)))) {
    df[[col]] <- info[[col]][m]
  }
  rownames(df) <- NULL
  df
}

# Overwrite date-based cabinet information in metaList with the per-unit
# assignment in voteCabinet or questionCabinet (REPDEM data). Votes or
# questions not in that table keep the date-based values.
apply_unit_cabinet <- function(x, table, id_col, date_col) {
  vc <- as.data.frame(x[[table]])
  m <- match(as.character(x$metaList[[id_col]]), as.character(vc[[id_col]]))
  has <- !is.na(m)
  if (!any(has)) {
    return(x)
  }

  cols <- intersect(
    c("cabinet_id", "cabinet_period_id", "caretaker", "caretaker_reason", "caretaker_parlgov"),
    names(vc)
  )
  for (col in cols) {
    value <- vc[[col]][m]
    if (is.factor(value) || col %in% c("cabinet_id", "cabinet_period_id", "caretaker_reason")) {
      value <- as.character(value)
    }
    if (!(col %in% names(x$metaList))) {
      x$metaList[[col]] <- rep(value[NA_integer_], nrow(x$metaList))
    }
    x$metaList[[col]][has] <- value[has]
  }

  # Cabinet names follow from the cabinet_id in voteCabinet/questionCabinet
  if ("cabinet_id" %in% cols && "cabinet_id" %in% names(x$cabinetInfo)) {
    cab <- as.data.frame(x$cabinetInfo)
    cm <- match(x$metaList$cabinet_id, as.character(cab$cabinet_id))
    upd <- has & !is.na(cm)
    x$metaList$cabinet_name[upd] <- as.character(cab$cabinet_name)[cm[upd]]
    if ("cabinet_name_parlementcom" %in% names(x$metaList)) {
      x$metaList$cabinet_name_parlementcom[upd] <-
        as.character(cab$cabinet_name_parlementcom)[cm[upd]]
    }
    if ("cabinet_resigned" %in% names(x$metaList) && "formal_resignation" %in% names(cab)) {
      x$metaList$cabinet_resigned[upd] <- as.numeric(
        as.Date(x$metaList[[date_col]][upd]) > as.Date(cab$formal_resignation[cm[upd]])
      )
    }
  }
  x
}

# Party characteristics for each combination of date and party.
party_info_by_date <- function(x, dates, includetype = "basic") {
  includetype <- match.arg(includetype, c("basic", "all"))
  dates <- unique_dates(dates)
  cab <- cabinet_by_date(x, dates)

  # Cabinet participation. Match on cabinet_id if both tables have it (REPDEM),
  # otherwise on cabinet_name.
  pci <- as.data.frame(x$partyCabinetInfo)
  key <- if ("cabinet_id" %in% names(cab) && "cabinet_id" %in% names(pci)) "cabinet_id" else "cabinet_name"
  pci[[key]] <- as.character(pci[[key]])
  pci_cols <- c("party", "cabinet_party", "prime_minister", "support_party")
  if (includetype == "all") {
    pci_cols <- c(pci_cols, "family_name", "ches_lrgen", "ches_lrecon", "rile")
  }
  pci <- pci[!is.na(pci$party), unique(c(key, intersect(pci_cols, names(pci)))), drop = FALSE]
  pci$party <- as.character(pci$party)
  pci <- dplyr::distinct(pci, .data[[key]], .data$party, .keep_all = TRUE)
  pci$in_cabinet_data <- TRUE

  cab_keys <- data.frame(date = cab$date, key = as.character(cab[[key]]))
  names(cab_keys)[2] <- key
  cab_keys <- cab_keys[!is.na(cab_keys[[key]]), , drop = FALSE]
  partyCabinet <- dplyr::inner_join(cab_keys, pci, by = key, relationship = "many-to-many")
  partyCabinet[[key]] <- NULL

  # Election results of the parliamentary term in force
  el <- election_by_date(x, dates)[, c("date", "term_start")]
  el <- el[!is.na(el$term_start), , drop = FALSE]
  pei <- as.data.frame(x$partyElectionInfo)
  pei$term_start <- as.Date(pei$term_start)
  pei <- dplyr::rename(pei, dplyr::any_of(c("party_seats" = "seats")))
  pei <- pei[!is.na(pei$party) & !is.na(pei$term_start),
    intersect(c("term_start", "party", "vote_share", "seat_share", "party_seats"), names(pei)),
    drop = FALSE
  ]
  pei$party <- as.character(pei$party)
  pei <- dplyr::distinct(pei, .data$term_start, .data$party, .keep_all = TRUE)
  partyElection <- dplyr::inner_join(el, pei, by = "term_start", relationship = "many-to-many")
  partyElection$term_start <- NULL

  combined <- dplyr::full_join(partyCabinet, partyElection, by = c("date", "party"))

  # Parties not listed for a cabinet are not in the cabinet
  absent <- is.na(combined$in_cabinet_data) & combined$date %in% cab_keys$date
  for (col in intersect(c("cabinet_party", "prime_minister"), names(combined))) {
    combined[[col]][absent] <- 0L
  }
  combined$in_cabinet_data <- NULL

  if (includetype == "all" && !is.null(x$partyInfo)) {
    pinfo <- as.data.frame(x$partyInfo)
    pinfo <- pinfo[!is.na(pinfo$party), , drop = FALSE]
    pinfo$party <- as.character(pinfo$party)
    pinfo <- dplyr::distinct(pinfo, .data$party, .keep_all = TRUE)
    pinfo <- pinfo[, c("party", setdiff(names(pinfo), names(combined))), drop = FALSE]
    combined <- dplyr::left_join(combined, pinfo, by = "party")
  }

  combined <- combined[order(combined$date, combined$party), , drop = FALSE]
  rownames(combined) <- NULL
  combined
}

# Add party information to a sub-table, matching on date and party. If the
# sub-table has no date column, it is taken from metaList via the id column.
join_party_info <- function(tbl, info, meta, id_col, date_col, party_col, tblname) {
  if (is.null(tbl)) {
    return(tbl)
  }
  if (!(date_col %in% names(tbl))) {
    m <- match(as.character(tbl[[id_col]]), as.character(meta[[id_col]]))
    tbl[[date_col]] <- as.Date(meta[[date_col]])[m]
  }
  newcols <- setdiff(names(info), c("date", "party", names(tbl)))
  if (length(newcols) == 0) {
    warning("All party information is already in ", tblname, ". Did not add.")
    return(tbl)
  }
  info <- info[, c("date", "party", newcols), drop = FALSE]
  names(info)[1:2] <- c(date_col, party_col)
  if (is.factor(tbl[[party_col]])) {
    info[[party_col]] <- factor(info[[party_col]])
  }
  dplyr::left_join(tbl, info, by = c(date_col, party_col))
}


# addCabinetInfo ----------------------------------------------------------
#' @exportS3Method
addCabinetInfo.default <- function(x, ...) {}

#' Add information on cabinet composition and elections to metadata.
#'
#' Cabinet and election data are obtained from REPDEM (with ParlGov for the
#' period before 1946) or, in older versions of the data, from ParlGov
#' (http://www.parlgov.org/). The source is recorded in \code{x$datasetInfo}.
#'
#' The following columns are added to \code{metaList}: \code{cabinet_name}
#' (ordered factor), \code{cabinet_name_parlementcom}, \code{caretaker},
#' \code{cabinet_resigned} (1 if the date is after the formal resignation of
#' the cabinet), \code{term_start} and \code{election_date}.
#'
#' The meaning of \code{caretaker} depends on the source. ParlGov records
#' caretaker spells as separate cabinets, and \code{caretaker} is 1 for these.
#' REPDEM has one record per cabinet; caretaker status is recorded per cabinet
#' period (table \code{cabinetPeriods}), per vote (table \code{voteCabinet})
#' and per question (table \code{questionCabinet}, for the response date).
#' For REPDEM data, \code{caretaker} is 1 whenever the cabinet was caretaker
#' (demissionair): after its resignation, after an election, or as an interim
#' cabinet. REPDEM data also get \code{cabinet_id}, \code{cabinet_period_id},
#' \code{caretaker_reason} and \code{caretaker_parlgov} (the ParlGov
#' definition). For votes and questions, the assignment in \code{voteCabinet}
#' or \code{questionCabinet} is used; votes or questions missing from that
#' table are matched on date.
#'
#' @param x A voteList or questionList object
#' @return A voteList or questionList object
#' @param ... Other parameters passed on.
#' @importFrom rlang .data
#' @export
#' @examples
#' addCabinetInfo(examplevotes)
#' addCabinetInfo(examplevotes_legacy)
#' addCabinetInfo(examplequestions)
addCabinetInfo <- function(x, ...) {
  UseMethod("addCabinetInfo")
}

#' @describeIn addCabinetInfo Cabinet Information for voteList object
#' @export
addCabinetInfo.voteList <- function(x, ...) {
  if ("cabinet_name" %in% colnames(x$metaList)) {
    warning("Cabinet names are already included in metaList. Nothing has been added.")
    return(x)
  }

  info <- cabinet_info_by_date(x, x$metaList$date)
  x$metaList <- add_by_date(x$metaList, info, "date")

  if (!is.null(x$voteCabinet)) {
    x <- apply_unit_cabinet(x, "voteCabinet", id_col = "id", date_col = "date")
  }

  return(x)
}


# addCabinetInfo.questionList ---------------------------------------------
#' @describeIn addCabinetInfo Cabinet Information for questionList object. Information is valid for the date of the response to the question
#' @export
addCabinetInfo.questionList <- function(x, ...) {
  if ("cabinet_name" %in% colnames(x$metaList)) {
    warning("Cabinet names are already included in metaList. Nothing has been added.")
    return(x)
  }

  info <- cabinet_info_by_date(x, x$metaList$dateResponse)
  x$metaList <- add_by_date(x$metaList, info, "dateResponse")

  if (!is.null(x$questionCabinet)) {
    x <- apply_unit_cabinet(x, "questionCabinet",
      id_col = "dcIdentifier", date_col = "dateResponse"
    )
  }

  return(x)
}

# addPartyInfo ------------------------------------------------------------

#' @exportS3Method
addPartyInfo.default <- function(x, ...) {}

#' Add information on party characteristics to voteList or questionList object.
#'
#' Party data are obtained from REPDEM (with ParlGov for the period before
#' 1946) or, in older versions of the data, from ParlGov
#' (http://www.parlgov.org/). The source is recorded in \code{x$datasetInfo}.
#'
#' The basic information consists of \code{cabinet_party},
#' \code{prime_minister}, \code{vote_share}, \code{seat_share} and
#' \code{party_seats} (REPDEM data also include \code{support_party}).
#' Parties that hold seats but are not listed for the cabinet in office get
#' \code{cabinet_party = 0} and \code{prime_minister = 0}.
#'
#' @param x A voteList object
#' @param includetype A character value, "basic" or "all". Defaults to "basic". Basic only includes party characteristics that vary by date, while all includes all party characteristics (from \code{partyInfo}, and for REPDEM data also the cabinet-specific \code{family_name}, \code{ches_lrgen}, \code{ches_lrecon} and \code{rile}).
#' @param addto Character vector including the subtables to which party information should be added. Defaults to c("voteList", "sponsorList","votePerParty") vote a voteList object or c("questionerList", "responderList") for a questionList object.
#' @return A voteList or questionList object.
#' @param ... Other parameters passed on.
#' @export
#' @examples
#' examplevotes_with_partyinfo <- addPartyInfo(examplevotes)
addPartyInfo <- function(x, ...) {
  UseMethod("addPartyInfo")
}

#' @describeIn addPartyInfo Party characteristics for voteList object
#' @export
addPartyInfo.voteList <- function(x, includetype = "basic",
                                  addto = c("voteList", "sponsorList", "votePerParty"),
                                  ...) {
  info <- party_info_by_date(x, x$metaList$date, includetype)

  if ("voteList" %in% addto) {
    x$voteList <- join_party_info(x$voteList, info, x$metaList,
      id_col = "id", date_col = "date", party_col = "party", tblname = "voteList"
    )
  }
  if ("sponsorList" %in% addto) {
    x$sponsorList <- join_party_info(x$sponsorList, info, x$metaList,
      id_col = "id", date_col = "date", party_col = "sponsorParty", tblname = "sponsorList"
    )
  }
  if ("votePerParty" %in% addto) {
    x$votePerParty <- join_party_info(x$votePerParty, info, x$metaList,
      id_col = "id", date_col = "date", party_col = "party", tblname = "votePerParty"
    )
  }

  return(x)
}



#' @describeIn addPartyInfo Party characteristics for questionList object. Information for questioners refers to the day the question was asked, for the responders it pertains to the day the response was given.
#' @export
addPartyInfo.questionList <- function(x, includetype = "basic",
                                      addto = c("questionerList", "responderList"),
                                      ...) {
  if (nrow(x$metaList) > 5e3) warning("This is a large questionList object. This operation will increase the object size significantly.")

  info <- party_info_by_date(
    x, c(x$metaList$dateQuestion, x$metaList$dateResponse), includetype
  )

  if ("questionerList" %in% addto) {
    x$questionerList <- join_party_info(x$questionerList, info, x$metaList,
      id_col = "dcIdentifier", date_col = "dateQuestion",
      party_col = "questionerParty", tblname = "questionerList"
    )
  }
  if ("responderList" %in% addto) {
    x$responderList <- join_party_info(x$responderList, info, x$metaList,
      id_col = "dcIdentifier", date_col = "dateResponse",
      party_col = "responderParty", tblname = "responderList"
    )
  }

  return(x)
}





# addInfo -----------------------------------------------------------------
#' @exportS3Method
addInfo.default <- function(x, ...) {}

#' Add information on cabinets, elections and parties to dataset.
#'
#' Data are obtained from REPDEM (with ParlGov for the period before 1946) or,
#' in older versions of the data, from ParlGov (http://www.parlgov.org/).
#'
#' @param x A voteList object
#' @return A voteList object
#' @param ... Other parameters passed on.
#' @details This is a wrapper that runs both addCabinetInfo and addPartyInfo with default settings.
#' @export
#' @examples
#' examplevotes_with_info <- addInfo(examplevotes)
addInfo <- function(x, ...) {
  UseMethod("addInfo")
}

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
