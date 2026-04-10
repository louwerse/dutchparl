#' Subset voteList object
#'
#' @param x A voteList object, most of the time the votes object from the Dutch Parliamentary Behaviour Dataset.
#' @param df The name of the data.frame in the voteList to filter on. Options include metaList, sponsorList, and categoryList.
#' @param subset The subset command.
#' @param select Expression, indicating columns to select from data frame
#' @param drop passed on to [ indexing operator
#' @param drop.levels If true, superfluous levels in the data.frames will be removed.
#' @param ... Other parameters (ignored)
#' @return The subsetted voteList object.
#' @export
#' @examples
#' subset(examplevotes, examplevotes$metaList, date > as.Date("2010-01-15"))
subset.voteList <- function(x, df, subset, select, drop = FALSE, drop.levels = TRUE, ...) {
  if (missing(subset)) {
    r <- TRUE
  } else {
    e <- substitute(subset)
    r <- eval(e, df, parent.frame())
    if (!is.logical(r)) {
      stop("'subset' must evaluate to logical")
    }
    r <- r & !is.na(r)
  }
  if (missing(select)) {
    vars <- TRUE
  } else {
    nl <- as.list(seq_along(df))
    names(nl) <- names(df)
    vars <- eval(substitute(select), nl, parent.frame())
  }
  ss <- df[r, vars, drop = drop]
  select_ids <- ss$id

  voteList <- x
  voteList$metaList <- voteList$metaList[voteList$metaList$id %in% select_ids, ]
  voteList$voteList <- voteList$voteList[voteList$voteList$id %in% select_ids, ]
  voteList$voteMatrix <- voteList$voteMatrix[voteList$voteMatrix$id %in% select_ids, ]
  voteList$sponsorList <- voteList$sponsorList[voteList$sponsorList$id %in% select_ids, ]
  voteList$categoryList <- voteList$categoryList[voteList$categoryList$id %in% select_ids, ]
  voteList$votePerParty <- voteList$votePerParty[voteList$votePerParty$id %in% select_ids, ]

  if (drop.levels) {
    voteList$metaList <- droplevels(voteList$metaList)
    voteList$voteList <- droplevels(voteList$voteList)
    voteList$voteMatrix <- droplevels(voteList$voteMatrix)
    if (!is.null(voteList$sponsorList)) voteList$sponsorList <- droplevels(voteList$sponsorList)
    if (!is.null(voteList$categoryList)) voteList$categoryList <- droplevels(voteList$categoryList)
    if (!is.null(voteList$votePerParty)) voteList$votePerParty <- droplevels(voteList$votePerParty)
  }

  return(voteList)
}



# filter ------------------------------------------------------------------

#' Filter a voteList object
#'
#' @param .data A voteList object.
#' @param ... Logical expressions passed to [dplyr::filter()], evaluated
#'   against the sub-table specified by \code{.table}. Multiple conditions are
#'   combined with \code{&}.
#' @param .table Name of the sub-table to filter on. One of \code{"metaList"}
#'   (default), \code{"voteList"}, \code{"votePerParty"}, \code{"sponsorList"},
#'   or \code{"categoryList"}. The matching \code{id}s are then used to subset
#'   all other sub-tables.
#' @param drop.levels If \code{TRUE} (default), unused factor levels are
#'   dropped from all sub-tables after filtering.
#' @return A voteList object containing only the votes whose rows in
#'   \code{.table} match the filter conditions.
#' @importFrom dplyr filter
#' @export
#' @examples
#' # Filter on metaList (default)
#' dplyr::filter(examplevotes, date > as.Date("2010-01-15"))
#'
#' # Filter on sponsorList to keep only votes with a specific sponsor party
#' dplyr::filter(examplevotes, sponsorParty == "VVD", .table = "sponsorList")
filter.voteList <- function(.data, ..., .table = "metaList", drop.levels = TRUE) {
  valid_tables <- c("metaList", "voteList", "votePerParty", "sponsorList", "categoryList")
  if (!.table %in% valid_tables) {
    stop("`.table` must be one of: ", paste(valid_tables, collapse = ", "))
  }

  selected_ids <- dplyr::filter(.data[[.table]], ...)$id

  out <- .data
  out$metaList     <- out$metaList[out$metaList$id %in% selected_ids, ]
  out$voteList     <- out$voteList[out$voteList$id %in% selected_ids, ]
  out$voteMatrix   <- out$voteMatrix[out$voteMatrix$id %in% selected_ids, ]
  out$sponsorList  <- out$sponsorList[out$sponsorList$id %in% selected_ids, ]
  out$categoryList <- out$categoryList[out$categoryList$id %in% selected_ids, ]
  out$votePerParty <- out$votePerParty[out$votePerParty$id %in% selected_ids, ]

  if (drop.levels) {
    out$metaList     <- droplevels(out$metaList)
    out$voteList     <- droplevels(out$voteList)
    out$voteMatrix   <- droplevels(out$voteMatrix)
    if (!is.null(out$sponsorList))  out$sponsorList  <- droplevels(out$sponsorList)
    if (!is.null(out$categoryList)) out$categoryList <- droplevels(out$categoryList)
    if (!is.null(out$votePerParty)) out$votePerParty <- droplevels(out$votePerParty)
  }

  return(out)
}

#' Filter a questionList object
#'
#' @param .data A questionList object.
#' @param ... Logical expressions passed to [dplyr::filter()], evaluated
#'   against the sub-table specified by \code{.table}. Multiple conditions are
#'   combined with \code{&}.
#' @param .table Name of the sub-table to filter on. One of \code{"metaList"}
#'   (default), \code{"questionerList"}, \code{"responderList"}, or
#'   \code{"categoryList"}. The matching \code{dcIdentifier}s are then used to
#'   subset all other sub-tables.
#' @param drop.levels If \code{TRUE} (default), unused factor levels are
#'   dropped from all sub-tables after filtering.
#' @return A questionList object containing only the questions whose rows in
#'   \code{.table} match the filter conditions.
#' @importFrom dplyr filter
#' @export
#' @examples
#' # Filter on metaList (default)
#' dplyr::filter(examplequestions, dateQuestion > as.Date("2010-01-04"))
#'
#' # Filter on responderList to keep only questions answered by a specific party
#' dplyr::filter(examplequestions, responderParty == "VVD", .table = "responderList")
filter.questionList <- function(.data, ..., .table = "metaList", drop.levels = TRUE) {
  valid_tables <- c("metaList", "questionerList", "responderList", "categoryList")
  if (!.table %in% valid_tables) {
    stop("`.table` must be one of: ", paste(valid_tables, collapse = ", "))
  }

  selected_ids <- dplyr::filter(.data[[.table]], ...)$dcIdentifier

  out <- .data
  out$metaList        <- out$metaList[out$metaList$dcIdentifier %in% selected_ids, ]
  out$questionerList  <- out$questionerList[out$questionerList$dcIdentifier %in% selected_ids, ]
  out$responderList   <- out$responderList[out$responderList$dcIdentifier %in% selected_ids, ]
  out$categoryList    <- out$categoryList[out$categoryList$dcIdentifier %in% selected_ids, ]

  if (drop.levels) {
    out$metaList       <- droplevels(out$metaList)
    out$questionerList <- droplevels(out$questionerList)
    out$responderList  <- droplevels(out$responderList)
    out$categoryList   <- droplevels(out$categoryList)
  }

  return(out)
}


#' Subset questionList object
#'
#' @param x A questionList object, most of the time the questions object from the Dutch Parliamentary Behaviour Dataset.
#' @param df The name of the data.frame in the questionList to filter on. Options include metaList, questionerList, responderList, and categoryList.
#' @param subset The subset command.
#' @param select Expression, indicating columns to select from data frame
#' @param drop passed on to [ indexing operator
#' @param drop.levels If true, superfluous levels in the data.frames will be removed.
#' @param ... Other parameters (ignored)
#' @return The subsetted questionList object.
#' @export
#' @examples
#' subset(examplequestions, examplequestions$metaList, dateQuestion > as.Date("2010-01-04"))
subset.questionList <- function(x, df, subset, select, drop = FALSE, drop.levels = TRUE, ...) {
  if (missing(subset)) {
    r <- TRUE
  } else {
    e <- substitute(subset)
    r <- eval(e, df, parent.frame())
    if (!is.logical(r)) {
      stop("'subset' must evaluate to logical")
    }
    r <- r & !is.na(r)
  }
  if (missing(select)) {
    vars <- TRUE
  } else {
    nl <- as.list(seq_along(df))
    names(nl) <- names(df)
    vars <- eval(substitute(select), nl, parent.frame())
  }
  ss <- df[r, vars, drop = drop]
  select_ids <- ss$dcIdentifier

  questionList <- x
  questionList$metaList <- questionList$metaList[questionList$metaList$dcIdentifier %in% select_ids, ]
  questionList$questionerList <- questionList$questionerList[questionList$questionerList$dcIdentifier %in% select_ids, ]
  questionList$responderList <- questionList$responderList[questionList$responderList$dcIdentifier %in% select_ids, ]
  questionList$categoryList <- questionList$categoryList[questionList$categoryList$dcIdentifier %in% select_ids, ]

  if (drop.levels) {
    questionList$metaList <- droplevels(questionList$metaList)
    questionList$questionerList <- droplevels(questionList$questionerList)
    questionList$responderList <- droplevels(questionList$responderList)
    questionList$categoryList <- droplevels(questionList$categoryList)
  }

  return(questionList)
}


#' Select a random number of votes from a voteList object
#'
#' @param voteList A voteList object
#' @param size Size of random selection.
#' @return The subsetted voteList object.
#' @export
#' @examples
#' randomvotes(examplevotes)
randomvotes <- function(voteList, size = 10) {
  n <- nrow(voteList$metaList)
  selected <- sample.int(n, size)
  ids <- voteList$metaList$id[selected]
  out <- dplyr::filter(voteList, id %in% ids)
  return(out)
}
