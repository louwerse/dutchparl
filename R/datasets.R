#' Dutch Parliamentary Behaviour Dataset (Votes)
#'
#' Votes taken in the Dutch lower house in January 2010, with cabinet,
#' election and party information from REPDEM (and ParlGov for 1918-1946).
#'
#' The object has class \code{c("voteListRepdem", "voteList")} and contains
#' the sub-tables \code{datasetInfo}, \code{metaList}, \code{voteList},
#' \code{voteMatrix}, \code{votePerParty}, \code{sponsorList},
#' \code{categoryList}, \code{cabinetInfo}, \code{cabinetPeriods},
#' \code{voteCabinet}, \code{electionInfo}, \code{partyElectionInfo},
#' \code{partyCabinetInfo} and \code{partyInfo}.
#'
#' \code{cabinetInfo} has one row per cabinet. Caretaker (demissionair)
#' status is recorded per cabinet period in \code{cabinetPeriods} and per
#' vote in \code{voteCabinet}; \code{cabinetInfo$caretaker} only marks
#' interim cabinets. See \code{\link{addCabinetInfo}}.
#'
#' @docType data
#' @keywords datasets
#' @name examplevotes
#' @usage data(examplevotes)
#' @format A voteList object
#' @seealso \code{\link{examplevotes_legacy}} for the same votes with ParlGov data.
"examplevotes"

#' Dutch Parliamentary Behaviour Dataset (Votes, legacy ParlGov version)
#'
#' Votes taken in the Dutch lower house in January 2010, with cabinet,
#' election and party information from ParlGov, as in earlier releases of the
#' dataset.
#'
#' The object has class \code{voteList}. \code{cabinetInfo} records caretaker
#' spells as separate cabinets, flagged by \code{cabinetInfo$caretaker}. There
#' are no \code{cabinetPeriods} or \code{voteCabinet} tables.
#'
#' @docType data
#' @keywords datasets
#' @name examplevotes_legacy
#' @usage data(examplevotes_legacy)
#' @format A voteList object
#' @seealso \code{\link{examplevotes}} for the same votes with REPDEM data.
"examplevotes_legacy"

#' Dutch Parliamentary Behaviour Dataset (Questions)
#'
#' Written questions from Members of the Dutch lower house, asked 4-5 January
#' 2010, with cabinet, election and party information from REPDEM (and
#' ParlGov for 1918-1946).
#'
#' The object has class \code{c("questionListRepdem", "questionList")}. Besides
#' \code{metaList}, \code{questionerList}, \code{responderList},
#' \code{categoryList} and \code{questionText}, it contains the same cabinet,
#' election and party tables as \code{\link{examplevotes}}, with
#' \code{questionCabinet} (cabinet and caretaker status at the date of the
#' response) in place of \code{voteCabinet}.
#'
#' @docType data
#' @keywords datasets
#' @name examplequestions
#' @usage data(examplequestions)
#' @format A questionList object
#' @seealso \code{\link{examplequestions_legacy}} for the same questions with ParlGov data.
"examplequestions"

#' Dutch Parliamentary Behaviour Dataset (Questions, legacy ParlGov version)
#'
#' Written questions from Members of the Dutch lower house, asked 4-5 January
#' 2010, with cabinet, election and party information from ParlGov, as in
#' earlier releases of the dataset. There are no \code{cabinetPeriods} or
#' \code{questionCabinet} tables.
#'
#' @docType data
#' @keywords datasets
#' @name examplequestions_legacy
#' @usage data(examplequestions_legacy)
#' @format A questionList object
#' @seealso \code{\link{examplequestions}} for the same questions with REPDEM data.
"examplequestions_legacy"
