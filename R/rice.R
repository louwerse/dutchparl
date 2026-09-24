# rice --------------------------------------------------------------------
#' @exportS3Method
rice.default <- function(x, ...) {}

#' Calculate rice score
#'
#' @param x A voteList object
#' @param minvotes The minimum number of votes for a party to have participated in. Defaults to 10.
#' @return A list of rice scores.
#' @param ... Other parameters passed on.
#' @importFrom stats "setNames"
#' @export
#' @examples
#' rice(examplevotes)
rice <- function(x, ...) {
  UseMethod("rice")
}

#' @describeIn rice Rice index for voteList object
#' @export
rice.voteList <- function(x, minvotes = 10, ...) {
  includeParties <- names(which(table(x$votePerParty$party) > minvotes))
  x$votePerParty <- x$votePerParty |>
    dplyr::filter(party %in% includeParties)

  # If votePerParty variables are named 0, 1, 8 instead of vote_0, vote_1, vote_8, fix:
  if (!("vote_0" %in% names(x$votePerParty)) && "0" %in% names(x$votePerParty)) {
    x$votePerParty$`vote_0` <- x$votePerParty$`0`
  }
  if (!("vote_1" %in% names(x$votePerParty)) && "1" %in% names(x$votePerParty)) {
    x$votePerParty$`vote_1` <- x$votePerParty$`1`
  }
  if (!("vote_8" %in% names(x$votePerParty)) && "8" %in% names(x$votePerParty)) {
    x$votePerParty$`vote_8` <- x$votePerParty$`8`
  }

  x$votePerParty$rice <- abs(x$votePerParty$`vote_1` - x$votePerParty$`vote_0`) / (x$votePerParty$`vote_1` + x$votePerParty$`vote_0`)

  rice_out <- x$votePerParty |>
    dplyr::summarise(rice_mean = mean(.data$rice, na.rm = TRUE), .by = "party")
  return(as.data.frame(rice_out))
}
