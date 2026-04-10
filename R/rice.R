
# rice --------------------------------------------------------------------
rice.default <- function(x, ...) {}

#' Calculate rice score
#'
#' @param x A voteList object
#' @param minvotes The minimum number of votes for a party to have participated in. Defaults to 10.
#' @return A list of rice scores.
#' @param ... Other parameters passed on.
#' @importFrom magrittr "%>%"
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
  x$votePerParty <- x$votePerParty %>%
    dplyr::filter_(.dots = lazyeval::interp(~ x %in% y,
      .values = list(
        x = as.name("party"),
        y = includeParties
      )
    ))

  # If votePerParty variables are named 0, 1, 8 instead of vote_0, vote_1, vote_8, fix:
  if (is.null(x$votePerParty$`vote_0`) & (!is.null(x$votePerParty$`0`))) {
    x$votePerParty$`vote_0` <- x$votePerParty$`0`
  }
  if (is.null(x$votePerParty$`vote_1`) & (!is.null(x$votePerParty$`1`))) {
    x$votePerParty$`vote_1` <- x$votePerParty$`1`
  }
  if (is.null(x$votePerParty$`vote_8`) & (!is.null(x$votePerParty$`8`))) {
    x$votePerParty$`vote_8` <- x$votePerParty$`8`
  }

  x$votePerParty$rice <- abs(x$votePerParty$`vote_1` - x$votePerParty$`vote_0`) / (x$votePerParty$`vote_1` + x$votePerParty$`vote_0`)

  rice_out <- x$votePerParty %>%
    dplyr::group_by_(.dots = "party") %>%
    dplyr::summarise_(.dots = setNames("mean(rice, na.rm=TRUE)", "rice_mean"))
  return(as.data.frame(rice_out))
}
