
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
rice <- function(x, ...)
  UseMethod("rice")

#' @describeIn rice Rice index for voteList object
#' @export
rice.voteList <- function(x, minvotes=10, ...) {

  includeParties <- names(which(table(x$votePerParty$party) > minvotes))
  x$votePerParty <- x$votePerParty %>%
    dplyr::filter_(.dots=lazyeval::interp(~ x %in% y,
                                          .values=list(x = as.name("party"),
                                                       y = includeParties)))

  x$votePerParty$rice <- abs(x$votePerParty$`1` - x$votePerParty$`0`) / (x$votePerParty$`1` + x$votePerParty$`0`)

  rice_out <- x$votePerParty %>%
    dplyr::group_by_(.dots="party") %>%
    dplyr::summarise_(.dots=setNames("mean(rice, na.rm=TRUE)", "rice_mean"))
  return(as.data.frame(rice_out))
}
