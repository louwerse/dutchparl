# voteSame ----------------------------------------------------------------
votesame.default <- function(x) {}

#' Calculate the percentate of votes in which parties voted the same
#'
#' @param x A rollcall or voteList object.
#' @param order.x Character matrix of legislator names. This is used to re-order legislator (or party) columns, if desired.
#' @return A matrix of voting similarities
#' @param ... Other parameters passed on.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' votesame(examplevotes)
votesame <- function(x, ...)
  UseMethod("votesame")

#' @describeIn votesame Votesame for rollcall object
#' @export
votesame.rollcall = function(x, order.x = NULL, ...) {
  z <- x
  x <- t(x$votes)
  if (!is.null(order.x)) x <- x[, order(order.x, 1:length(order.x), na.last = NA)]
  x = replace(x, x == z$codes$notInLegis, NA)
  x = replace(x, x %in% z$codes$missing, NA)
  y = matrix(NA, ncol(x), ncol(x))
  rownames(y) = colnames(y) = colnames(x)

  for (i in 1:ncol(y)) {
    for (j in 1:ncol(y)) {
      y[i, j] = sum(x[, i] == x[, j], na.rm = TRUE) / sum(!is.na(x[, i]) & !is.na(x[, j]), na.rm = TRUE)
    }
  }
  return(y)
}

#' @describeIn votesame Votesame for voteList object
#' @export
votesame.voteList = function(x, order.x = NULL, ...) {
  if (class(x) == "voteList") {
    return(votesame(as.rollcall(x), order.x = order.x, ...))
  } else {
    stop("x is not a voteList object")
  }
}
