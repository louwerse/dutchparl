
as.rollcall.default <- function(x, ...) {}

#' Transform into rollcall object
#'
#' @param x Object to be transformed
#' @return Rollcall object from package 'pscl'
#' @param yea numeric, possibly a vector, code(s) for a Yea vote. Default is 1.
#' @param nay numeric, possibly a vector, code(s) for a Nay vote. Default is 0.
#' @param missing numeric or NA, possibly a vector, code(s) for missing data. Default is NA.
#' @param notInLegis numeric or NA, possibly a vector, code(s) for the legislator not being in the legislature when a particular roll call was recorded (e.g., deceased, retired, yet to be elected).
#' @param ... Other parameters (ignored)
#' @export
#' @examples
#' as.rollcall(examplevotes)
as.rollcall <- function(x, ...) {
  UseMethod("as.rollcall")
}

#' @describeIn as.rollcall Transform voteList object into rollcall object
#' @export
as.rollcall.voteList <- function(x, yea=1, nay=0, missing=c(3,7,8), notInLegis = 9, ...) {

  # Delete any entries that have all missing values
  y <- x$voteMatrix[, c(1, which(!colSums(x$voteMatrix[, -1]) == nrow(x$voteMatrix) * 9) + 1)]

  # If there is an (Unknown) column, exclude it from the rollcall object
  y$`(Unknown)` <- NULL

  # Set vote names
  vote.names <- y$id

  # Construct vote.data, and transform to data.frame to avoid warning about setting rownames (by pscl)
  meta <- x$metaList
  meta <- meta[which(meta$id %in% y$id), ] # only select votes which are in the voteList
  meta <- data.frame(meta)

  y <- y[, -1]
  legis.names <- colnames(y)

  rc <-
    pscl::rollcall(
      t(y),
      yea = yea,
      nay = nay,
      missing = missing,
      notInLegis = notInLegis,
      legis.names = legis.names,
      vote.names = vote.names,
      vote.data = meta,
      desc = "Parliamentary Voting Data",
      source = paste(unique(meta$source), collapse = "/")
    )
  return(rc)
}
