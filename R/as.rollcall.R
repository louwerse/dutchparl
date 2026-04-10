
as.rollcall.default <- function(x, ...) {}

#' Transform into rollcall object
#'
#' @param x Object to be transformed
#' @return Rollcall object from package 'pscl'
#' @param ... Other parameters (ignored)
#' @export
#' @examples
#' as.rollcall(examplevotes)
as.rollcall <- function(x, ...)
  UseMethod("as.rollcall")

#' @describeIn as.rollcall Transform voteList object into rollcall object
#' @export
as.rollcall.voteList <- function(x, ...) {

  y <- x$voteMatrix[,c(1, which(!colSums(x$voteMatrix[,-1])==nrow(x$voteMatrix)*9)+1)]
  y$`(Unknown)` <- NULL
  vote.names <- y$id

  meta <- x$metaList
  meta <- meta[which(meta$id %in% y$id),] #only select votes which are in the voteList

  y <- y[,-1]
  legis.names <- colnames(y)

  rc <- pscl::rollcall(t(y), yea=1, nay=0, missing=c(7,8), notInLegis=9, legis.names=legis.names, vote.names=vote.names, vote.data=meta, desc="Dutch Parliamentary Voting Data", source="OB/SGD")
  return(rc)
}


