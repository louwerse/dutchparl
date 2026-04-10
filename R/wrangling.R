#' Subset voteList object
#'
#' @param x A voteList object, most of the time the votes objects that comes with the dutchparl package.
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
subset.voteList <- function (x, df, subset, select, drop = FALSE, drop.levels=TRUE, ...)
{
  if (missing(subset))
    r <- TRUE
  else {
    e <- substitute(subset)
    r <- eval(e, df, parent.frame())
    if (!is.logical(r))
      stop("'subset' must evaluate to logical")
    r <- r & !is.na(r)
  }
  if (missing(select))
    vars <- TRUE
  else {
    nl <- as.list(seq_along(df))
    names(nl) <- names(df)
    vars <- eval(substitute(select), nl, parent.frame())
  }
  ss <- df[r, vars, drop = drop]
  select_ids <- ss$id

  voteList <- x
  voteList$metaList <- voteList$metaList[voteList$metaList$id %in% select_ids,]
  voteList$voteList <- voteList$voteList[voteList$voteList$id %in% select_ids,]
  voteList$voteMatrix <- voteList$voteMatrix[voteList$voteMatrix$id %in% select_ids,]
  voteList$sponsorList <- voteList$sponsorList[voteList$sponsorList$id %in% select_ids,]
  voteList$categoryList <- voteList$categoryList[voteList$categoryList$id %in% select_ids,]
  voteList$votePerParty <- voteList$votePerParty[voteList$votePerParty$id %in% select_ids,]

  if(drop.levels) {
    voteList$metaList <- droplevels(voteList$metaList)
    voteList$voteList <- droplevels(voteList$voteList)
    voteList$voteMatrix <- droplevels(voteList$voteMatrix)
    voteList$sponsorList <- droplevels(voteList$sponsorList)
    voteList$categoryList <- droplevels(voteList$categoryList)
    voteList$votePerParty <- droplevels(voteList$votePerParty)
  }

  return(voteList)
}

#' Select a random number of votes from a voteList object
#'
#' @param voteList A voteList object
#' @param size Size of random selection.
#' @return The subsetted voteList object.
#' @export
#' @examples
#' randomvotes(examplevotes)
randomvotes <- function(voteList, size=10) {
  n <- nrow(voteList$metaList)
  selected <- sample.int(n, size)
  ids <- voteList$metaList$id[selected]
  out <- subset(voteList, voteList$metaList, voteList$metaList$id %in% ids)
  return(out)
}

