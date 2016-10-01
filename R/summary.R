
#' @export
summary.voteList <- function(object, ...) {
  voteList <- object
  dat <- list()
  dat$nVotes <- nrow(voteList$metaList)

  dat$categories <- table(voteList$categoryList$category)

  dat$parties_min <- min(nrow(voteList$metaList), 100)

  dat$parties <- names(table(voteList$voteList$party)[which(table(voteList$voteList$party) >= dat$parties_min)])

  class(dat) <- "summary.voteList"
  return(dat)
}

#' @export
print.summary.voteList <- function(x, digits=max(3L, getOption("digits") - 3L), ...) {

  cat("Number of votes in dataset: ", x$nVotes, "\n")

  cat("Category count: \n")
  print(x$categories)

  cat("Parties appearing at least ", x$parties_min, " times in the dataset:\n")
  print(x$parties)
}

#' @export
print.voteList <- function(x, verbose=FALSE, ...) {

  if(verbose) print.default(x)
  if(!verbose) print(summary.default(x))

}
