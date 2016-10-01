#' Download Dutch Parliamentary Behaviour Dataset from the internet
#'
#' @return voteList object
#' @param url URL to download from. Defaults to downloading from dutchparl dataverse.
#' @export
#' @examples
#' \dontrun{
#' downloadVotes()
#' }
downloadVotes <- function(url=NULL) {
  if(is.null(url)) url <- "https://dl.dropboxusercontent.com/u/31727287/Other/2016/votes.RData"
  utils::download.file(url, destfile="votes.RData")
}
