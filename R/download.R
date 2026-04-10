#' Download Dutch Parliamentary Behaviour Vote Dataset from the internet
#'
#' @return Nothing. Side effect is to download the file.
#' @param url URL to download from. Defaults to downloading from dutchparl dataverse.
#' @param destfile Destination file (rds).
#' @export
#' @examples
#' \dontrun{
#' downloadVotes()
#' }
downloadVotes <- function(url=NULL,destfile="votes.rds") {
  cat("Creative Commons Attribution 4.0 International license applies. See: https://creativecommons.org/licenses/by/4.0/legalcode.\r\n")
  if(is.null(url)) url <- "https://dataverse.harvard.edu/api/access/datafile/4279819?gbrecs=true"
  utils::download.file(url, destfile=destfile, mode="wb")
}

#' Download Dutch Parliamentary Behaviour Questions Dataset from the internet
#'
#' @return Nothing. Side effect is to download the file.
#' @param url URL to download from. Defaults to downloading from dutchparl dataverse.
#' @param destfile Destination file (rds).
#' @export
#' @examples
#' \dontrun{
#' downloadQuestions()
#' }
downloadQuestions <- function(url=NULL,destfile="questions.rds") {
  cat("Creative Commons Attribution 4.0 International license applies. See: https://creativecommons.org/licenses/by/4.0/legalcode.\r\n")
  if(is.null(url)) url <- "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/5EEXZY/CQFKD9&version=1.0"
  utils::download.file(url, destfile=destfile, mode="wb")
}





