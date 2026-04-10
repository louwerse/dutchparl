#' Calculate cosponsorship between MPs
#'
#' @param voteList A voteList object, most of the time the votes objects that comes with the dutchparl package.
#' @return A list with cosponsorship data in which each row represents a pair of MPs.
#' @details Note that this operation can be expensive for very large datasets. It is recommended to limit the data to one parliamentary term.
#' @importFrom magrittr "%>%"
#' @importFrom stats "setNames"
#' @export
#' @examples
#' cosponsors(examplevotes)
cosponsors <- function(voteList) {

  # Create list with sponsors
  sponsors <- voteList$sponsorList %>%
    dplyr::mutate(isSponsor=1) %>%
    dplyr::select_("-sponsorNumber")

  # Create matrix from this
  sponsorMatrix <- tidyr::spread_(sponsors, "id", "isSponsor", fill=0)

  # Matrix operation to calculate co-sponsorship
  coSponsor <- as.matrix(sponsorMatrix[,-c(1:3)]) %*% as.matrix(t(sponsorMatrix[,-c(1:3)]))

  # Add some information to this matrix
  coSponsor <- coSponsor %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var="MP1number")

  #
  coSponsor <- tidyr::gather_(as.data.frame(coSponsor),
                             "MP2number", "nCosponsor", colnames(coSponsor)[-1])

  # Create dataframe combining numbers with names
  sponsorNames <- sponsorMatrix %>%
    dplyr::select_(.dots=c("sponsorName", "sponsorId", "sponsorParty")) %>%
    dplyr::mutate_(.dots=setNames(list("as.character(1:n())"),"MPnumber"))


  # Merge coSponsor with names for MP1
  coSponsor <- dplyr::left_join(coSponsor, sponsorNames,
                                by=c("MP1number"="MPnumber")) %>%
    dplyr::rename_(.dots=setNames("sponsorName", "MP1.name")) %>%
    dplyr::rename_(.dots=setNames("sponsorId", "MP1.id")) %>%
    dplyr::rename_(.dots=setNames("sponsorParty", "MP1.party"))

  # Merge coSponsor with names for MP2
  coSponsor <- dplyr::left_join(coSponsor, sponsorNames,
                                by=c("MP2number"="MPnumber")) %>%
    dplyr::rename_(.dots=setNames("sponsorName", "MP2.name")) %>%
    dplyr::rename_(.dots=setNames("sponsorId", "MP2.id")) %>%
    dplyr::rename_(.dots=setNames("sponsorParty", "MP2.party"))


  # Select only relevant variables and order
  coSponsor <- coSponsor %>%
    dplyr::select_(.dots=list("MP1.id", "MP1.name", "MP1.party",
                  "MP2.id", "MP2.name", "MP2.party",
                  "nCosponsor"))

  # Create dataframe with total number of proposals sponsored
  # (= number of proposals coSponsored with oneself)
  totalSponsor <- coSponsor %>%
    dplyr::filter_(.dots="MP1.id==MP2.id & MP1.party==MP2.party") %>%
    dplyr::select_(.dots=list("MP1.id", "MP1.name", "MP1.party",
                              "nCosponsor")) %>%
    dplyr::rename_(.dots=setNames("nCosponsor", "totalSponsor"))

  # Join coSponsor and totalSponsor and add percent Sponsorship match
  coSponsor <- dplyr::left_join(coSponsor, totalSponsor,
                                by = c("MP1.id", "MP1.name", "MP1.party")) %>%
    dplyr::mutate_(.dots=setNames("nCosponsor / totalSponsor", "percCosponsor")) %>%
    dplyr::arrange_(.dots="MP1.name")

  return(coSponsor)
}
