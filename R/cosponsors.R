#' Calculate cosponsorship between MPs
#'
#' @param voteList A voteList object, most of the time the votes objects that comes with the dutchparl package.
#' @param partylevel Boolean, default = FALSE. Analyze cosponsorship at the party level.
#' @return A list with cosponsorship data in which each row represents a pair of MPs.
#' @details Note that this operation can be expensive for very large datasets. It is recommended to limit the data to one parliamentary term.
#' @importFrom magrittr "%>%"
#' @importFrom stats "setNames"
#' @importFrom rlang .data
#' @export
#' @examples
#' cosponsors(examplevotes)
cosponsors <- function(voteList, partylevel = FALSE) {
  if (partylevel == FALSE) {
    # Create list with sponsors
    sponsors <- voteList$sponsorList %>%
      dplyr::mutate(isSponsor = 1) %>%
      dplyr::select(dplyr::any_of(c("id", "sponsorName", "sponsorId", "sponsorParty", "isSponsor")))

    # Create matrix from this
    sponsorMatrix <- tidyr::pivot_wider(sponsors,
      names_from = "id",
      values_from = "isSponsor",
      values_fill = 0
    )

    # Matrix operation to calculate co-sponsorship
    ninfo <- ncol(sponsors) - 2
    coSponsor <- as.matrix(sponsorMatrix[, -c(1:ninfo)]) %*% as.matrix(t(sponsorMatrix[, -c(1:ninfo)]))
    colnames(coSponsor) <- 1:ncol(coSponsor)

    # Add some information to this matrix
    coSponsor <- coSponsor %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "MP1number")

    coSponsor <- tidyr::pivot_longer(
      data = as.data.frame(coSponsor),
      cols = !dplyr::contains("MP1number"),
      names_to = "MP2number",
      values_to = "nCosponsor"
    )

    # Create dataframe combining numbers with names
    sponsorNames <- sponsorMatrix %>%
      dplyr::select(c("sponsorName", "sponsorId", "sponsorParty")) %>%
      dplyr::mutate(MPnumber = as.character(1:dplyr::n()))


    # Merge coSponsor with names for MP1
    coSponsor <- dplyr::left_join(coSponsor, sponsorNames,
      by = c("MP1number" = "MPnumber")
    ) %>%
      dplyr::rename("MP1.name" = "sponsorName") %>%
      dplyr::rename("MP1.id" = "sponsorId") %>%
      dplyr::rename("MP1.party" = "sponsorParty")

    # Merge coSponsor with names for MP2
    coSponsor <- dplyr::left_join(coSponsor, sponsorNames,
      by = c("MP2number" = "MPnumber")
    ) %>%
      dplyr::rename("MP2.name" = "sponsorName") %>%
      dplyr::rename("MP2.id" = "sponsorId") %>%
      dplyr::rename("MP2.party" = "sponsorParty")


    # Select only relevant variables and order
    coSponsor <- coSponsor %>%
      dplyr::select(dplyr::all_of(c(
        "MP1.id", "MP1.name", "MP1.party",
        "MP2.id", "MP2.name", "MP2.party",
        "nCosponsor"
      )))

    # Create dataframe with total number of proposals sponsored
    # (= number of proposals coSponsored with oneself)
    totalSponsor <- coSponsor %>%
      dplyr::filter(.data$MP1.id == .data$MP2.id & .data$MP1.party == .data$MP2.party) %>%
      dplyr::select(dplyr::all_of(c(
        "MP1.id", "MP1.name", "MP1.party",
        "nCosponsor"
      ))) %>%
      dplyr::rename("totalSponsor" = "nCosponsor")

    # Join coSponsor and totalSponsor and add percent Sponsorship match
    coSponsor <- dplyr::left_join(coSponsor, totalSponsor,
      by = c("MP1.id", "MP1.name", "MP1.party")
    ) %>%
      dplyr::mutate(percCosponsor = .data$nCosponsor / .data$totalSponsor) %>%
      dplyr::arrange("MP1.name")

    return(coSponsor)
  } else {
    # Create list with party sponsors / remove duplicates (multiple MPs from one)
    # party cosponsor
    sponsors <- voteList$sponsorList %>%
      dplyr::mutate(isSponsor = 1) %>%
      dplyr::select(dplyr::all_of(c("id", "sponsorParty", "isSponsor"))) %>%
      dplyr::distinct()

    # Create matrix from this
    sponsorMatrix <- tidyr::pivot_wider(sponsors,
      names_from = "id",
      values_from = "isSponsor",
      values_fill = 0
    )

    # Matrix operation to calculate co-sponsorship
    ninfo <- ncol(sponsors) - 2
    coSponsor <- as.matrix(sponsorMatrix[, -c(1:ninfo)]) %*% as.matrix(t(sponsorMatrix[, -c(1:ninfo)]))
    colnames(coSponsor) <- 1:ncol(coSponsor)

    # Add some information to this matrix
    coSponsor <- coSponsor %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Party1number")

    #
    coSponsor <- tidyr::pivot_longer(
      data = as.data.frame(coSponsor),
      cols = !dplyr::contains("Party1number"),
      names_to = "Party2number",
      values_to = "nCosponsor"
    )


    # Create dataframe combining numbers with names
    sponsorNames <- sponsorMatrix %>%
      dplyr::select(c("sponsorParty")) %>%
      dplyr::mutate(Partynumber = as.character(1:dplyr::n()))


    # Merge coSponsor with party Names
    coSponsor <- dplyr::left_join(coSponsor, sponsorNames,
      by = c("Party1number" = "Partynumber")
    ) %>%
      dplyr::rename("Party1" = "sponsorParty")

    # Merge coSponsor with party names for party 2
    coSponsor <- dplyr::left_join(coSponsor, sponsorNames,
      by = c("Party2number" = "Partynumber")
    ) %>%
      dplyr::rename("Party2" = "sponsorParty")


    # Select only relevant variables and order
    coSponsor <- coSponsor %>%
      dplyr::select(dplyr::all_of(c("Party1", "Party2", "nCosponsor")))


    # Create dataframe with total number of proposals sponsored
    # (= number of proposals coSponsored with oneself)
    totalSponsor <- coSponsor %>%
      dplyr::filter(.data$Party1 == .data$Party2) %>%
      dplyr::select(dplyr::all_of(c("Party1", "nCosponsor"))) %>%
      dplyr::rename("totalSponsor" = "nCosponsor")


    # Join coSponsor and totalSponsor and add percent Sponsorship match
    coSponsor <- dplyr::left_join(coSponsor, totalSponsor,
      by = c("Party1")
    ) %>%
      dplyr::mutate(percCosponsor = .data$nCosponsor / .data$totalSponsor) %>%
      dplyr::arrange("Party1")

    return(coSponsor)
  }
}
