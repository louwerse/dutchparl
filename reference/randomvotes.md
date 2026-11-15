# Select a random number of votes from a voteList object

Select a random number of votes from a voteList object

## Usage

``` r
randomvotes(voteList, size = 10)
```

## Arguments

- voteList:

  A voteList object

- size:

  Size of random selection.

## Value

The subsetted voteList object.

## Examples

``` r
randomvotes(examplevotes)
#>                   Length Class      Mode
#> datasetInfo        5     tbl_df     list
#> metaList          15     tbl_df     list
#> voteList           7     tbl_df     list
#> categoryList       5     tbl_df     list
#> sponsorList        7     tbl_df     list
#> cabinetInfo       43     tbl_df     list
#> partyElectionInfo 15     tbl_df     list
#> partyInfo         20     tbl_df     list
#> electionInfo       2     tbl_df     list
#> votePerParty      11     data.frame list
#> partyCabinetInfo  30     tbl_df     list
#> voteMatrix        12     tbl_df     list
#> cabinetPeriods    11     tbl_df     list
#> voteCabinet        6     tbl_df     list
```
