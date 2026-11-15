# Filter a voteList object

Filter a voteList object

## Usage

``` r
# S3 method for class 'voteList'
filter(.data, ..., .table = "metaList", drop.levels = TRUE)
```

## Arguments

- .data:

  A voteList object.

- ...:

  Logical expressions passed to \[dplyr::filter()\], evaluated against
  the sub-table specified by `.table`. Multiple conditions are combined
  with `&`.

- .table:

  Name of the sub-table to filter on. One of `"metaList"` (default),
  `"voteList"`, `"votePerParty"`, `"sponsorList"`, `"categoryList"`, or
  (REPDEM data) `"voteCabinet"`. The matching `id`s are then used to
  subset all other sub-tables.

- drop.levels:

  If `TRUE` (default), unused factor levels are dropped from all
  sub-tables after filtering.

## Value

A voteList object containing only the votes whose rows in `.table` match
the filter conditions.

## Examples

``` r
# Filter on metaList (default)
dplyr::filter(examplevotes, date > as.Date("2010-01-15"))
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

# Filter on sponsorList to keep only votes with a specific sponsor party
dplyr::filter(examplevotes, sponsorParty == "VVD", .table = "sponsorList")
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
