# Subset voteList object

Subset voteList object

## Usage

``` r
# S3 method for class 'voteList'
subset(x, df, subset, select, drop = FALSE, drop.levels = TRUE, ...)
```

## Arguments

- x:

  A voteList object, most of the time the votes object from the Dutch
  Parliamentary Behaviour Dataset.

- df:

  The name of the data.frame in the voteList to filter on. Options
  include metaList, sponsorList, and categoryList.

- subset:

  The subset command.

- select:

  Expression, indicating columns to select from data frame

- drop:

  passed on to \[ indexing operator

- drop.levels:

  If true, superfluous levels in the data.frames will be removed.

- ...:

  Other parameters (ignored)

## Value

The subsetted voteList object.

## Examples

``` r
subset(examplevotes, examplevotes$metaList, date > as.Date("2010-01-15"))
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
