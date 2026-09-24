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
#> metaList          12     data.frame list
#> voteList           7     data.frame list
#> categoryList       5     data.frame list
#> sponsorList        5     data.frame list
#> voteMatrix        12     data.frame list
#> votePerParty       5     data.frame list
#> partyInfo         24     data.frame list
#> cabinetInfo        6     data.frame list
#> partyCabinetInfo  28     data.frame list
#> partyElectionInfo 12     data.frame list
#> electionInfo       2     data.frame list
```
