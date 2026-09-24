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
  `"voteList"`, `"votePerParty"`, `"sponsorList"`, or `"categoryList"`.
  The matching `id`s are then used to subset all other sub-tables.

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

# Filter on sponsorList to keep only votes with a specific sponsor party
dplyr::filter(examplevotes, sponsorParty == "VVD", .table = "sponsorList")
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
