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
