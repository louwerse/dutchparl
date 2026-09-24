# Add information on cabinet composition and elections to metadata. This data is obtained from ParlGov (http://www.parlgov.org/).

Add information on cabinet composition and elections to metadata. This
data is obtained from ParlGov (http://www.parlgov.org/).

## Usage

``` r
addCabinetInfo(x, ...)

# S3 method for class 'voteList'
addCabinetInfo(x, ...)

# S3 method for class 'questionList'
addCabinetInfo(x, ...)
```

## Arguments

- x:

  A voteList or questionList object

- ...:

  Other parameters passed on.

## Value

A voteList or questionList object

## Methods (by class)

- `addCabinetInfo(voteList)`: Cabinet Information for voteList object

- `addCabinetInfo(questionList)`: Cabinet Information for questionList
  object. Information is valid for the date of the response to the
  question

## Examples

``` r
addCabinetInfo(examplevotes)
#>                   Length Class      Mode
#> metaList          18     data.frame list
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
