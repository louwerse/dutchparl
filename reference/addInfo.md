# Add information on cabinets, elections and parties to dataset. This data is obtained from ParlGov (http://www.parlgov.org/).

Add information on cabinets, elections and parties to dataset. This data
is obtained from ParlGov (http://www.parlgov.org/).

## Usage

``` r
addInfo(x, ...)

# S3 method for class 'voteList'
addInfo(x, ...)

# S3 method for class 'questionList'
addInfo(x, ...)
```

## Arguments

- x:

  A voteList object

- ...:

  Other parameters passed on.

## Value

A voteList object

## Details

This is a wrapper that runs both addCabinetInfo and addPartyInfo with
default settings.

## Methods (by class)

- `addInfo(voteList)`: Add information to voteList object

- `addInfo(questionList)`: Add information to questionList object

## Examples

``` r
examplevotes_with_info <- addInfo(examplevotes)
```
