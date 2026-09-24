# Calculate rice score

Calculate rice score

## Usage

``` r
rice(x, ...)

# S3 method for class 'voteList'
rice(x, minvotes = 10, ...)
```

## Arguments

- x:

  A voteList object

- ...:

  Other parameters passed on.

- minvotes:

  The minimum number of votes for a party to have participated in.
  Defaults to 10.

## Value

A list of rice scores.

## Methods (by class)

- `rice(voteList)`: Rice index for voteList object

## Examples

``` r
rice(examplevotes)
#>           party rice_mean
#> 1           CDA         1
#> 2  ChristenUnie         1
#> 3           D66         1
#> 4    GroenLinks         1
#> 5          PvdA         1
#> 6          PvdD         1
#> 7           PVV         1
#> 8           SGP         1
#> 9            SP         1
#> 10      Verdonk         1
#> 11          VVD         1
```
