# Calculate cosponsorship between MPs

Calculate cosponsorship between MPs

## Usage

``` r
cosponsors(voteList, partylevel = FALSE)
```

## Arguments

- voteList:

  A voteList object, most of the time the votes objects that comes with
  the dutchparl package.

- partylevel:

  Boolean, default = FALSE. Analyze cosponsorship at the party level.

## Value

A list with cosponsorship data in which each row represents a pair of
MPs.

## Details

Note that this operation can be expensive for very large datasets. It is
recommended to limit the data to one parliamentary term.

## Examples

``` r
cosponsors(examplevotes)
#> # A tibble: 1,600 × 9
#>    MP1.id MP1.name   MP1.party MP2.id MP2.name MP2.party nCosponsor totalSponsor
#>    <chr>  <chr>      <chr>     <chr>  <chr>    <chr>          <dbl>        <dbl>
#>  1 02226  Agnes Kant SP        02218  Ineke v… GroenLin…          0            1
#>  2 02226  Agnes Kant SP        02455  Margot … PvdA               0            1
#>  3 02226  Agnes Kant SP        03124  Mariann… SP                 0            1
#>  4 02226  Agnes Kant SP        03058  Madelei… CDA                0            1
#>  5 02226  Agnes Kant SP        03114  Ed Anker Christen…          0            1
#>  6 02226  Agnes Kant SP        02682  Bas van… SGP                0            1
#>  7 02226  Agnes Kant SP        02416  Ineke D… VVD                0            1
#>  8 02226  Agnes Kant SP        03183  Richard… PVV                0            1
#>  9 02226  Agnes Kant SP        02993  Alexand… D66                1            1
#> 10 02226  Agnes Kant SP        02220  Femke H… GroenLin…          1            1
#> # ℹ 1,590 more rows
#> # ℹ 1 more variable: percCosponsor <dbl>
```
