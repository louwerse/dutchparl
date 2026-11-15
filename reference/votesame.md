# Calculate the percentate of votes in which parties voted the same

Calculate the percentate of votes in which parties voted the same

## Usage

``` r
votesame(x, ...)

# S3 method for class 'rollcall'
votesame(x, order.x = NULL, ...)

# S3 method for class 'voteList'
votesame(x, order.x = NULL, ...)
```

## Arguments

- x:

  A rollcall or voteList object.

- ...:

  Other parameters passed on.

- order.x:

  Character matrix of legislator names. This is used to re-order
  legislator (or party) columns, if desired.

## Value

A matrix of voting similarities

## Methods (by class)

- `votesame(rollcall)`: Votesame for rollcall object

- `votesame(voteList)`: Votesame for voteList object

## Examples

``` r
votesame(examplevotes)
#>                    SGP      PvdA       VVD       D66       CDA        GL
#> SGP          1.0000000 0.6885246 0.7049180 0.5737705 0.7049180 0.6557377
#> PvdA         0.6885246 1.0000000 0.6229508 0.8524590 0.8852459 0.7377049
#> VVD          0.7049180 0.6229508 1.0000000 0.6065574 0.6393443 0.5245902
#> D66          0.5737705 0.8524590 0.6065574 1.0000000 0.8032787 0.8196721
#> CDA          0.7049180 0.8852459 0.6393443 0.8032787 1.0000000 0.6229508
#> GL           0.6557377 0.7377049 0.5245902 0.8196721 0.6229508 1.0000000
#> SP           0.5573770 0.6721311 0.4590164 0.6885246 0.5901639 0.8360656
#> ChristenUnie 0.7213115 0.8360656 0.5901639 0.7868852 0.8852459 0.7049180
#> PVV          0.5409836 0.4262295 0.7377049 0.4754098 0.4426230 0.3934426
#> PvdD         0.5573770 0.7049180 0.4590164 0.7213115 0.5901639 0.8688525
#> Verdonk      0.5901639 0.5409836 0.7868852 0.5573770 0.5245902 0.5409836
#>                     SP ChristenUnie       PVV      PvdD   Verdonk
#> SGP          0.5573770    0.7213115 0.5409836 0.5573770 0.5901639
#> PvdA         0.6721311    0.8360656 0.4262295 0.7049180 0.5409836
#> VVD          0.4590164    0.5901639 0.7377049 0.4590164 0.7868852
#> D66          0.6885246    0.7868852 0.4754098 0.7213115 0.5573770
#> CDA          0.5901639    0.8852459 0.4426230 0.5901639 0.5245902
#> GL           0.8360656    0.7049180 0.3934426 0.8688525 0.5409836
#> SP           1.0000000    0.6065574 0.3934426 0.8032787 0.5409836
#> ChristenUnie 0.6065574    1.0000000 0.3934426 0.6721311 0.4754098
#> PVV          0.3934426    0.3934426 1.0000000 0.3606557 0.7540984
#> PvdD         0.8032787    0.6721311 0.3606557 1.0000000 0.4754098
#> Verdonk      0.5409836    0.4754098 0.7540984 0.4754098 1.0000000
```
