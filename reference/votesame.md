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
#>                    CDA ChristenUnie       D66 GroenLinks      PvdA      PvdD
#> CDA          1.0000000    0.8852459 0.8032787  0.6229508 0.8852459 0.5901639
#> ChristenUnie 0.8852459    1.0000000 0.7868852  0.7049180 0.8360656 0.6721311
#> D66          0.8032787    0.7868852 1.0000000  0.8196721 0.8524590 0.7213115
#> GroenLinks   0.6229508    0.7049180 0.8196721  1.0000000 0.7377049 0.8688525
#> PvdA         0.8852459    0.8360656 0.8524590  0.7377049 1.0000000 0.7049180
#> PvdD         0.5901639    0.6721311 0.7213115  0.8688525 0.7049180 1.0000000
#> PVV          0.4426230    0.3934426 0.4754098  0.3934426 0.4262295 0.3606557
#> SGP          0.7049180    0.7213115 0.5737705  0.6557377 0.6885246 0.5573770
#> SP           0.5901639    0.6065574 0.6885246  0.8360656 0.6721311 0.8032787
#> Verdonk      0.5245902    0.4754098 0.5573770  0.5409836 0.5409836 0.4754098
#> VVD          0.6393443    0.5901639 0.6065574  0.5245902 0.6229508 0.4590164
#>                    PVV       SGP        SP   Verdonk       VVD
#> CDA          0.4426230 0.7049180 0.5901639 0.5245902 0.6393443
#> ChristenUnie 0.3934426 0.7213115 0.6065574 0.4754098 0.5901639
#> D66          0.4754098 0.5737705 0.6885246 0.5573770 0.6065574
#> GroenLinks   0.3934426 0.6557377 0.8360656 0.5409836 0.5245902
#> PvdA         0.4262295 0.6885246 0.6721311 0.5409836 0.6229508
#> PvdD         0.3606557 0.5573770 0.8032787 0.4754098 0.4590164
#> PVV          1.0000000 0.5409836 0.3934426 0.7540984 0.7377049
#> SGP          0.5409836 1.0000000 0.5573770 0.5901639 0.7049180
#> SP           0.3934426 0.5573770 1.0000000 0.5409836 0.4590164
#> Verdonk      0.7540984 0.5901639 0.5409836 1.0000000 0.7868852
#> VVD          0.7377049 0.7049180 0.4590164 0.7868852 1.0000000
```
