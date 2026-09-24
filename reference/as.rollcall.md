# Transform into rollcall object

Transform into rollcall object

## Usage

``` r
as.rollcall(x, ...)

# S3 method for class 'voteList'
as.rollcall(x, yea = 1, nay = 0, missing = c(3, 7, 8), notInLegis = 9, ...)
```

## Arguments

- x:

  Object to be transformed

- ...:

  Other parameters (ignored)

- yea:

  numeric, possibly a vector, code(s) for a Yea vote. Default is 1.

- nay:

  numeric, possibly a vector, code(s) for a Nay vote. Default is 0.

- missing:

  numeric or NA, possibly a vector, code(s) for missing data. Default is
  NA.

- notInLegis:

  numeric or NA, possibly a vector, code(s) for the legislator not being
  in the legislature when a particular roll call was recorded (e.g.,
  deceased, retired, yet to be elected).

## Value

Rollcall object from package 'pscl'

## Methods (by class)

- `as.rollcall(voteList)`: Transform voteList object into rollcall
  object

## Examples

``` r
as.rollcall(examplevotes)
#> Description:  Parliamentary Voting Data 
#> Source:       OB 
#> Number of Legislators:    11 
#> Number of Votes:  61 
#> 
#> Using the following codes to represent roll call votes:
#> Yea:      1 
#> Nay:      0 
#> Abstentions:  3 7 8 
#> Not In Legislature:   9 
#> 
#> Vote-specific variables:
#>  [1] "id"           "file"         "document"     "title"        "date"        
#>  [6] "proposaltype" "votetype"     "annotation"   "result"       "voteURL"     
#> [11] "proposalURL"  "source"      
#> Detailed information is available via the summary function.
```
