# Add information on party characteristics to voteList or questionerList object. This data is obtained from ParlGov (http://www.parlgov.org/).

Add information on party characteristics to voteList or questionerList
object. This data is obtained from ParlGov (http://www.parlgov.org/).

## Usage

``` r
addPartyInfo(x, ...)

# S3 method for class 'voteList'
addPartyInfo(
  x,
  includetype = "basic",
  addto = c("voteList", "sponsorList", "votePerParty"),
  ...
)

# S3 method for class 'questionList'
addPartyInfo(
  x,
  includetype = "basic",
  addto = c("questionerList", "responderList"),
  ...
)
```

## Arguments

- x:

  A voteList object

- ...:

  Other parameters passed on.

- includetype:

  A charcter value, "basic" or "all". Defaults to "basic". Basic only
  includes party characteristics that vary by date, while all includes
  all party characteristics.

- addto:

  Character vector including the subtables to which party information
  should be added. Defaults to c("voteList",
  "sponsorList","votePerParty") vote a voteList object or
  c("questionerList", "responderList") for a questionList object.

## Value

A voteList or questionList object.

## Methods (by class)

- `addPartyInfo(voteList)`: Party characteristics for voteList object

- `addPartyInfo(questionList)`: Party characteristics for questionList
  object. Information for questioners refers to the day the question was
  asked, for the responders it pertains to the day the response was
  given.

## Examples

``` r
examplevotes_with_partyinfo <- addPartyInfo(examplevotes)
```
