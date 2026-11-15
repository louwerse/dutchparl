# Add information on party characteristics to voteList or questionList object.

Party data are obtained from REPDEM (with ParlGov for the period before
1946) or, in older versions of the data, from ParlGov
(http://www.parlgov.org/). The source is recorded in `x$datasetInfo`.

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

  A character value, "basic" or "all". Defaults to "basic". Basic only
  includes party characteristics that vary by date, while all includes
  all party characteristics (from `partyInfo`, and for REPDEM data also
  the cabinet-specific `family_name`, `ches_lrgen`, `ches_lrecon` and
  `rile`).

- addto:

  Character vector including the subtables to which party information
  should be added. Defaults to c("voteList",
  "sponsorList","votePerParty") vote a voteList object or
  c("questionerList", "responderList") for a questionList object.

## Value

A voteList or questionList object.

## Details

The basic information consists of `cabinet_party`, `prime_minister`,
`vote_share`, `seat_share` and `party_seats` (REPDEM data also include
`support_party`). Parties that hold seats but are not listed for the
cabinet in office get `cabinet_party = 0` and `prime_minister = 0`.

The cabinet is the same as in
[`addCabinetInfo`](https://louwerse.github.io/dutchparl/reference/addCabinetInfo.md):
for votes and responders, the assignment in `voteCabinet` or
`questionCabinet` (REPDEM data) is used, with the vote or response date
as fallback. Questioners are linked to the cabinet in office on the date
of the question. Election results refer to the parliamentary term in
force on that date.

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
