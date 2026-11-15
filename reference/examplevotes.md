# Dutch Parliamentary Behaviour Dataset (Votes)

Votes taken in the Dutch lower house in January 2010, with cabinet,
election and party information from REPDEM (and ParlGov for 1918-1946).

## Usage

``` r
data(examplevotes)
```

## Format

A voteList object

## Details

The object has class `c("voteListRepdem", "voteList")` and contains the
sub-tables `datasetInfo`, `metaList`, `voteList`, `voteMatrix`,
`votePerParty`, `sponsorList`, `categoryList`, `cabinetInfo`,
`cabinetPeriods`, `voteCabinet`, `electionInfo`, `partyElectionInfo`,
`partyCabinetInfo` and `partyInfo`.

`cabinetInfo` has one row per cabinet. Caretaker (demissionair) status
is recorded per cabinet period in `cabinetPeriods` and per vote in
`voteCabinet`; `cabinetInfo$caretaker` only marks interim cabinets. See
[`addCabinetInfo`](https://louwerse.github.io/dutchparl/reference/addCabinetInfo.md).

## See also

[`examplevotes_legacy`](https://louwerse.github.io/dutchparl/reference/examplevotes_legacy.md)
for the same votes with ParlGov data.
