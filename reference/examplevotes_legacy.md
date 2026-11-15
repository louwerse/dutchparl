# Dutch Parliamentary Behaviour Dataset (Votes, legacy ParlGov version)

Votes taken in the Dutch lower house in January 2010, with cabinet,
election and party information from ParlGov, as in earlier releases of
the dataset.

## Usage

``` r
data(examplevotes_legacy)
```

## Format

A voteList object

## Details

The object has class `voteList`. `cabinetInfo` records caretaker spells
as separate cabinets, flagged by `cabinetInfo$caretaker`. There are no
`cabinetPeriods` or `voteCabinet` tables.

## See also

[`examplevotes`](https://louwerse.github.io/dutchparl/reference/examplevotes.md)
for the same votes with REPDEM data.
