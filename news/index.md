# Changelog

## dutchparl (development version)

- Cabinet, election and party information can now come from REPDEM (new
  data releases, class `voteListRepdem`) or ParlGov (older releases).
  All functions work with both layouts.
- [`addCabinetInfo()`](https://louwerse.github.io/dutchparl/reference/addCabinetInfo.md)
  uses the new `cabinetPeriods` and `voteCabinet` tables for REPDEM
  data. `caretaker` is then 1 for every caretaker (demissionair) period:
  after resignation, after an election, and for interim cabinets. REPDEM
  data also get `cabinet_id`, `cabinet_period_id`, `caretaker_reason`
  and `caretaker_parlgov`. For ParlGov data, `caretaker` is taken from
  `cabinetInfo` as before.
- [`addPartyInfo()`](https://louwerse.github.io/dutchparl/reference/addPartyInfo.md)
  matches cabinet parties on `cabinet_id` when available, ignores
  duplicate rows in `partyElectionInfo`, `partyCabinetInfo` and
  `partyInfo`, and sets `cabinet_party` and `prime_minister` to 0 for
  parties that hold seats but are not listed for the cabinet in office.
  REPDEM data also get `support_party`; `includetype = "all"` adds
  `family_name`, `ches_lrgen`, `ches_lrecon` and `rile`. Cabinet
  participation follows the same cabinet as
  [`addCabinetInfo()`](https://louwerse.github.io/dutchparl/reference/addCabinetInfo.md):
  `voteCabinet`/`questionCabinet` for votes and responders (date as
  fallback), the question date for questioners.
- [`addCabinetInfo()`](https://louwerse.github.io/dutchparl/reference/addCabinetInfo.md)
  for questionList objects no longer fails when `cabinetInfo` has no
  `formal_resignation` column.
- [`filter()`](https://rdrr.io/r/stats/filter.html) and
  [`subset()`](https://rdrr.io/r/base/subset.html) keep the
  `voteCabinet`, `questionCabinet` and `questionText` tables in sync.
  [`filter()`](https://rdrr.io/r/stats/filter.html) on a voteList can
  filter on `.table = "voteCabinet"`.
- [`addCabinetInfo()`](https://louwerse.github.io/dutchparl/reference/addCabinetInfo.md)
  for questionList objects uses the `questionCabinet` table of REPDEM
  data (cabinet at the date of the response).
- `examplevotes` and `examplequestions` now use the REPDEM layout. The
  same votes and questions with ParlGov data are available as
  `examplevotes_legacy` and `examplequestions_legacy`. Text in the
  example data is ASCII only.

## dutchparl 0.0.0.9009

- [`addPartyInfo()`](https://louwerse.github.io/dutchparl/reference/addPartyInfo.md)
  and [`rice()`](https://louwerse.github.io/dutchparl/reference/rice.md)
  no longer warn about unknown columns when the data are stored as
  tibbles.
- Fixed `na.rm = TRUE` being passed to
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html) instead of
  [`max()`](https://rdrr.io/r/base/Extremes.html) in
  [`addCabinetInfo()`](https://louwerse.github.io/dutchparl/reference/addCabinetInfo.md)
  and
  [`addPartyInfo()`](https://louwerse.github.io/dutchparl/reference/addPartyInfo.md).
- [`cosponsors()`](https://louwerse.github.io/dutchparl/reference/cosponsors.md)
  output is sorted with dplyr’s default C locale (dplyr \>= 1.1.0), so
  upper-case names sort before lower-case names.
- Requires tidyr \>= 1.1.0 (single-value `values_fill` in
  `pivot_wider()`).
- `inst/CITATION` now uses
  [`bibentry()`](https://rdrr.io/r/utils/bibentry.html) instead of the
  deprecated [`citEntry()`](https://rdrr.io/r/utils/citEntry.html).
- Expanded the test suite and updated the GitHub Actions check workflow.

## dutchparl 0.0.0.9007

- Added a `NEWS.md` file to track changes to the package.
