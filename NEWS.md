# dutchparl (development version)

* Cabinet, election and party information can now come from REPDEM (new data
  releases, class `voteListRepdem`) or ParlGov (older releases). All functions
  work with both layouts.
* `addCabinetInfo()` uses the new `cabinetPeriods` and `voteCabinet` tables for
  REPDEM data. `caretaker` is then 1 for every caretaker (demissionair) period:
  after resignation, after an election, and for interim cabinets. REPDEM data
  also get `cabinet_id`, `cabinet_period_id`, `caretaker_reason` and
  `caretaker_parlgov`. For ParlGov data, `caretaker` is taken from
  `cabinetInfo` as before.
* `addPartyInfo()` matches cabinet parties on `cabinet_id` when available,
  ignores duplicate rows in `partyElectionInfo`, `partyCabinetInfo` and
  `partyInfo`, and sets `cabinet_party` and `prime_minister` to 0 for parties
  that hold seats but are not listed for the cabinet in office. REPDEM data
  also get `support_party`; `includetype = "all"` adds `family_name`,
  `ches_lrgen`, `ches_lrecon` and `rile`.
* `addCabinetInfo()` for questionList objects no longer fails when
  `cabinetInfo` has no `formal_resignation` column.
* `filter()` and `subset()` keep the `voteCabinet` table in sync, and `filter()`
  can filter on it with `.table = "voteCabinet"`.
* `examplevotes` now uses the REPDEM layout. The same votes with ParlGov data are
  available as `examplevotes_legacy`.

# dutchparl 0.0.0.9009

* `addPartyInfo()` and `rice()` no longer warn about unknown columns when the
  data are stored as tibbles.
* Fixed `na.rm = TRUE` being passed to `as.Date()` instead of `max()` in
  `addCabinetInfo()` and `addPartyInfo()`.
* `cosponsors()` output is sorted with dplyr's default C locale (dplyr >= 1.1.0),
  so upper-case names sort before lower-case names.
* Requires tidyr >= 1.1.0 (single-value `values_fill` in `pivot_wider()`).
* `inst/CITATION` now uses `bibentry()` instead of the deprecated `citEntry()`.
* Expanded the test suite and updated the GitHub Actions check workflow.

# dutchparl 0.0.0.9007

* Added a `NEWS.md` file to track changes to the package.
