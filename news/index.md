# Changelog

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
