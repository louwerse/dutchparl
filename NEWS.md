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
