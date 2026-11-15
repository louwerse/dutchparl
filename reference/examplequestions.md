# Dutch Parliamentary Behaviour Dataset (Questions)

Written questions from Members of the Dutch lower house, asked 4-5
January 2010, with cabinet, election and party information from REPDEM
(and ParlGov for 1918-1946).

## Usage

``` r
data(examplequestions)
```

## Format

A questionList object

## Details

The object has class `c("questionListRepdem", "questionList")`. Besides
`metaList`, `questionerList`, `responderList`, `categoryList` and
`questionText`, it contains the same cabinet, election and party tables
as
[`examplevotes`](https://louwerse.github.io/dutchparl/reference/examplevotes.md),
with `questionCabinet` (cabinet and caretaker status at the date of the
response) in place of `voteCabinet`.

## See also

[`examplequestions_legacy`](https://louwerse.github.io/dutchparl/reference/examplequestions_legacy.md)
for the same questions with ParlGov data.
