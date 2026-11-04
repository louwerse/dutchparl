# Convert every data.frame inside a voteList/questionList to a tibble, keeping
# the object's class. Used to check that functions work on tibble input.
as_tibble_object <- function(x) {
  out <- lapply(x, \(el) if (is.data.frame(el)) tibble::as_tibble(el) else el)
  class(out) <- class(x)
  out
}

# The example votes in both supported layouts: REPDEM (current) and ParlGov
# (legacy). Tests that should hold for both loop over this list.
example_votelists <- list(repdem = examplevotes, parlgov = examplevotes_legacy)
example_questionlists <- list(repdem = examplequestions, parlgov = examplequestions_legacy)
