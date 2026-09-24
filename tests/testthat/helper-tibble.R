# Convert every data.frame inside a voteList/questionList to a tibble, keeping
# the object's class. Used to check that functions work on tibble input.
as_tibble_object <- function(x) {
  out <- lapply(x, \(el) if (is.data.frame(el)) tibble::as_tibble(el) else el)
  class(out) <- class(x)
  out
}
