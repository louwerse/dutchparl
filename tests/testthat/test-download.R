test_that("downloadVotes calls download.file with correct default url", {
  mock_download <- function(url, destfile, mode) {
    expect_match(url, "dataverse.harvard.edu")
    expect_equal(mode, "wb")
  }
  local_mocked_bindings(download.file = mock_download, .package = "utils")
  expect_output(downloadVotes(destfile = tempfile()), regexp = "Creative Commons")
})

test_that("downloadVotes uses custom url when provided", {
  custom_url <- "https://example.com/votes.rds"
  mock_download <- function(url, destfile, mode) {
    expect_equal(url, custom_url)
  }
  local_mocked_bindings(download.file = mock_download, .package = "utils")
  capture.output(downloadVotes(url = custom_url, destfile = tempfile()))
  succeed()
})

test_that("downloadQuestions calls download.file with correct default url", {
  mock_download <- function(url, destfile, mode) {
    expect_match(url, "dataverse.harvard.edu")
    expect_equal(mode, "wb")
  }
  local_mocked_bindings(download.file = mock_download, .package = "utils")
  expect_output(downloadQuestions(destfile = tempfile()), regexp = "Creative Commons")
})

test_that("downloadQuestions uses custom url when provided", {
  custom_url <- "https://example.com/questions.rds"
  mock_download <- function(url, destfile, mode) {
    expect_equal(url, custom_url)
  }
  local_mocked_bindings(download.file = mock_download, .package = "utils")
  capture.output(downloadQuestions(url = custom_url, destfile = tempfile()))
  succeed()
})
