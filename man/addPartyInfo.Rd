% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addInfo.R
\name{addPartyInfo}
\alias{addPartyInfo}
\alias{addPartyInfo.voteList}
\alias{addPartyInfo.questionList}
\title{Add information on party characteristics to voteList or questionerList object. This data
is obtained from ParlGov (http://www.parlgov.org/).}
\usage{
addPartyInfo(x, ...)

\method{addPartyInfo}{voteList}(
  x,
  includetype = "basic",
  addto = c("voteList", "sponsorList", "votePerParty"),
  ...
)

\method{addPartyInfo}{questionList}(
  x,
  includetype = "basic",
  addto = c("questionerList", "responderList"),
  ...
)
}
\arguments{
\item{x}{A voteList object}

\item{...}{Other parameters passed on.}

\item{includetype}{A charcter value, "basic" or "all". Defaults to "basic". Basic only includes party characteristics that vary by date, while all includes all party characteristics.}

\item{addto}{Character vector including the subtables to which party information should be added. Defaults to c("voteList", "sponsorList","votePerParty") vote a voteList object or c("questionerList", "responderList") for a questionList object.}
}
\value{
A voteList or questionList object.
}
\description{
Add information on party characteristics to voteList or questionerList object. This data
is obtained from ParlGov (http://www.parlgov.org/).
}
\section{Methods (by class)}{
\itemize{
\item \code{voteList}: Party characteristics for voteList object

\item \code{questionList}: Party characteristics for questionList object. Information for questioners refers to the day the question was asked, for the responders it pertains to the day the response was given.
}}

\examples{
examplevotes_with_partyinfo <- addPartyInfo(examplevotes)
}
