% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/votesame.R
\name{votesame}
\alias{votesame}
\alias{votesame.rollcall}
\alias{votesame.voteList}
\title{Calculate the percentate of votes in which parties voted the same}
\usage{
votesame(x, ...)

\method{votesame}{rollcall}(x, order.x = NULL, ...)

\method{votesame}{voteList}(x, order.x = NULL, ...)
}
\arguments{
\item{x}{A rollcall or voteList object.}

\item{...}{Other parameters passed on.}

\item{order.x}{Character matrix of legislator names. This is used to re-order legislator (or party) columns, if desired.}
}
\value{
A matrix of voting similarities
}
\description{
Calculate the percentate of votes in which parties voted the same
}
\section{Methods (by class)}{
\itemize{
\item \code{rollcall}: Votesame for rollcall object

\item \code{voteList}: Votesame for voteList object
}}

\examples{
votesame(examplevotes)
}
