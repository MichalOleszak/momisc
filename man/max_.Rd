% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na_robust_aggregators.R
\name{max_}
\alias{max_}
\title{Maximal value ignoring NAa}
\usage{
max_(x)
}
\arguments{
\item{x}{numeric vector}
}
\value{
A single numeric.
}
\description{
Maximal value ignoring NAa
}
\examples{
a <- c(1, 2, 3, NA, 5)
max_(a)
}
