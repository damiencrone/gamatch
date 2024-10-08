% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_f.R
\name{compute_f}
\alias{compute_f}
\title{Compute F-statistic for ANOVA}
\usage{
compute_f(x, group)
}
\arguments{
\item{x}{A numeric vector of data values.}

\item{group}{A factor vector specifying the groups to which the data values belong.}
}
\value{
The F-statistic for the one-way ANOVA.
}
\description{
This function computes the F-statistic for a one-way ANOVA.
}
\examples{
set.seed(123)
x <- rnorm(100)
group <- rep(letters[1:4], 25)
compute_f(x, group)

}
