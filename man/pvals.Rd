% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pvals.R
\name{pvals}
\alias{pvals}
\title{Extract p-values}
\usage{
pvals(model, col_name)
}
\arguments{
\item{model}{An `evaluCATE` object/model considered}

\item{col_name}{A string object of how the column of p-values should be called}
}
\value{
A vector of p-values
}
\description{
Extract p-values from the GATE results of the `evaluCATE` package.

The p-values extracted are specifically of the hypothesis test \eqn{H_0: \ \gamma_k = \gamma_1}
}
\examples{
pvals(evaluation_tl, "tl_pval")
}
