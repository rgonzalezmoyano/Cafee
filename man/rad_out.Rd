% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_scores.R, R/efficiency_scores.R
\name{rad_out}
\alias{rad_out}
\title{The output-oriented radial model in the envelopment format}
\usage{
rad_out(tech_xmat, tech_ymat, eval_xmat, eval_ymat, convexity, returns)

rad_out(tech_xmat, tech_ymat, eval_xmat, eval_ymat, convexity, returns)
}
\arguments{
\item{tech_xmat}{A \code{data.frame} or \code{matrix} containing the observed inputs to determine the technology.}

\item{tech_ymat}{A \code{data.frame} or \code{matrix} containing the observed outputs to determine the technology.}

\item{eval_xmat}{A \code{data.frame} or \code{matrix} containing the containing the input data of the DMUs to be evaluated.}

\item{eval_ymat}{A \code{data.frame} or \code{matrix} containing the containing the output data of the DMUs to be evaluated.}

\item{convexity}{A \code{logical} value indicating if a convex technology is assumed.}

\item{returns}{Type of returns to scale.}
}
\value{
A \code{vector} of \code{"numeric"} scores computed through the output-oriented radial model in the envelopment format.

A \code{vector} of \code{"numeric"} scores computed through the output-oriented radial model in the envelopment format.
}
\description{
This function computes the efficiency scores through the output-oriented radial model in the envelopment format.

This function computes the efficiency scores through the output-oriented radial model in the envelopment format.
}
