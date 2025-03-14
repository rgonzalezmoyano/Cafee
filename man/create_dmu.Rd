% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balanced_data.R
\name{create_dmu}
\alias{create_dmu}
\title{Create New DMUs to reach 150 dmus}
\usage{
create_dmu(data, data_factor, x, y, z, N, type, rts)
}
\arguments{
\item{data}{A \code{data.frame} containing the variables used in the model.}

\item{x}{number of inputs in the \code{data}.}

\item{y}{number of outputs in the \code{data}.}

\item{N}{number of dmus to create \code{data}.}

\item{type}{class of dmu to create \code{data}.}

\item{rts}{selection of technology for DEA projection \code{data}.}
}
\value{
It returns a \code{data.frame} with the newly created set of DMUs incorporated.
}
\description{
This function adds DMUs to reach a sample of 150.
}
