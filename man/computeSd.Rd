% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic.R, R/error_model.R
\name{computeSd}
\alias{computeSd}
\alias{computeSd,proportional_error_model,numeric-method}
\alias{computeSd,combined_error_model,numeric-method}
\title{Compute standard deviation of error model.}
\usage{
computeSd(object, x, ...)

\S4method{computeSd}{proportional_error_model,numeric}(object, x)

\S4method{computeSd}{combined_error_model,numeric}(object, x)
}
\arguments{
\item{object}{error model}

\item{x}{predicted values, numeric vector}

\item{...}{extra arguments}
}
\value{
the standard deviation
}
\description{
Compute standard deviation of error model.
}
