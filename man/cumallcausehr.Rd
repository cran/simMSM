\name{cumallcausehr}
\alias{cumallcausehr}
\title{Cumulative All Cause Hazard Rate}
\description{Calculates the value of the integral of the all cause hazard
  rate.}
\usage{
cumallcausehr(entry, exit, all.bhr, eta.ij, x.i)
}
\arguments{
  \item{entry}{entry time}
  \item{exit}{exit time}
  \item{all.bhr}{list of baseline hazard functions}
  \item{eta.ij}{list of transition type specific linear predictor
    functions}
  \item{x.i}{vector containing the current covariate values.}
}
\value{Value of the integral of the all cause hazard function from \code{entry}
to \code{exit}}
\author{Holger Reulen}
\examples{
\dontrun{cumallcausehr(entry, exit, all.bhr, eta.ij, x.i)}
}
