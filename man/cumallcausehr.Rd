\name{cumallcausehr}
\alias{cumallcausehr}
\title{Cumulative All Cause Hazard Rate}
\description{Calulates the value of the integral of the all cause hazard
  rate.}
\usage{
cumallcausehr(entry, exit, all.bhr, eta.ij = eta.ij)
}
\arguments{
  \item{entry}{entry time}
  \item{exit}{exit time}
  \item{all.bhr}{list of baseline hazard functions}
  \item{eta.ij}{vector of transition specific linear predictor values}
}
\value{value of the integral of the all cause hazard function from \code{entry}
to \code{exit}}
\author{Holger Reulen}
\examples{
cumallcausehr(entry=0, exit=3,
  all.bhr=list(function(x){1+sin(x)}, function(x){0.3}),
  eta.ij=c(1, 1))
}
