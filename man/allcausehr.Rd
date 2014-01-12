\name{allcausehr}
\alias{allcausehr}
\title{All Cause Hazard Rate Function}
\description{This function calculates the all cause hazard rate function
  for given hazard rate functions.}
\usage{
allcausehr(t, all.bhr, eta.ij, x.i)
}
\arguments{
  \item{t}{scalar value giving the time the value of the all cause
    hazard has to be evaluated at.}
  \item{all.bhr}{list object containing all baseline hazard functions.}
  \item{eta.ij}{list containing transition type specific functions for
    the linear predictor.}
  \item{x.i}{vector containing the current covariate values.}
}
\value{Scalar value measuring the all cause hazard for a given initial
  state at time \code{t}}
\author{Holger Reulen}
\examples{
\dontrun{allcausehr(t, all.bhr, eta.ij, x.)}
}
