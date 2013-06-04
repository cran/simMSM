\name{allcausehr}
\alias{allcausehr}
\title{All Cause Hazard Rate}
\description{This function calculates the all cause hazard rate function
  for given hazard rate functions.}
\usage{
allcausehr(t, all.bhr, eta.ij)
}
\arguments{
  \item{t}{scalar value giving the time the value of the all cause
    hazard has to be evaluated at.}
  \item{all.bhr}{list object containing all baseline hazard functions.}
  \item{eta.ij}{vector conating transition specific values of the current linear predictor.}
}
\value{scalar value measuring the all cause hazard for a given initial
  state at time \code{t}}
\author{Holger Reulen}
\examples{
allcausehr(t=3, all.bhr = list(function(x){1+sin(x)}, function(x){0.3}), eta.ij=c(1, 1))
}
