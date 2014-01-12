\name{simexitinner}
\alias{simexitinner}
\title{Help Function used to simulate New Exit Time.}
\description{Help function for \code{simexit}.}
\usage{
simexitinner(u, entry, all.bhr, eta.ij, x.i, max.time)
}
\arguments{
  \item{u}{random number as a realisation of the uniform distribution}
  \item{entry}{entry time}
  \item{all.bhr}{list containing all cause specific baseline hazard functions}
  \item{eta.ij}{list containing transition type specific linear predictor functions}
  \item{x.i}{vector of covariate values}
  \item{max.time}{maximum entry time}
}
\details{This function is used inside \code{simexit}.}
\value{A list with two elements: \code{new.exit} and \code{u}.}
\author{Holger Reulen}
\examples{
\dontrun{simexitinner(u, entry, all.bhr, eta.ij, x.i)}
}
