\name{simexit}
\alias{simexit}
\title{Simulate New Exit Time.}
\description{Simulates new exit time using all cause hazard functions.}
\usage{
simexit(entry, all.bhr, eta.ij, x.i, diagnostics, max.time)
}
\arguments{
  \item{entry}{entry time}
  \item{all.bhr}{list containing all cause specific baseline hazard functions}
  \item{eta.ij}{list containing transition type specific linear predictor functions}
  \item{x.i}{vector of covariate values}
  \item{diagnostics}{logical triggering whether diagnostics about errors (most likeliy because uniroot does not find a root) should be printed during the simulation process?}
  \item{max.time}{maximum entry time}
}
\details{Functions begins with drawing a quantile from the uniform
  distribution ehich will be used in the inversion sampling algorithm
  for event times.}
\value{New exit time}
\author{Holger Reulen}
\examples{
\dontrun{simexit(entry, all.bhr, eta.ij, x.i, diagnostics, max.time)}
}
