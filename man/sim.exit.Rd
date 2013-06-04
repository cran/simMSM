\name{sim.exit}
\alias{sim.exit}
\title{Simulate New Exit Time.} 
\description{Simulates new exit time using all cause hazard functions.}
\usage{
sim.exit(entry, all.bhr, eta.ij = eta.ij)
}
\arguments{
  \item{entry}{entry time}
  \item{all.bhr}{list containing all cause specific baseline hazard functions}
  \item{eta.ij}{vector of transition specific linear predictor values}
}
\details{Functions begins with drawing a quantile for the current transition specific
  distribution of event times.}
\value{New exit time}
\author{Holger Reulen}
\examples{
sim.exit(entry=0, all.bhr=list(function(x){1+sin(x)}, function(x){0.3}),
eta.ij=c(1, 1))
}
