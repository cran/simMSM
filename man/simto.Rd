\name{simto}
\alias{simto}
\title{Simulate New Final State.}
\description{Simulation of the new final state for given entry time, inital
  state, all cause baseline hazard rates, transition specific covariate
  effect functions and transition specific values of the linear predictor.}
\usage{
simto(entry.ij, from.ij, mpl, eta.ij, x.i, diagnostics, max.time)
}
\arguments{
  \item{entry.ij}{current entry time}
  \item{from.ij}{current initial state}
  \item{mpl}{model parameter list.}
  \item{eta.ij}{transition specific values of the linear predictor.}
  \item{x.i}{vector of covariate values if indivdual \code{i}.}
  \item{diagnostics}{logical triggering whether diagnostics about errors (most likeliy because uniroot does not find a root) should be printed during the simulation process?}
  \item{max.time}{maximum entry time}
}
\value{list including new final state.}
\author{Holger Reulen}
\examples{
\dontrun{simto(entry.ij, from.ij, mpl, eta.ij, x.i, diagnostics, max.time)}
}
