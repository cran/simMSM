\name{simsinglehistory}
\alias{simsinglehistory}
\title{Simulation of Single Event Histories.}
\description{Simulates event history for one individual i.}
\usage{
simsinglehistory(first.entry = 0, first.from, max.time, change.times, mpl, 
x.i, diagnostics)
}
\arguments{
  \item{first.entry}{first entry time}
  \item{first.from}{first initial state}
  \item{max.time}{maximum entry time}
  \item{change.times}{vector giving the times of change of the
    time-change covariates}
  \item{mpl}{model parameter list.}
  \item{x.i}{vector of covariate values if indivdual \code{i}.}
  \item{diagnostics}{logical ...}
}
\value{A matrix that contains the individual event history.}
\author{Holger Reulen}
\examples{
\dontrun{simsinglehistory(simsinglehistory(first.entry = 0, first.from, 
max.time, change.times, mpl, x.i, diagnostics)}
}
