\name{f.for.uniroot}
\alias{f.for.uniroot}
\title{Helpfunction used in uniroot}
\description{Used in uniroot. Uniroot searches for t where cumulative
  all cause hazard equals log(1-u).}
\usage{
f.for.uniroot(exit, u, entry, all.bhr, eta.ij = eta.ij)
}
\arguments{
  \item{exit}{exit time}
  \item{u}{realization from uniform distribution in [-1;1].}
  \item{entry}{entry time}
  \item{all.bhr}{list of all baseline hazard functions}
  \item{eta.ij}{vector of linear predictors}
}
\value{time t where cumulative all cause hazard equals log(1-u).}
\author{Holger Reulen}
\note{Only a helpfunction for uniroot.}
\examples{
\dontrun{f.for.uniroot(exit=100, u=0.5,
  all.bhr=list(function(x){1+sin(x)}, function(x){0.3}),
  eta.ij=c(1, 1))}}