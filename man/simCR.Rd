\name{simCR}
\alias{simCR}
\title{Simulation of Competing Risk Data}
\description{Simulates Competing Risk Data}
\usage{
simCR(n, bhr, beta.x.12, beta.x.13, beta.x.14, diagnostics)
}
\arguments{
  \item{n}{number of event histories to generate}
  \item{bhr}{baseline hazard rate factor (time-constant function)}
  \item{beta.x.12}{covariate effect of \code{x} on transition from 1 to 2}
  \item{beta.x.13}{covariate effect of \code{x} on transition from 1 to 3}
  \item{beta.x.14}{covariate effect of \code{x} on transition from 1 to 4}
  \item{diagnostics}{logical triggering whether diagnostics about errors
    (most likeliy because uniroot does not find a root) should be
    printed during the simulation process?}
}
\details{The underyling multi-state model is a 4 state-type model with one initial state-type and three different exit state-types.}
\value{A data frame}
\author{Holger Reulen}
\examples{
\dontrun{simCR(n, bhr, beta.x.12, beta.x.13, beta.x.14, diagnostics)}
}