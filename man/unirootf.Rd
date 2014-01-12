\name{unirootf}
\alias{unirootf}
\title{Warapper function for \code{cumallcausehr} to be used in \code{uniroot}.}
\description{Function used for the numerical inversion of the cumulative all-cause hazard rate function.}
\usage{
unirootf(exit, u, entry, all.bhr, eta.ij, x.i)
}
\arguments{
  \item{exit}{exit time}
  \item{u}{sample from the uniform distribution}
  \item{entry}{entry time}
  \item{all.bhr}{list containg all cause-specific baseline hazard rate functions.}
  \item{eta.ij}{linear predictor functions}
  \item{x.i}{covariate observations for indididual \code{i}}
  }
\value{Return value of \code{cumallcausehr}}
\author{Holger Reulen}
\examples{
\dontrun{unirootf(exit, u, entry, all.bhr, eta.ij, x.i)}
}