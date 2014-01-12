\name{hr}
\alias{hr}
\title{Proportional Hazard Rate Function.}
\description{Calculates the proportional hazard rate for given baseline
  hazard function, time and linear predictor.}
\usage{
hr(bhr, t, eta.ij, x.i)
}
\arguments{
  \item{bhr}{transition specific baseline hazard function}
  \item{t}{time}
  \item{eta.ij}{linear predictor}
  \item{x.i}{vector containing the current covariate values.}
}
\details{Proportional Hazards Parameterization as proposed by Cox (1972).}
\value{Value of the transition specific hazard rate}
\references{Cox, D.R. (1972): 'Regression Models and Life-Tables', Journal
  of the Royal Statistical Society. Series B (Methodological), Vol. 34,
  No. 2. (1972), pp. 187--220.}
\author{Holger Reulen}
\examples{
\dontrun{hr(bhr, t, eta.ij, x.i)}
}
