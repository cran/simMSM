\name{hr}
\alias{hr}
\title{Proportional Hazard Rate Function.}
\description{Calculaes the proportional hazard rate for given baseline
  hazard function, time and linear predictor.}
\usage{
hr(bhr, t, eta.ij)
}
\arguments{
  \item{bhr}{transition specific baseline hazard function}
  \item{t}{time}
  \item{eta.ij}{linear predictor}}
\details{Proportional Hazards Parameterization as proposed by Cox (1972).}
\value{Value of the transition specific hazard rate}
\references{Cox, D.R. (1972): Regression Models and Life-Tables. Journal
  of the Royal Statistical Society. Series B (Methodological), Vol. 34,
  No. 2. (1972), pp. 187--220.}
\author{Holger Reulen}
\examples{
hr(bhr = function(x){0.5+0.5*sin(x)}, t=2, eta.ij=1)}
