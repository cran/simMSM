\name{plotcph}
\alias{plotcph}
\title{Cox PH Model Effect Estimates Illustration}
\description{Plot effects of a Cox proportional hazards model.}
\usage{
plotcph(m, ...)
}
\arguments{
  \item{m}{estimated stratified \code{coxph} model.}
  \item{...}{further arguments and graphical parameters passed to \code{plot}, as for example \code{ylim} for a re-specification of the shown covariate effect axis.}
 }
\details{The Cox proportional hazards model coefficients are illustrated by the solid black lines representing the estimated effect values (y axis) for the respective covariates (x axis), the grey
polygons denote 95\% confidence intervals.}
\value{A plot.}
\author{Holger Reulen}
\references{Therneau T (2014). A Package for Survival Analysis in S. R package version 2.37-7, http://CRAN.R-project.org/package=survival.

Terry M. Therneau and Patricia M. Grambsch (2000). Modeling Survival Data: Extending the Cox Model. Springer, New York. ISBN 0-387-98784-3.}
\examples{
\dontrun{plotcph(m, ...)}
}