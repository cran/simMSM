\name{meancentering}
\alias{meancentering}
\title{Mean Centering with Taking Care of the Transition Type(s)}
\description{This function is needed inside the \code{simEBMT} function.}
\usage{meancentering(d, x, q, x.name, q.name)}
\arguments{
  \item{d}{dat set}
  \item{x}{covariate}
  \item{q}{transition type(s)}
  \item{x.name}{name of the covariate for pasting the new transition
    type specific covariate name}
  \item{q.name}{name of the transition type for pasting the new transition
    type specific covariate name}
}
\details{Mean centering with taking care of transition-types is important for boosting of partial likelihood multi-state models as implemented by the package \code{gamboostMSM}.}
\value{A data set containing simulated event histories.}
\author{Holger Reulen}