\name{pmeskeleton}
\alias{pmeskeleton}
\title{Build Up a Partial Markov Model Linear Predictor List Skeleton}
\description{Constructs the skeleton of a linear predictor list for partial Markov influences on basis of the transition-type definition matrix.}
\usage{
pmeskeleton(tmat)
}
\arguments{
  \item{tmat}{a transition-type definition matrix. This is a square matrix containing the boolean information of which exit state-types (the columns) are reachable from which entry state-type (the lines).}
 }
\details{The example below provides an intuitive description of how to suitably set up the input arguments. }
\value{An incomplete (therefore the function name ends with 'skeleton') linear predictor list as used for the \code{partial.markov.eta} input argument in the function \code{\link{simeventhistories}}.}
\author{Holger Reulen}
\examples{
## Two state-type model with transient state-types 1 and 2: 
tra2 <- matrix(ncol = 2, nrow = 2, data = FALSE)
tra2[1, 2] <- tra2[2, 1] <- TRUE
pmeskeleton(tmat = tra2)
## Illness-death model (IDM) with recovery:
traIDM <- matrix(nrow = 3, ncol = 3, FALSE)
traIDM[1, 2] <- traIDM[1, 3] <- traIDM[2, 1] <- traIDM[2, 3] <- TRUE
pmeskeleton(tmat = traIDM)
}