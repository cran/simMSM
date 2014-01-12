\name{simeventhistories}
\alias{simeventhistories}
\title{Simulate Event Histories}
\description{Simulates n individual event histories.}
\usage{
simeventhistories(n, mpl, max.time, change.times, X, diagnostics =
FALSE, states.at.origin = NULL)
}
\arguments{
  \item{n}{number of individuals}
  \item{mpl}{model parameter list}
  \item{max.time}{maximum entry time}
  \item{change.times}{vector giving the times of change of the time-change covariates}
  \item{X}{design matrix}
  \item{diagnostics}{logical triggering whether diagnostics about errors
    (most likeliy because uniroot does not find a root) should be
    printed during the simulation process?}
  \item{states.at.origin}{state-types at origin}
}
\details{See examples below for an intuitive description of how to use the different input arguments. }
\value{Data set containing variables:
\item{id}{id (1, ..., n) of the individual}
\item{entry}{entry times}
\item{exit}{exit times}
\item{from}{values of initial states}
\item{to}{values of final states}
\item{delta}{non-censoring indicator function}
\item{x1}{values of first covariate. possible additional covariates
  follow in adjacent columns}
}
\author{Holger Reulen}
\examples{
bhr.11 <- bhr.22 <- function(t){return(0)}
bhr.12 <- bhr.21 <- function(t){return(0.5)}
eta.11 <- eta.22 <-  function(x.i, t){return(0)}
eta.11 <- function(x.i, t){eta <- 0 * sum(x.i); return(eta)}
eta.22 <- function(x.i, t){eta <- 0 * sum(x.i); return(eta)}
eta.12 <- function(x.i, t){ ## time-varying x2 and time-varying f(x2)
  ifelse(t < 5, 
         eta <- 1.0 * x.i[1] + 0.5 * x.i[2], 
         eta <- 1.0 * x.i[1] + 1.0 * x.i[3])
  return(eta)}
eta.21 <- function(x.i, t){ ## time-varying x2 and time-varying f(x1)
  ifelse(t < 5, 
         eta <- -0.5 * x.i[1] + 0.5 * x.i[2], 
         eta <-  1.0 * x.i[1] + 0.5 * x.i[3])
  return(eta)}
mpl <- list(from.1 = list(from = 1,
                          all.to = 2,
                          all.bhr = list(bhr.11, bhr.12),
                          eta = list(to.1 = eta.11, to.2 = eta.12)),
            from.2 = list(from = 2,
                          all.to = 1,
                          all.bhr = list(bhr.21, bhr.22),
                          eta = list(to.1 = eta.21, to.2 = eta.22)))
set.seed(123)
N <- 10
X <- matrix(nrow = N, ncol = 2, rnorm(2 * N) - 0.5)
X <- cbind(X, X[, 2] + runif(N)/10)
colnames(X) <- c("x1", "x2.t1", "x2.t2")
d <- simeventhistories(n = N, mpl = mpl, max.time = 10, 
                       change.times = c(5), X = X)                          
}
