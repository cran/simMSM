\name{simMSM-package}
\alias{simMSM-package}
\docType{package}
\title{simMSM}
\description{Provides functions for simulation of event histories with
  nonlinear baseline hazard rate functions and nonlinear covariate
  effects using inversion sampling on all cause hazard rate functions.}
\details{The central function of this package is
  \code{sim.event.histories} which wraps all other functions to perform
  simulation of event histories.}
\author{Holger Reulen}
\references{Beyersmann, J., Schumacher, M., Allignol, A. (2012): Competing Risks and
  Multistate Models with R, Springer.

  Kneib, T., Hennerfeind, A. (2008): Bayesian semi parametric
  multi-state models. Statistical Modelling, 8, 2, 169--198.}
\keyword{ package }
\examples{
bhr.11 <- function(t){return(0*t)}
bhr.12 <- function(t){return(0.5)}
bhr.13 <- function(t){return(0*t)}
bhr.14 <- function(t){return(0*t)}

bhr.21 <- function(t){return(0.5*sin(t)+0.5)}
bhr.22 <- function(t){return(0*t)}
bhr.23 <- function(t){return(0.5*sin(t)+0.5)}
bhr.24 <- function(t){return(0*t)}

bhr.31 <- function(t){return(0*t)}
bhr.32 <- function(t){return(0.5*sin(t)+0.5)}
bhr.33 <- function(t){return(0*t)}
bhr.34 <- function(t){return(0.5*sin(t)+0.5)}

bhr.41 <- function(t){return(0*t)}
bhr.42 <- function(t){return(0*t)}
bhr.43 <- function(t){return(0.5)}
bhr.44 <- function(t){return(0*t)}

all.beta = list("to.1" = list("x.1" = NULL, "x.2" = NULL), 
                "to.2" = list("x.1" = NULL, "x.2" = NULL), 
                "to.3" = list("x.1" = NULL, "x.2" = NULL),
                "to.4" = list("x.1" = NULL, "x.2" = NULL))

all.to.all.bhr.all.beta <- list(from.1=list(from = 1, all.to = c(2), 
                                            all.bhr = list(bhr.11, bhr.12, 
                                                           bhr.13, bhr.14), 
                                            all.beta = all.beta), 
                                from.2=list(from = 2, all.to = c(1, 3), 
                                            all.bhr = list(bhr.21, bhr.22, 
                                                           bhr.23, bhr.24), 
                                            all.beta = all.beta), 
                                from.3=list(from = 3, all.to = c(2, 4), 
                                            all.bhr = list(bhr.31, bhr.32, 
                                                           bhr.33, bhr.34), 
                                            all.beta = all.beta), 
                                from.4=list(from = 4, all.to = c(3), 
                                            all.bhr = list(bhr.41, bhr.42, 
                                                           bhr.43, bhr.44), 
                                            all.beta = all.beta))

all.to.all.bhr.all.beta$from.1$all.beta$to.1$x.1 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.1$all.beta$to.1$x.2 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.1$all.beta$to.2$x.1 <- function(x){return(0.5*x)}
all.to.all.bhr.all.beta$from.1$all.beta$to.2$x.2 <- function(x){return(1*x)}
all.to.all.bhr.all.beta$from.1$all.beta$to.3$x.1 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.1$all.beta$to.3$x.2 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.1$all.beta$to.4$x.1 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.1$all.beta$to.4$x.2 <- function(x){return(0*x)}

all.to.all.bhr.all.beta$from.2$all.beta$to.1$x.1 <- function(x){return(-0.5*x)}
all.to.all.bhr.all.beta$from.2$all.beta$to.1$x.2 <- function(x){return(-1*x)}
all.to.all.bhr.all.beta$from.2$all.beta$to.2$x.1 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.2$all.beta$to.2$x.2 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.2$all.beta$to.3$x.1 <- function(x){return(0.5*x)}
all.to.all.bhr.all.beta$from.2$all.beta$to.3$x.2 <- function(x){return(1*x)}
all.to.all.bhr.all.beta$from.2$all.beta$to.4$x.1 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.2$all.beta$to.4$x.2 <- function(x){return(0*x)}

all.to.all.bhr.all.beta$from.3$all.beta$to.1$x.1 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.3$all.beta$to.1$x.2 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.3$all.beta$to.2$x.1 <- function(x){return(-0.5*x)}
all.to.all.bhr.all.beta$from.3$all.beta$to.2$x.2 <- function(x){return(-1*x)}
all.to.all.bhr.all.beta$from.3$all.beta$to.3$x.1 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.3$all.beta$to.3$x.2 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.3$all.beta$to.4$x.1 <- function(x){return(0.5*x)}
all.to.all.bhr.all.beta$from.3$all.beta$to.4$x.2 <- function(x){return(1*x)}

all.to.all.bhr.all.beta$from.4$all.beta$to.1$x.1 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.4$all.beta$to.1$x.2 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.4$all.beta$to.2$x.1 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.4$all.beta$to.2$x.2 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.4$all.beta$to.3$x.1 <- function(x){return(0.5*x)}
all.to.all.bhr.all.beta$from.4$all.beta$to.3$x.2 <- function(x){return(1*x)}
all.to.all.bhr.all.beta$from.4$all.beta$to.4$x.1 <- function(x){return(0*x)}
all.to.all.bhr.all.beta$from.4$all.beta$to.4$x.2 <- function(x){return(0*x)}
set.seed(13)
d <- sim.event.histories(n=100, all.to.all.bhr.all.beta, max.time=10)
head(d, n=30)
}
