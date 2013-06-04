\name{sim.event.histories}
\alias{sim.event.histories}
\title{Simulate Event Histories}
\description{Simulates n individual event histories.}
\usage{
sim.event.histories(n, all.to.all.bhr.all.beta, max.time = 10)
}
\arguments{
  \item{n}{number of individuals}
  \item{all.to.all.bhr.all.beta}{list of length of number of initial
    states. Contains transition specific baseline hazard rate functions,
    initial state specific possible final states and transition specific
    covariate effect functions.}
  \item{max.time}{max entry time}}
\details{In the current implementation, covariates are sampled from
  continuous uniform distribution on [-1;1].}
\value{Data set containing variables
\item{id }{id (1, ..., n) of the individual}
\item{entry }{entry times}
\item{exit }{exit times}
\item{from }{values of initial states}
\item{to }{values of final states}
\item{x1 }{values of first covariate. possible additional covariates
  follow in adjacent columns}
}
\author{Holger Reulen}
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
