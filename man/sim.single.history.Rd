\name{sim.single.history}
\alias{sim.single.history}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulation of Single Event Histories.}
\description{Simulates event history for one individual i.}
\usage{
sim.single.history(first.entry = 0, first.from, max.time, all.to.all.bhr.all.beta, x.i)
}
\arguments{
  \item{first.entry}{first entry time}
  \item{first.from}{first initial state}
  \item{max.time}{maximum entry time}
  \item{all.to.all.bhr.all.beta}{list of length of number of initial
    states. Contains transition specific baseline hazard rate functions,
    initial state specific possible final states and transition specific
    covariate effect functions.}
  \item{x.i}{vector of covariate values if uindicual i.}
}
\value{matrix containing individual event history}
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
sim.single.history(first.entry=0, first.from=1, max.time=5,
all.to.all.bhr.all.beta=all.to.all.bhr.all.beta, x.i=c(1, 1))
}
