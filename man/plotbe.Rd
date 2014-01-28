\name{plotbe}
\alias{plotbe}
\title{Breslow Estimator of the Cumulative Baseline Hazard Rate Function}
\description{Calculates the Breslow estimator of the cumulative baseline hazard rate functions.}
\usage{
plotbe(m, mpl, return.be = FALSE, ...)
}
\arguments{
  \item{m}{estimated stratified \code{coxph} model.}
	\item{mpl}{model parameter list.}
	\item{return.be}{should a list containing the Breslow estimator values be returned?}
  \item{...}{further arguments and graphical parameters passed to \code{plot}, e.g. \code{xlim} for a re-specification of the shown time axis.} 
 }
\details{The function is a specific wrapper function to the function \code{basehaz} from the R package \code{survival}.}
\value{Plot of the Breslow estimator for the transition-type specific cumulative baseline hazard rate functions.}
\author{Holger Reulen}
\references{Therneau T (2014). A Package for Survival Analysis in S. R package version 2.37-7, http://CRAN.R-project.org/package=survival.

Terry M. Therneau and Patricia M. Grambsch (2000). Modeling Survival Data: Extending the Cox Model. Springer, New York. ISBN 0-387-98784-3.}
\examples{
\dontrun{plotbe(d, mpl, return.be = FALSE, ...)}
}