\name{plotnae}
\alias{plotnae}
\title{Nelson-Aalen Estimators for Simulated Event History Data}
\description{Calculates the Nelson-Aalen estimators for the cumulative 
hazard rate functions for simulated event history data}
\usage{
plotnae(d, mpl, return.nae = FALSE, ...)
}
\arguments{
  \item{d}{simulated data-set list as the return object from the 
  \code{\link{simeventhistories}} function.}
  \item{mpl}{model parameter list as provided to \code{\link{simeventhistories}}.}
	\item{return.nae}{should a list containing the values of the calculated 
	Nelson-Aalen estimator be returned?}
  \item{...}{further arguments and graphical parameters passed to 
  \code{plot.mvna}, e.g. \code{xlim} for a re-specification of the shown 
  time axis, or \code{conf.int} for the plotting of pointwise 
  confidence intervals.} 
 }
\details{The function is a specific (w.r.t. to the structure of the 
result from \code{\link{simeventhistories}}) wrapper function to the 
function \code{mvna} from the same-named R package \code{mvna}. }
\value{Plot of the Nelson-Aalen estimator and the underyling \code{mvna} 
result if \code{return.nae} is set to \code{TRUE}.}
\author{Holger Reulen}
\seealso{\code{\link{mplskeleton}}, \code{\link{simeventhistories}}}
\references{Allignol, A., Beyersmann, J., Schumacher, M. (2008)
\emph{mvna: An R Package for the Nelson-Aalen Estimator in Multistate Models}, 
R News, 8 (2): 48 -- 50}
\examples{
\dontrun{plotnae(d, mpl, return.nae = FALSE, ...)}
}