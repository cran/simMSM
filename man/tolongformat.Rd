\name{tolongformat}
\alias{tolongformat}
\title{Transforms Data Frame into Long Format Design}
\description{Data frame with one line per event gets transformed to a data 
frame in a format that has as many rows as each subject has transitions for 
which he/she is at risk.}
\usage{
tolongformat(d, mpl)
}
\arguments{
  \item{d}{simulated data-set as the return object from the 
  \code{\link{simeventhistories}} function.}
	\item{mpl}{model parameter list.}
 }
\details{In the format of the input data frame object \code{d}, the data 
are not yet suitable for a stratified Cox partial likelihood analysis: 
we need the data frame in a format that has many rows as each subject 
has transitions for which he/she is at risk.
We will denote this as 'long format' in reference to the literature on 
multi-state model software, as for example on page 5 in de Wreede et al (2011).}
\value{A list of data-sets.}
\author{Holger Reulen}
\seealso{\code{\link{simeventhistories}}}
\references{Liesbeth C. de Wreede, Marta Fiocco, Hein Putter (2011)
mstate: An R Package for the Analysis of Competing Risks and Multi-State Models.
Journal of Statistical Software, 38(7), 1-30. URL http://www.jstatsoft.org/v38/i07/.}
\examples{
\dontrun{tolongformat(d, mpl)}
}