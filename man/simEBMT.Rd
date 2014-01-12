\name{simEBMT}
\alias{simEBMT}
\title{Simulate Event Histories According to the Results of an EBMT Data Analysis}
\description{This functions simulates event histories, with a parameterization as given by the results presented in the article given in the References section for this help page.}
\usage{simEBMT(n)}
\arguments{
  \item{n}{integer, number of event histories to be simulated.}
}
\details{The final data set is mean centered with taking care of transition-types. This is important for boosting of partial likelihood multi-state models as implemented by the package \code{gamboostMSM}.}
\value{A data set.}
\references{Liesbeth C. de Wreede, Marta Fiocco, and Hein Putter (2011): 'mstate: An R Package for the Analysis of
Competing Risks and Multi-State Models', Journal of Statistical Software, January 2011, Volume 38, Issue 7.}
\author{Holger Reulen}
\examples{
\dontrun{simCR(n, bhr, beta.x.12, beta.x.13, beta.x.14, diagnostics)}
}