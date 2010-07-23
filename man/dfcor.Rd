\encoding{UTF-8}
\name{dfcor}
\alias{dfcor}
\title{ Calculate permuted (Mantel) correlations between one and many variables }
\description{
  The function uses \code{\link{permcor2}} to calculate permuted correlation on vectors. One vector is compared to various vectors of the same length. Useful e.g. if one variable has to be tested against various variables.
}

\usage{
dfcor(ox, y, method = "pearson", permutations = 1000, ...)
}

\arguments{
  \item{ox}{ Numeric vector. If it is a similarity matrix (i.e. a \code{dist} object, extract vector via \code{as.vector(x)} beforehand). }
  \item{y}{ A \code{data.frame} containing numeric vectors to correlate \code{ox} with. Number of rows has to equal the length of \code{x} }
  \item{method}{ Method for correlation. Defaults to "pearson". See \code{\link[stats]{cor}} for other possibilities.}
  \item{permutations}{ Number of permutations. Defaults to 1000, which gives reasonable results and allows to test against alpha = 0.001. }
  \item{\dots}{ Further arguments passed to internal functions (i.e. to \code{\link[stats]{cor}}). }
}

\details{
  \code{dfcor} is a wrapper for permcor2, which is usually called as a part of \code{\link[simba]{pcol}}. Here, the numeric vector in \code{x} is compared to each column vector of \code{y}.
}

\value{
  A list with the following:
 
  \item{call }{ The function call }
  \item{method }{ P-value obtained by testing the initial correlation against the permuted correlation values. }
  \item{out }{ A table with statistics. See details below. }
  \item{gesN }{ The included number of cases. }
  \item{strata }{ The number of variables against which \code{x} was tested. }
  \item{permutations }{ The number of permutations. }
  \item{}{}
  \item{}{The included printing method gives nice output (where information appears in a slightly different order) ending with the table of \code{out}. It is based on a data.frame with nrow = ncol(\code{y}) giving the statistics for the correlation between \code{x} and each column of \code{y} as follows.}  
  \item{corr }{ Correlation value (regarding to \code{method}). }
  \item{sig }{ P-value obtained by testing the initial correlation against the permuted correlation values. }
  \item{nop }{ Number of included pairs. The function tests for complete cases before calculation starts. Pairs containing NA's are not included. }
  \item{miss }{ Number of missing pairs. }
}

\references{ 
    Legendre, P, & Legendre, L. (1998) \emph{Numerical Ecology}. 2nd 
    English Edition. Elsevier. 
}

\author{ Gerald Jurasinski }

\note{ Maybe \code{\link{pcol}} will get this functionality in future releases.
}

\seealso{ For related functions of \code{simba} \code{\link{permcor}}, \code{\link{permcor2}}, \code{\link{mancor}}, \code{\link{pcol}}. Further see \code{\link[vegan]{mantel}} of package \code{vegan} for a different implementation of permuted correlation on distance matrices.
}

\examples{
## load included data
data(abis)

## obtain several similarity matrices and correlate them
## with one other matrix.
# calculate soerensen similarity for species composition data
abis.soer <- sim(abis.spec, listout = TRUE)[,3]
# calculate euclidean distances between the plot locations
abis.geodist <- dist(abis.env[,1:2])
# calculate the ecological distance according to slope and aspect
abis.aslopect <- with(abis.env, aslopect(aspect, slope))
# calculate the ecological distance according to disturbance
abis.pert <- dist(abis.env[,19-25])

# put distances according to environment together
abis.envdist <- data.frame(dist = c(abis.geodist), aslopect = c(abis.aslopect), pert = c(abis.pert))

# no mantel correlation between those constructed
# variables and the similarity in species composition
dfcor(abis.soer, abis.envdist)

}

\keyword{ methods }
\keyword{ multivariate }
