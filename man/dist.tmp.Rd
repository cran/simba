\encoding{UTF-8}
\name{dist.tmp}
\alias{dist.tmp}
\title{ Calculate the distance between the instances or variables of two similar data-sets. }
\description{
  The function calculates the distance between the instances or variables of two data-sets, preferably two recordings from the same places/variables at different time steps. Could for example be useful to calculate the dissimilarity of species records from the same set of plots at different recording campaigns.
}
\usage{
dist.tmp(x, y, method = "euclidean", margin = 1, adjust=TRUE)
}

\arguments{
  \item{x}{ \code{data.frame} with numeric columns giving the data at time one }
  \item{y}{ \code{data.frame} with numeric columns giving the data at time two }
  \item{method}{ Which distance measure should be employed? The following measurees are available: \code{"manhattan"}, \code{"euclidean"}, \code{"bray"}, \code{"canberra"}, \code{"kulczynski"}, \code{"gower"}, \code{"jaccard"}. See details.}
  \item{margin}{ Shall the distance between the instances (rows, \code{margin} = 2), or the distance between the variables (columns, \code{margin} = 1) be calculated? }
  \item{adjust}{ The default \code{adjust} = TRUE takes care of your input data. However, instances and variable names have to be unique, as the matching relies on these names. If your data meets this requirement the function automatically detects which instances and variables can be found in both data-sets and calculates the distance based on these data. }
}

\details{
  The function provides just an alternative to code your repeated measurements from the same plots, calculate the distances between all instcances and sort out the distances between the same instances at different time steps. Here just the latter is calculated directly.
  
  For details regarding the different distance coefficients, see \code{\link[vegan]{vegdist}}. The formulae behind were taken from there. However, not all choices have been implemented here.
}

\value{
  Returns a vector of distances. Length depends on \code{margin}. If you choose \code{margin} = 1 it equals the number of instances (plots) in your two data-sets. If you choose \code{margin} = 2 it equals the number of variables (species) in your data-sets. 
}

\references{ See references in \code{\link[vegan]{vegdist}} }

\author{ Gerald Jurasinski \email{terhorab@mac.com} }

\seealso{ \code{\link[vegan]{vegdist}}, \code{\link[stats]{dist}}, \code{\link[simba]{sim}} }

\examples{
data(abis)

##just to see that the function takes care of matching instances 
##and variables we compare abis.spec with a permuted version of
##itself and skip some instances and variables in the original matrix.
dist.tmp(abis.spec[5:50,3:120], apply(abis.spec, 2, sample), method="bray")

}

\keyword{ methods }
\keyword{ multivariate }
