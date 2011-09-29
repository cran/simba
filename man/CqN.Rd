\encoding{UTF-8}
\name{CqN}
\alias{CqN}
\alias{CqNa}
\title{
Probabilistic multiple-plot similarity indices following Chao et al. (2008). Strictly preliminary.
}
\description{
The two functions follow different approaches to the probabilistic multiple-plot similarity indices as developed by Chao et al. (2008). Both are in alpha state.
}

\usage{
CqN(x, q = 2, method = "pi.a")

CqNa(x, q = 0, base = exp(1), ...)
}

\arguments{
  \item{x}{
Species x sites matrix.
}
  \item{q}{
Integer. Specifies the order of the diversity (see References).
}
  \item{method}{
Character string. How the proportional abundances (pi) are to be calculated. Following the gamma-, omega- or alpha-classification in the sense of Tuomisto 2010a,b. Possible are "pi.a" – pi are calculated against the sums per site, "pi.ga" - pi are calculated against the total sum of the abundances in the dataset, "pi.g" – pi are calculated as the columns sums (sums per species) of the former.
}
  \item{base}{
In \code{CqNa} there is logarithm calculation involved. Per default the general default of R for the \code{base} (exp(1)) is taken, but it can be changed if wanted via \code{base}.
}
  \item{\dots}{
Further arguments to \code{\link{trudi}}.
}

}

\details{
Both function are approaches to the family of multiple-plot similarity measures suggested by Chao et al. (2008) including measures of overlap and homogeneity. Strictly preliminary. Basically included at this stage because C27 was also calculated in Jurasinski et al. 2011 as one of many different multiple-plot similarity indices. Currently \code{CqN} is not defined at q < 2.

\code{CqNa} bases its calculations on calculations of \code{\link{trudi}} for the respective order \code{q}.
}

\value{
\code{CqN} returns the value and a correct description. For instance when using q = 3 on a species x sites matrix with 5 sites the result will be named "C35".

\code{CqNa} returns a named vector with values for the overlap measure based on CqN (naming follows the same path as described above), the homogeneity measure sensu MacArthur (1965) based on CqN, and the relative homogeneity measure based on CqN.
}

\references{
see References in \code{\link{trudi}}
}

\author{ Gerald Jurasinski \email{gerald.jurasinski@uni-rostock.de} }

\seealso{
\code{\link{trudi}}
}

\examples{
# get data
data(bernina)
# run CqNa on the first time step in the summit data
CqNa(veg[1:7,], q=1)
# vary q to see what is happening when you lay more emphasis on rare (0) 
# or common (2) species
CqNa(veg[1:7,], q=0)
CqNa(veg[1:7,], q=0)

# check the development of multiple plot similarity on the summits in time
ans <- c(rep(1907,7), rep(1985,7), rep(2003,7))
by(veg, ans, function(x) CqNa(x, q=1))

}

\keyword{ methods }
\keyword{ multivariate }
