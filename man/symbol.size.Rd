\encoding{UTF-8}
\name{symbol.size}
\alias{symbol.size}
\title{Little helper function to obtain bubble plots with simple plot and points methods}
\description{Can be used within the cex argument of a plotting command (\code{plot} or \code{points}) to scale the symbols with regard to the values of a numeric variable. Thus, a bubble plot is computed.}

\usage{
symbol.size(x, cex.min = 0.2, cex.max = 5)
}

\arguments{
  \item{x}{ A numeric vector giving the data for scaling the symbols in a scatter (however, you can scale everything that's got a \code{cex} argument). The vector must have the same length as \code{x} (and \code{y}) that give the plotting positions. }
  \item{cex.min}{ The minimum size (scaling) of the plotted symbols. }
  \item{cex.max}{ The maximum size (scaling) of the plotted symbols. }
}

\details{ The same, but in a mapping context can be achieved by \code{\link[sp]{bubble}}. Also \code{\link[graphics]{symbols}} provides a comparable result. However, with \code{symbol.size} a very simple interface to bubble plots that can be used within regular scatter plot methods is given. }

\value{ A vector is returned with values between \code{cex.min} and \code{cex.max} that represent the values in the input vector {x}. After all, there is just a scaling applied. The vector is meant to be used to specifiy \code{cex} whereever appropriate.}

\references{
  Tufte ER (1998) The visual display of quantitative information. Graphics Press. 
  Cheshire, Connecticut.
}

\author{ Gerald Jurasinski }

\seealso{ \code{\link[sp]{bubble}}, \code{\link[graphics]{symbols}} } 

\examples{
# load abisko data that comes with simba
data(abis)

# take the environmental data end plot species richness at the field plot positions
with(abis.env, {plot(X, Y, cex=symbol.size(n.spec))})

# make kind of a multivariate plot in 3d:
# the relation between shannon, evenness, and simpson index (bubble size)
with(abis.env, {plot(shannon, even, cex=symbol.size(simps, cex.max=8))})

}

\keyword{ aplot }
\keyword{ dplot }
\keyword{ iplot }

