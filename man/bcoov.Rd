\encoding{UTF-8}
\name{bcoov}
\alias{bcoov}
\title{ Calculate Bray-Curtis distance for only one variable }
\description{ Calculates Bray-Curtis distance for only one variable: How dissimilar are the units regarding for instance pH?
}

\usage{
bcoov(x, names, listout = FALSE)
}

\arguments{
  \item{x}{ numeric vector with quantitative data from sampling-units, standardization may be applied before }
  \item{names}{ where should the names be taken from, must be a vector of the same length as \code{x} and should give the names of the sampling-units }
  \item{listout}{ Shall the result given in list-format (\code{data.frame})? Defaults to FALSE }
}

\details{
It is just a simple way to calculate similarity based on only one variable. In the future there might be a possibility to choose from some functions. However, you could also use the difference in the data-values instead (which is the Euclidean distance in one dimensional space).
}

\value{
  Returns a \code{dist} object or a \code{data.frame} (in the case of \code{listout} = TRUE) with
  \item{NBX}{ one of the compared sampling units }
  \item{NBY}{ the other part of the pair }
  \item{x}{ The returned distance value. It is named like the tested variable }
}

\author{ Gerald Jurasinski }

\seealso{ \code{\link[vegan]{vegdist}}, \code{\link[ade4]{dist.quant}}, \code{\link[labdsv]{dsvdis}} }

\examples{
data(abis)
names(abis.env) ##take a look at the data
##calculate the similarity (Bray-Curtis) between the plots 
##regarding pH
pH.dist <- bcoov(abis.env$pH, names=rownames(abis.env))

## directly give it as a list (data.frame)
pH.dist.ls <- bcoov(abis.env$pH, names=rownames(abis.env)
, listout=TRUE)

}

\keyword{ methods }
\keyword{ univar }
