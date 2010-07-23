\encoding{UTF-8}
\name{simba-package}
\alias{simba-package}
\alias{simba}
\docType{package}
\title{Calculate similarity measures for binary data
}
\description{Besides a function for the calculation of similarity measures with binary data (for instance presence/absence species data) the package contains some simple wrapper functions for reshaping species lists into matrices and vice versa and some other functions for further processing of similarity data.
}
\details{
\tabular{ll}{
Package: \tab simba\cr
Type: \tab Package\cr
Version: \tab 0.3-1\cr
Date: \tab 2010-07-08\cr
License: \tab GPL version 2\cr
}
The functions in this package can be used to calculate similarities between species records (in binary format). Functions related to the correlation of similarity matrices and some other useful functions for the analysis of spatial patterns and their change in time are included as well.
}

\author{Gerald Jurasinski \email{terhorab@mac.com}, with some help by Vroni Retzer \email{vroni.retzer@uni-bayreuth.de}
}

\references{
  Legendre, P. & Legendre, L. (1998) Numerical Ecology. -- Elsevier.
  
  Wilson, M. V. & Shmida, A. (1984) Measuring beta-diversity with presence-absence data. -- Journal of Ecology 72, 1055--1064.
  
  Gower, J.C. and Legendre, P. (1986) Metric and Euclidean properties of dissimilarity coefficients. 
\emph{Journal of Classification}, \bold{3}, 5--48.

  Faith, D. P, Minchin, P. R. and Belbin, L. (1987).
  Compositional dissimilarity as a robust measure of ecological
  distance. \emph{Vegetatio} 69, 57--68.

  Krebs, C. J. (1999). \emph{Ecological Methodology.} Addison Wesley Longman.

  Legendre, P, & Legendre, L. (1998) \emph{Numerical Ecology}. 2nd English
  Edition. Elsevier.

  Mountford, M. D. (1962). An index of similarity and its application to
  classification problems. In: P.W.Murphy (ed.),
  \emph{Progress in Soil Zoology}, 43--50. Butterworths.

  Wolda, H. (1981). Similarity indices, sample size and
  diversity. \emph{Oecologia} 50, 296--302.
}

\keyword{ package }
\seealso{
	\code{\link[vegan]{vegdist}}, \code{\link[ade4]{dist.binary}}, 
	\code{\link[labdsv]{dsvdis}}, \code{\link[stats]{dist}},
	\code{\link[stats]{reshape}}, \code{\link[stats]{cor.test}}
}
\examples{
##see functions for examples
}
