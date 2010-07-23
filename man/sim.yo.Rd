\encoding{UTF-8}
\name{sim.yo}
\alias{sim.yo}
\title{ Calculate a binary similarity index you define. }
\description{
  Enables the calculation of any binary similarity index via the provision of a formula.
}
\usage{
sim.yo(x, coord = NULL, method = "(2*a)/((2*a) + b + c)", dn = NULL, 
normalize = FALSE, listin = FALSE, listout = FALSE, ...)
}

\arguments{
  \item{x}{Vegetation data, either as matrix with rows = plots 
  and columns = species (similarities are calculated between rows!), or as \code{\link[base]{data.frame}} with 
  first three columns representing plots, species and occurence 
  information respectively.  All further columns are dumped before 
  calculation.  Occurence is only considered as binary. If your list 
  or matrix contains abundances or frequencies they are transformed 
  automatically.}
  \item{coord}{ A \code{data.frame} with two columns containing the coordinate values of the sampling units. If given, it triggers the simultaneous calculation of the geographical distances between the sampling units, the coordinates of virtual centre-points between all possible pairs of plots, and the geographical distances in either x- or y-direction. If \code{coord} is given, output is always in \code{database} format (no matrix). }
  \item{method}{ Give the formula for a binary similarity index. Defaults to the formula of Sørensen index. See \code{\link[simba]{sim}} for more examples and general explanations.
  }
  \item{dn}{ Neighbor definition. A geographic distance represented by a numeric or a two value vector defining a ring around each plot. Only takes effect when \code{coord} != NULL. If specified, the output does only contain similarities between neighboring plots. A plot is a neighbour to any given plot if it is within the range of the neighbor definition. See details.}
  \item{normalize}{ Logical value indicating whether the values for 
  \code{a}, \code{b} and \code{c} which are calculated in the process should be normalized to 100\% (per row, which means per plot comparison). If normalize = TRUE an asymmetric index must be chosen (see details in \code{\link[simba]{sim}}).}
  \item{listin}{ if \code{x} is given in database (list) format this must be set to TRUE (there is no automatic detection of the format)}
  \item{listout}{ If output is wanted in database format rather than as a \code{\link[stats]{dist}}-object set this to TRUE. Output is automatically given in database-format, when \code{coord} is specified. }
  \item{\dots}{ Arguments to other functions }
}
\details{
  Presumably this function will rarely be used because \code{\link[simba]{sim}} already allows for the calculation of a large variety of binary similarity coefficients. But just in case you found or thought of an alternative this function is provided. For details regarding similarity indices see \code{\link[simba]{sim}}. You have to give your formula in quotation marks like this: "(2*a)/((2*a) + b + c)". 
}
\value{
  If listout = FALSE a distance matrix of class \code{\link[stats]{dist}} is returned. If listout = TRUE, a \code{\link[base]{data.frame}} is returned with 7 columns giving the names of the compared plots in the first two and the calculated similarity measure in the third column. The rest of the columns give the values for \code{a, b, c,} and \code{d} (in this order). Naming of the first three columns can be changed but defaults to \code{NBX} (one of the compared plots), \code{NBY} (the other one), \code{used index} (the values of the calculated index). If \code{coord} != NULL, the following columns are given in addition and the columns \code{a:d} shift to the end of the data.frame. 
    
    \item{distance}{ Geographical distance between compared plots }
    \item{X}{ For plotting purposes, the x-coordinate of the virtual position of the calculated similarity value in the center between the two compared plots }
    \item{Y}{ For plotting purposes, the y-coordinate of the virtual position of the calculated similarity value in the center between the two compared plots }
    \item{xdist}{ Geographical distance between compared plots, on the x-axis only }
    \item{ydist}{ Geographical distance between compared plots, on the y-axis only }
}

\references{ 
    Sørensen, T. (1948) A method of establishing groups of equal amplitude in plant sociology based on similarity of species content. \emph{Biologiske Skrifter} \bold{5}: 1-34.   
}

\author{ Gerald Jurasinski \email{gerald.jurasinski@uni-rostock.de} }

\note{ In general, concepts of data-handling are taken from \code{\link[vegan]{vegdist}} and the calculation of \code{a}, \code{b}, \code{c} and \code{d} is taken from \code{\link[ade4]{dist.binary}}. Thanks to Jari Oksanen for his \code{vegan} package and the idea to provide a custom build distance formula.
}

\seealso{ 
	\code{\link[vegan]{vegdist}}, \code{\link[vegan]{designdist}}, \code{\link[ade4]{dist.binary}}, 
	\code{\link[labdsv]{dsvdis}}, \code{\link[stats]{dist}} for other dissimilarity coefficients, and \code{\link{sim}} for a variety of formulae for similarity coefficients.
}

\examples{

data(abis)

##calculate the default Sørensen index
abis.soer <- sim.yo(abis.spec)

##calculate a custom similarity index
abis.sim <- sim.yo(abis.spec, method="(2*a)/((a) + b + c)")

}

\keyword{ methods }
\keyword{ multivariate }
