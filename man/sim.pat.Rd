\encoding{UTF-8}
\name{sim.pat}
\alias{sim.pat}
\alias{sima}
\alias{simn}
\title{ Calculate pattern diversity }
\description{
  The function \code{sim.pat} uses \code{sima} to calculate compositional similarity between a focal sampling unit and all units in the data-set and \code{simn} to calculate compositional similarity between a focal unit and its surrounding units. Surrounding can be specified as a radius or a ring. Significance can be tested.
}

\usage{
sim.pat(veg, coord = NULL, dn, presabs = TRUE, test = TRUE, 
permutations = 100, ...)

sima(veg, presabs = TRUE, d.inc = FALSE, ...)

simn(veg, coord, dn, presabs = TRUE, d.inc = FALSE, ...)
}

\arguments{
  \item{veg}{ A (species) matrix with rows representing plots/sampling units and columns representing species/variables. Up to now all values are transformed to presence/absence: all values > 0 are transformed to 1, all other values are 0. }
  \item{coord}{ A two column \code{data.frame} defining the geographic positions of the sampling units. Should give (in this order) the x- and y-values in UTM or similar coordinates. Defaults to NULL. Triggers also if similarity to neighbours (when it is given) or to all units in the data-set (default) should be calculated. }
  \item{dn}{ Neighbour definition. A positive numeric or a two value vector. In the first case it gives the distance from each sampling unit until which other sampling units should be seen as neighbours. In the second the two values define a ring around each plot. Plots which fall into the ring are considered as neighbors. See details. If \code{coord} is not given it becomes obsolete. }
  \item{presabs}{ Triggers the calculation method. Should only the presence in the focal unit (FALSE) or also the absence in the focal unit (TRUE) be regarded? }
  \item{d.inc}{ If species not occuring on the focal plot or its surroundings - but in the rest of the data-set should be considered, set it to TRUE. However, default behaviour is to do not. Note that when calling \code{sim.pat} without coordinates or \code{sima} this setting takes not effect unless there are non-occuring species in the matrix. }
  \item{test}{ Should significance be tested? Defaults to true. See details. }
  \item{permutations}{ If \code{test=TRUE}, how many permutations of the species matrix should be carried out? Defaults to 100. See details. }
  \item{\dots}{ Further arguments to functions. }
}

\details{
The idea behind the function is the extension of available similarity measures by the possibility to calculate similarities between a focal unit and several other (surrounding) units at once whilst preserving species identities. In an equidistant grid one could also calculate the similarities between a focal unit and its six neighbours and then calculate a mean similarity value. However, this might be problematic as the values are based on the presence of different species. So the same similarity value can derive from a different set of species.

\code{sim.pat} is different as it respects species identities of all included units at once. It has two modes. If a coordinate file and a neighbour definition is given it calculates the similarity between each unit and its surrounding units. Neighbours are all units which fall into the specified radius or ring of radiuses (\code{dn}). On an equidistant grid the distance between grid units can simply be given if first hand neighbours shall be included, otherwise a ring has to be specified with a two value vector. If there is no \code{coord}, \code{dn} is obsolete. Then the similarity from each unit to all other units in the data-set is calculated. 

The significance of the obtained similarity value can be tested in both cases. In the neighbour-case it is tested against a randomly rearranged species matrix \code{veg}. For each permuted matrix, \code{simn} is calculated for each unit and stored. The initial value is then tested against the resulting random values. If the initial value is lower than the mean of the permuted values (for each unit), the function looks at the lower end. If it is higher, the upper tail is tested. The test direction is given in the results file. Thus, for each unit it is known, whether the pattern is a deviation from random, if it is lower or higher than random, and if it is signifcantly different from random. In the all-case it is virtually the same but as a rearrangement of species on plots would change nothing, the function tests against an artificially produced data-set. This is done with \code{\link{ads}} and the key parameters resemble the original species matrix \code{veg}.
}

\value{ Returns a \code{data.frame} with the similarity values calculated between the focal unit and its neighbours (\code{sim2nbs}) or all other units in the data-set (\code{sim2all}). In case of the first, the two first columns of the \code{data.frame} give the number of species on the focal plot (\code{n.spec}) and the number of neighbours (\code{nbs}) to which similarity was calculated based on the neighbour definition (\code{dn}).

If \code{test = TRUE} the following columns are given in addition:

  \item{sim.test }{ The p-value of the significance test. }
  \item{sig }{ The significance decision, tested against alpha = 0.05. \code{"*"} means similarity value is significantly different from random. }
  \item{sig.prefix }{ Marks against which tail significance was tested. If the initial similarity value is lower than the mean of the permuted values the lower tail is tested (\code{sig.prefix = "-"}) and vice versa (\code{sig.prefix = "+"}). }
}

\references{ 
    Jurasinski, G. & Retzer, V. in prep. Measuring multi-plot similarity with presence-absence data. \emph{Ecology} in prep.
     }

\author{ Gerald Jurasinski \email{terhorab@mac.com} \url{http://homepage.mac.com/terhorab/gerald/}, Vroni Retzer \email{vroni.retzer@uni-bayreuth.de} }

\seealso{ \code{\link{sim}} from this package and the following functions of other packages for the calculation of similarities between two sites: \code{\link[vegan]{vegdist}}, \code{\link[ade4]{dist.binary}}, 
	\code{\link[labdsv]{dsvdis}}, \code{\link[stats]{dist}}. }

\examples{
% Has to be included...
}

\keyword{ methods }
\keyword{ multivariate }
