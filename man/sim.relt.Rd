\encoding{UTF-8}
\name{sim.relt}
\alias{sim.relt}
\alias{sim.rel}
\title{ Calculate additive and relative partitioning.  }
\description{
  Enables the calculation of three \eqn{\beta}{beta}-diversity measures closely related to each other: additive partitioning, multiplicative partitioning (Whittaker 1960), reverse multiplicative partitioning (Whittaker 1960, full change notion).
}
\usage{
sim.relt(veg, coord = NULL, dn, method = "additive", test = TRUE, 
permutations = 100, ...)

sim.rel(veg, coord=NULL, dn, method="additive", ...)
}

\arguments{
  \item{veg}{ A species matrix. The function takes only presence/absence into account. However the translation is done automatically by assuming that zero means no occurrence, whereas any value > 0 is transformed to 1 }
  \item{coord}{ A two column \code{data.frame} defining the geographic positions of the sampling units. Should give (in this order) the x- and y-values in UTM or similar coordinates. Defaults to NULL. Triggers also if the similarity measures should only incorporate a focal plot and its neighbours  (moving window) or if all units in the data-set (default) should be considered. }
  \item{dn}{ A numeric or a two value vector. In the first case it gives the distance until which sampling units should be considered as neighbors. In the second the two values define a ring around each plot. Plots which fall into the ring are considered as neighbors. If \code{coord} is not given it becomes obsolete. }
  \item{method}{ Which similarity measure should be calculated. Give a partial match to "\code{additive}", "\code{relative}" or "\code{reverse}". See details. }
  \item{test}{ Shall significance be tested? Defaults to true. Does only make sense when \code{coord} and \code{dn} is given. In general \code{sim.relt} calls \code{sim.rel} to calculate the values. So the latter will rarely used by the user directly. }
  \item{permutations}{ The number of permutations. Defaults to 100 which allows for a p-level of 0.01 }
  \item{\dots}{ Arguments to other functions. }
}

\details{
  Three different diversity measures related to the concept of \eqn{\beta}{beta}-diversity can be calculated with this function. Besides the definition of \eqn{\beta}{beta}-diversity as the turnover in species composition (Whittaker 1960), Whittaker defined the \emph{relation} between \eqn{\gamma}{gamma}- and \eqn{\alpha}{alpha}-diversity as \eqn{\beta}{beta}-diversity where \eqn{\alpha}{alpha} is the mean species richness found on local plots or sampling units and \eqn{\gamma}{gamma}-diversity is the species richness throughout a reference area (e.g. an investigation area). However, \eqn{\gamma}{gamma} could also be defined for smaller units e.g. for a focal plot and its neighbours, which is done in the function when \code{coord} and \code{dn} are set. \eqn{\beta}{Beta}-diversity in terms of \emph{multiplicative} diversity is calculated when \code{method} is set to "\code{relative}". "\code{reverse}" simply means that the relation is calculated vice versa (\eqn{\frac{\bar{\alpha}}{\gamma}}{mean alpha / gamma}) which results in numbers between 0 and 1 whereas the original formula gives results from 1 (\eqn{\bar{\alpha}=\gamma}{mean(alpha) = gamma}) upwards. An upper end is not defined. However, increasing values indicate increasing heterogeneity in species composition.
  
  The idea of "additive partitioning" (Lande 1996, Vellend 2001, Veech 2002) takes Whittakers approach further. Out of the criticism that \eqn{\beta}{beta} in Whittakers sense does not exhibit the same units (species numbers) as \eqn{\alpha}{alpha}- and \eqn{\gamma}{gamma}-diversity here \eqn{\beta}{beta}-diversity results as the subtract of \eqn{\gamma}{gamma}- and \eqn{\alpha}{alpha}-diversity. It expresses the average amount of diversity not found in a single, randomly-chosen sample. It is also rather calculated for a whole data-set. Here we apply it as well to a moving window of a focal plot and its neighbours if\code{coord} and \code{dn} are set.
  
  The idea of \eqn{\gamma}{gamma}-diversity might be questioned in general, as its quality is not different from \eqn{\alpha}{alpha}. Only the geographic extent is changed and often definition becomes problematic. Imagine a temporal study where different numbers of species are found throughout the years - Is \eqn{\gamma}{gamma} then the overall species richness, or the species richness in one year? Furthermore \eqn{\beta}{beta}-diversity is not clearly defined. There are even more definitions to it then mentioned here (e.g. Qian et al. 2005) so it may be better to use 'differentiation-diversity' instead.
  
  Significance is tested with a simple Monte-Carlo procedure. The initial value of the respective index is tested against a number of values which are calculated from a random reshuffling of the original species matrix. So the hypothesis tested is, that the observed pattern (for each focal plot) is different from random. This is meaningless when \code{coord} and \code{dn} are not set.
}

\value{
  The function returns either a named vector (if \code{coord} and \code{dn} are set) or a data.frame with the following items. The last three are only added if \code{test} = TRUE.
  
  \item{nnbs }{ Number of neighbours of the respective plot. Results from the neighbour definition \code{dn}. If this was not given it reflects the total number of plots in the data-set. }
  \item{n.spec }{ Number of species on the focal plot. If \code{dn} and \code{coord} were not given it reflects the total number of species in the data-set}
  \item{value }{ The calulated \eqn{\beta}{beta}-diversity. The name of the column changes respectively. }
  \item{sim.test }{ The p-value of the significance test for the diversity values. }
  \item{sig }{ The significance decision tested against alpha = 0.05. \code{"*"} means that the respective \eqn{\beta}{beta}-value is significantly different from random. }
  \item{sig.prefix}{ Marks against which tail significance of  \eqn{\beta}{beta}-values was tested. If the initial value is lower than the mean of the permuted values the lower tail is tested (\code{sig.prefix = "-"}) and vice versa (\code{sig.prefix = "+"}). }
}

\references{ 
    Jurasinski, G. & Retzer, V. in prep. Measuring multi-plot similarity with presence-absence data. \emph{Ecology} in prep.
    
    Lande, R. (1996) Statistics and partitioning of species diversity and similarity along multiple communities. \emph{Oikos} \bold{76}: 25-39.
    
    Qian, H., Ricklefs, R. E. & White, P. S. (2005) Beta diversity of angiosperms in temperate floras of eastern Asia and eastern North America. \emph{Ecology Letters} \bold{8}: 15-22.
    
    Veech, J. A., Summerville, K. S., Crist, T. O. & Gering, J. C. (2002) The additive partitioning of species diversity: recent revival of an old idea. \emph{Oikos} \bold{99}: 3-9.
    
    Vellend, M. (2001) Do commonly used indices of beta-diversity measure species turnover? \emph{Journal of Vegetation Science} \bold{12}: 545-552.
    
    Whittaker, R. H. (1960) Vegetation of the Siskiyou Mountains, Orgeon and California. \emph{Ecological Monographs} \bold{30}: 279-338.
}

\author{ Gerald Jurasinski \email{terhorab@mac.com} }

\seealso{ see also \code{\link{sim.pat}}, \code{\link{sim.het}}, \code{\link{sim}}, and for quantitative similarity measures \code{\link[vegan]{vegdist}}, \code{\link[labdsv]{dsvdis}}, \code{\link[stats]{dist}}. More qualitative similarity indices can be calculated with \code{\link[ade4]{dist.binary}} 
}

\examples{
% still to be implemented
}

\keyword{ methods }
\keyword{ multivariate }
