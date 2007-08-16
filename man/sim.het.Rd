\encoding{UTF-8}
\name{sim.het}
\alias{sim.het}
\alias{het2nbs}
\title{ Calculate a mean similarity to adjacent cells in an array }
\description{
  Useful for systematic sampling grids for calculating the mean similarity and its standard deviation between a focal unit and its surrounding units. Surrounding can be specified by the user.
}
\usage{
sim.het(mat, coord=NULL, dn, method="soerensen", test=TRUE, 
permutations=100, ...)

het2nbs(mat, coord=NULL, dn, method="soerensen", ...)
}

\arguments{
  \item{mat}{ Species matrix or \code{dist} object resulting from similarity calculation. }
  \item{coord}{ A \code{data.frame} with two columns containing the coordinates of the plots for which species data or a \code{dist} matrix is given. Defaults to NULL. Then, mean similarity and standard deviation of similarities from each plot to all other plots is calculated}
  \item{dn}{ A numeric or a two value vector. In the first case it gives the distance until which sampling units should be considered as neighbors. In the second the two values define a ring around each plot. Plots which fall into the ring are considered as neighbors. Not necessary and skipped when \code{coord}=NULL}
  \item{method}{ Only necessary when a species matrix is given. Specifies the method for calculating the similarity between plots with \code{\link{sim}}. For more details see the documentation of \code{\link{sim}}. }
  \item{test}{ Shall significance be calculated? Defaults to TRUE which means that it is tested if the observed pattern is significantly different from random}
  \item{permutations}{ Number of permutations for the significance test. Species are permuted on plots and the index is recalculated for each permuted species matrix. The initial values are then tested against the permuted values to obtain a significance level. }
  \item{\dots}{ Further arguments passed to \code{\link{sim}}. }
}

\details{
  If similarities (e.g. of species composition) are calculated between plots in a regular or irregular array (whereas the latter might be problematic because of spatial auto-correlation) the resulting values have no position. However they could be associated to a point in the centre between the compared plots (for each compared pair). Sometimes it is wanted to associate a dissimilarity value to the focal unit which comprises the similarities/dissimilarities of a focal unit to all surrounding units. One possibility is the calculation of a mean similarity (Williams 1996, Lennon 2001, Gaston 2001) out of the single similarities between a focal unit and its surrounding units. This might be problematic as the single similarity values might result from different sets of species. To include the variability of the single similarities between a focal unit and its neighbours as a measure of spatial heterogeneity the variation (sd) in the similarity values is included. See \code{\link{sim.pat}} for the calculation of similarity between a focal unit and its neighbours whilst preserving species identity.
  
  Significance is tested against random expectations with a permutation procedure. After calculating the values for \code{mean} and \code{sd} the species/similarity matrix is permuted and the values are calculated again. This is done \code{permutation} times. Then the initial values are tested against the obtained distribution. If the initial values are under the mean of the respective values among plots they are tested against the lower tail of the permuted distribution. If they exceed the mean, they are tested against the upper tail of the permuted distribution. If a value is significant, this means that it is signifcantly different from a random distribution of species and therefore might likely be caused by underlying envrionmental patterns.
}

\value{
  A \code{\link[base]{data.frame}} with the following columns
  \item{nnbs }{ Number of neighbours of the respective plot. Results from the neighbour definition \code{dn}. }
  \item{m.sims }{ Mean similarity between plot and its neighbours. }
  \item{sd.sims }{ Standard deviation of the similarity values calculated between the plot and its neighbours. }
  \item{sim.test.mean }{ The p-value of the significance test for the \code{mean}-values. }
  \item{sig.mean }{ The significance decision tested against alpha = 0.05. \code{"*"} indicates, that \code{mean} value is significantly different from random. }
  \item{sig.prefix.mean }{ Marks against which tail significance of  \code{mean} was tested. If the initial similarity value is lower than the mean of the permuted values the lower tail is tested (\code{sig.prefix = "-"}) and vice versa (\code{sig.prefix = "+"}). }
  \item{sim.test.sd }{ The p-value of the significance test for the \code{sd}-values. }
  \item{sig.sd }{ The significance decision tested against alpha = 0.05. \code{"*"} indicates, that \code{sd} value is significantly different from random. }
  \item{sig.prefix.sd }{ Marks against which tail significance of  \code{sd} was tested. If the initial similarity value is lower than the mean of the permuted values the lower tail is tested (\code{sig.prefix = "-"}) and vice versa (\code{sig.prefix = "+"}). }
}

\references{ 
    Gaston, K. J., Rodrigues, A. S. L., van Rensburg, B. J., Koleff, P. & Chown, S. L. (2001) Complementary representation and zones of ecological transition. \emph{Ecology Letters} \bold{4}: 4-9.
    
    Jurasinski, G. & Retzer, V. in prep. Measuring multi-plot similarity with presence-absence data. \emph{Ecology}
    
    Lennon, J. J., Koleff, P., Greenwood, J. J. D. & Gaston, K. J. (2001) The geographical structure of British bird distributions: diversity, spatial turnover and scale. \emph{Journal of Animal Ecology} \bold{70}: 966-979.
    
    Williams, P. H. (1996) Mapping variations in the strength and breadth of biogeographic transition zones using species turnover. \emph{Proceedings of the Royal Society of London Series B-Biological Sciences} \bold{263}: 579-588.
    
}

\author{ Gerald Jurasinski \email{terhorab@mac.com}; Vroni Retzer \email{vroni.retzer@uni.bayreuth.de} }

\seealso{ See also \code{\link{sim.pat}} and \code{\link{sim}} }

\examples{

data(abis)
## calculate average similarity for the focal plots
abis.het <- sim.het(abis.spec, coord=abis.env[,1:2], dn=100)
    
}

\keyword{ methods }
\keyword{ multivariate }
