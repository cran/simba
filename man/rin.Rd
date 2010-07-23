\encoding{UTF-8}
\name{rin}
\alias{rin}
\alias{mpd}
\alias{mps}
\alias{mps.ave}
\alias{mps.f}
\alias{sos}
\title{Calculate multiple plot resemblance measures}
\description{The functions calculate several multiple plot similarity measures. In addition \code{rin} provides a wrapper that allows for the easy calculation of multiple plot (site) \strong{r}esemblance measures \strong{i}n \strong{n}eighboorhoods in an automated fashion including testing whether the found resemblance patterns are significantly different from random.}

\usage{
mpd(x, method="simpson", all=FALSE)

mps(x, method="whittaker", all=FALSE)

mps.ave(x, method="soerensen", all=FALSE, foc=NULL, what="mean", ...)

mps.f(x, foc, d.inc=FALSE, preso=FALSE)

sos(x, method="mean", foc=NULL, normal.sp=TRUE, normal.pl=TRUE)

rin(veg, coord, dn, func, test = TRUE, permutations = 100, permute = 2, sfno = TRUE, p.level = 0.05, ...)
}

\arguments{
  \item{x}{ Species composition data, a matrix-like object. }
  \item{method}{ Method for the calculation of multiple plot resemblance. The possible choices depend on the function used and include (among others) Simpson based multiple plot dissimilarity, Sørensen based multiple plot dissimilarity, Nestedness based multiple plot dissimilarity, Whittaker's beta, additive partitioning, Harrison multiple plot similarity, Harrison multiple plot turnover, Williams multiple plot turnover, average pairwise similarity (with a similarity measure of your choice from \code{\link{sim}}), Diserud & Ødegaard multiple plot similarity. The methods of \code{mps.f} (a new group of multiple plot similarity measures) evolve from setting the arguments accordingly. For \code{sos} the choice is \code{mean} or \code{foc}. See details. }
  \item{all}{ Logical. Depending on the function this argument has a different meaning. In \code{mps} and \code{mpd} it sets whether the results of all possible methods shall be given in the result, or only the method given in the \code{method} argument. Because some of the measures are just derived from others all methods are always calculated within the function when it is called and the \code{method} argument just triggers which to give back. In \code{mps.ave} it sets whether all statistics calculated (\code{\link[base]{mean}} and \code{\link[stats]{sd}}) shall be given back or only the one specified by the \code{what} argument. }
  \item{foc}{ Character vector with length one or an integer specifying which one is the focal plot. Three of the functions are/can be sensitive to the species composition in the focal plot (\code{mps.f}, \code{mps.ave}, \code{sos}). The automation function \code{rin} is able to automatically derive the identity of the focal plot. Just set \code{foc = foc} in the \code{func} argumemt (see example). When \code{mps.ave} or \code{mps.f} are used stand alone either the name of the plot in parenthesis or the index of the plot within the species matrix (\code{x}) has to be given. }
  \item{what}{ For \code{mps.ave}, which statistic (\code{mean} or \code{sd}) should be given back? See details. }
  \item{d.inc}{ Logical. Shall all species that are within \code{veg} but not within the plots that make up a neighborhood be regarded when computing \code{mps.f}. This setting dramatically changes the behaviour of \code{mps.f} because it then becomes a symmetric similarity coefficient. Defaults to \code{FALSE} so that an asymmetric multiple plot similarity coefficient is computed. Only makes sense when \code{mps.f} is applied within \code{rin} and changes nothing otherwise. }
  \item{preso}{ Logical. Shall a presence only version of \code{mps.f} be computed? Default is \code{FALSE}. See details. }
  \item{normal.sp }{ In case of \code{sos} (\strong{s}um \strong{o}f \strong{s}quares of species matrix, which is a measure of beta-diversity (Legendre et al. 2005)): Shall the result be normalized with respect to the number of species. }
  \item{normal.pl }{ In case of \code{sos} (\strong{s}um \strong{o}f \strong{s}quares of species matrix, which is a measure of beta-diversity (Legendre et al. 2005)): Shall the result be normalized with respect to the number of plots. }
  \item{veg}{ Species composition data, a matrix-like object that is ought to be recorded in a regular array or a similar structure and that shall be divided into neighborhoods with a moving window so that each plot becomes the focal plot with a certain neighborhood of plots around for which the multiple plot resemblance measures are then calculated. }
  \item{coord}{ Spatial coordinates of the field plots where the data in veg comes from. The function expects a \code{data.frame} with two columns with the first column giving the x (easting) coordinate and the second giving the y (northing) coordinate in UTM or the like. These coordinates are used to calculate the neighborhoods within a moving window approach. }
  \item{dn}{ Distance to neighbors or neighbor definition. A positive numeric or a two value vector (also positive numeric). In the first case it gives the distance from each sampling unit in m until which other sampling units should be seen as neighbours. In the second the two values define a ring around each plot. Plots that fall into the ring are considered as neighbors. See details. }
  \item{func}{ A character string that defines the formula which shall be applied to calculate a multiple plot resemblance measure for all possible neighborhoods within an array. For instance \code{"mpd(x)"} to compute the Simpson multiple plot dissimilarity coefficient sensu Baselga (2010). See details. }
  \item{test}{ Logical. Shall the significance of the calculated values of multiple plot resemblance be tested regarding its deviation from random expectations. Defaults to \code{TRUE}. See details. }
  \item{permutations}{ The number of permutations run for testing the significance. Defaults to 100. And it is already slow. So test before you give much higher number of runs here. }
  \item{permute}{ When testing with \code{rin}, how should the permutation of species to reflect random expectations be done: An integer of either 1, 2, or 3. With \code{1} the species matrix (\code{veg}) is permuted across rows. With \code{2} the species matrix (\code{veg}) is permuted across columns. With \code{3} the species in the focal plot are permuted (They are randomly drawn from the species pool). }
  \item{sfno}{ Species from neigborhood only? Logical, that is only be set in combination with \code{permute} = 3. If \code{TRUE}, than the species are only drawn at random from the neighboorhod species sub matrix. If set to \code{FALSE}, the species are drawn at random from the whole species matrix \code{veg}. }
  \item{p.level}{ Significance level below which the resemblance patterns shall be considered as significantly different from random expectations. Defaults to 0.05. Enables to give asteriks and stars in the results. }
  \item{\dots}{ Further arguments to the workhorse functions \code{mpd}, \code{mps}, \code{mps.ave}, \code{mps.f} can be passed via \dots. }

}

\details{
Several multiple plot similarity indices have been presented that cure some of the problems associated with the approaches for the calculation of compositional similarity for groups of plots by averaging pairwise similarities (Diserud and Ødegaard 2007, Baselga 2010). These indices calculate the similarity between more than two plots whilst considering the species composition on all compared plots. The resulting similarity value is true for the whole group of plots considered (called neighborhood in the following). Further, there are multiple plot similarity coefficients that are determined by the species composition on a reference plot (named focal plot in the following). All of these, can be calculated with the functions described in this help file. See vignette for an overview table. Further, the function \code{rin} takes all of them and provides a framework for applying the measures to an array of plots to calculate multiple plot \strong{r}esemblance \strong{i}n \strong{n}eighborhoods (Jurasinski et al. submitted).

\code{mps} stands for \strong{m}ultiple \strong{p}lot \strong{s}imilarity, whereas \code{mpd} stands for \strong{m}ultiple \strong{p}lot \strong{d}issimilarity; the letters behind the . further specifiy the class of measures that can be calculated with the respective function.

\code{mps.ave} calculates \strong{ave}rage multiple plot (dis-)similarities from pairwise (dis-)similarity calculations between the plots in the dataset or in the specified neighborhood. It has several options. With setting the \code{foc} argument different from NULL, only the pairwise (dis-)similarities between the specified focal plot and all others in the dataset (neighborhood) are taken to calculate the \code{\link[base]{mean}} and \code{\link[stats]{sd}} from. When the specified focal plot is not existing, the function will issue a warning and stop. When run with defaults (\code{foc} = NULL), all pairwise similarities between the plots in the neighborhood (dataset) are considered. Any resemblance measure available via \code{\link{sim}} or \code{\link{sim.yo}} can be taken as base for calculating the average (dis-)similarity and its spread.

\code{mps} calculates \strong{m}ultiple \strong{p}lot (dis-)\strong{s}imilarities that are either derived from other approaches to beta-diversity calculation (Whittaker's beta, additive partitioning), or have been around for quite a while (Harrison multiple plot dissimilarity, Harrison multiple plot turnover, Williams multiple plot turnover). None of these considers the actual species composition on each of the compared plots. The following methods are available (n = number of plots, S = number of species, \eqn{\gamma = gamma diversity (S_n)}, \eqn{\alpha = alpha diversity (S_i)}):

\tabular{ll}{ 
	\code{whittaker} \tab Calculates Whittaker's beta (multiplicative partitioning, Whittaker 1960) \eqn{\beta = \gamma/mean(\alpha)}. \cr
	\code{inverse.whittaker} \tab Inverse Whittaker's beta (multiplicative partitioning). Scales between 1/n (when the considered plots do not share any species at all) and 1 (when all plots share the same species) \cr
	\code{additive} \tab Additive partitioning. Following Lande (1996) and keeping it with \eqn{\alpha} = species number, the additive beta-component of the neighborhood (in the \code{rin}-case or the complete dataset in the \code{mps}-case) is calculated. \cr
	\code{harrison} \tab Harrison (1992) multiple plot dissimilarity. A transformation of Whittaker's beta to be bounded between 0 and 1 (\eqn{\frac{\beta_W - 1}{n-1}}{(\beta_W-1)/(n-1)}. \cr
	\code{diserud} \tab Diserud & Ødegaard (2007) derived this from the pairwise Sørensen similarity measure. However, as Baselga highlights, this can also be derived from Whittaker's beta \eqn{\frac{n - \beta_W }{n-1}}{(n - \beta_W)/(n-1)} and is basically the same as Harrisons multiple plot dissimilarity but expressed as a similarity. \cr
	\code{harrison.turnover} \tab \eqn{\frac{\frac{\gamma}{max(\alpha)}-1}{n-1}}{((\gamma/max(\alpha))-1)/(n-1)} (Harrison et al. 1992)\cr
	\code{williams} \tab \eqn{1 - \frac{max(\alpha)}{\gamma}}{1 - max(\alpha)/\gamma} (Williams 1996) \cr}

\code{mpd} calculates \strong{m}ultiple \strong{p}lot \strong{d}issimilarity indices that have been suggested by Baselga (2010). The following methods are available (The implementation differs slightly from the one offered by Baselga in the electronic appendix of his paper and is computationally more efficient):

\tabular{ll}{ 
	\code{simpson} \tab mps.Sim in the following. Baselga et al. (2007) derive this multiple plot dissimilarity coefficient directly from the pairwise Simpson dissimilarity index by applying it to a group of plots/sites. The authors emphasize, that this coefficient is independent of patterns of richness and peforms better than the Diserud & Ødegaard cofficient in cases of unequal species numbers between plots, because it discriminates between situations in which shared species are distributed evenly among plots or concentrated in a few pairs of sites. \cr
	\code{sorensen} \tab mps.Sor in the following. By building multiple site equivalents of the matching components (a, b, c) Baselga (2010) derives a Sørensen based measure of multiple plot dissimilarity: \cr
	\code{nestedness} \tab mps.nes in the following. Because the Sørensen based multiple plot dissimilarity coefficient accounts for both spatial turnover and nestedness whilst the Simpson based multiple plot dissimilarity coefficient accounts only for spatial turnover, it is possible to calculate the multiple plot similarity that is completely due to nestedness by calculating mps.Sor - mps.Sim \cr}

\code{mps.f} calculates \strong{f}ocal \strong{m}ultiple \strong{p}lot \strong{s}imilarities. In contrast to the other functions the different outcomes can be triggered by setting the further arguments accordingly.

The indices of \code{mps.f} are the only ones whose value changes depending on the vegetation composition of the focal plot. It is therefore true and valid only for the comparison of the focal plot with the surrounding plots. Not the similarity in the neighborhood, but the similarity of the focal plot to all others in the neighborhood is calculated. The calculation is based on the occurrences and non-occurrences of species on the compared plots with the species composition on the focal plot determining which of the two is to be used for which species: For all species that occur on the focal plot the proportional frequencies of occurrence in the neighborhood are summed up. For species that do not occur on the focal plot the proportional frequencies of non-occurrence in the neighborhood are summed up. 

\deqn{\sum_{i=1}^{s_o} f_{oi} + \sum_{i=1}^{s_n} f_{ni}}{sum(f_oi)+sum(f_ni)}

with f_oi = proportional frequency of occurrences of the ith species on the compared plots, only carried out for species that do occur on the focal plot, f_nj = frequency of non-occurrences of the jth species on the compared plots, only carried out for species that do not occur on the focal plot). The frequencies are calculated against the total numbers of cells in the species matrix and are therefore 'proportional frequencies' (in analogy to 'proportional abundances' as in diversity indices like Shannon or Simpson). Thus, if all compared plots have an identical species composition, the resulting value of the multi-plot similarity coefficient is 1. In this rather hypothetical case the species presence absence matrix would be filled with ones only. This is the null model against which the 'proportional frequencies' are calculated. Therefore, the coefficient can be interpreted as a measure of deviation from complete uniformity. There are three versions.

\tabular{ll}{ 
	\code{preso=TRUE} \tab In this case a presence only version is calculated (\code{mps.fpo}). Therefore the second term is skipped and the formula simplifies to \eqn{\sum_{i=1}^{s_o} f_{oi}}{sum(f_oi)}. This very much glorifies the species composition on the focal plot and evaluates whether the surrounding plots in the neighborhood feature the same species. \cr
	\code{d.inc=FALSE} \tab When the \code{d.inc} argument is set to \code{FALSE}, only the species in the neighborhood build the basis against which the 'proportional frequencies' are calculated. This is the default index \code{mps.f}. \cr}

When run with defaults (\code{preso = FALSE})  and (\code{d.inc = TRUE}), a \strong{s}ymmetric \strong{f}ocal \strong{m}ultiple \strong{p}lot \strong{s}imilarity (\code{mps.fs}) index results. It is definetely meant for use in the context of \code{rin}. The 'proportional frequencies' are calculated against the whole species matrix. Thus, the index is a symmetric similarity coefficient sensu Legendre & Legendre 1998 that considers species that do not occur on the compared plots but in the whole data set. Therefore, it is more appropriate for biodiversity or conservation studies and not so much for the investigation of ecological relationships. However, it can be interpreted as an 'ordination on the spot': By calculating \code{mps.fs} for a focal plot against its surrounding plots its position along the main gradient according to its species composition is estimated immediately because the species composition in the rest of the data set is incorporated in the construction of the proportional frequencies of the species. Because of this, \code{mps.fs} can be interpreted as a measure of deviation from complete unity in species composition. When the neighborhood is increased to the full data set, \code{mps.f} and \code{mps.fs} converge.

\code{sos} calculates the \strong{s}um \strong{o}f \strong{s}quares of a species matrix. Legendre et al. (2005) show, that this is a measure of beta-diversity. However, when you don't normalize against the number of species and/or plots the obtained values can hardly be compared across data sets (or neighborhoods). Therefore, its advisable to run this with defaults (\code{normal.sp = TRUE} and \code{normal.pl = TRUE}). For experiments, \code{method} can be set to \code{"foc"}. Then, not the deviation from the mean of the species occurence across plots builds the basis, but the deviation from the situation on a focal plot. This makes it somewhat related to the \code{mps.f}-stuff.

\code{rin} applies the other functions to an array of plots. For each plot a neighborhood is constructed via the \code{dn} argument and the specified index is calculated for all plots and neighborhoods. The function to be calculated is specified simply by the \code{func} argument. For instance, with \code{func = "mpd(x, method='sorensen')"} the function \code{rin} calculates the Sørensen multiple plot dissimilarity for each plot and its neighborhood in an array. The functions that need the identity of a focal plot (\code{mps.ave} and \code{mps.f}) automatically derive the focal plots. However, to trigger this it has to be specified within the \code{func} argument: \code{func = "mps.f(x, foc = foc)"}.
}

\value{
The functions \code{mpd}, \code{mps}, \code{mps.ave}, \code{mps.f} return a single value with the calculated index (according to the \code{method} argument, or to the other arguments). When \code{all} is set to \code{TRUE}, \code{mps.ave} returns two values (the average and the standard deviation of the pairwise similarities in the neighborhood), whereas \code{mpd} and \code{mps} return a named numerical vector with the values for all indices that can be calculated with the respective function.

\code{rin} gives back a table (\code{data.frame}s), that reports several values for each plot in the dataset per row. The first three columns are always returned. In case \code{test = TRUE}, three more columns with information on the significance test are returned.

	\item{n.plots }{ Number of plots that make up the neighborhood. }
	\item{n.spec }{ Number of species that occur in the neighborhood. }
	\item{dis }{ Value of the calculated (dis)similarity index per plot. }
	\item{p.val }{ p value of the permutation test. According to the \code{permute} argument the data set is shuffled. The random data is subjected to the same calculations \code{permutations} times. The original value of multiple plot similarity is compared to the distribution of random values to obtain this p. }
	\item{sig }{ Significance flag. Just a translation of the p value into a significance flag. There are only two possibilities: "*" value is significantly different from random, "ns" value is not significantly different from random. }
	\item{sig.sign }{ The sign of the significance value. The tail which is tested is determined by the relation of the multiple plot similarity value to the average multiple plot similarity value of the random test distribution. Thus, the sign shows whether the multiple plot similarity is significantly higher than can be expected from random expectations (\code{+}) of lower (\code{-}). }
}

\references{
Baselga A (2007) A multiple–site similarity measure independent of richness. \emph{Biology Letters} 3: 642–645.	

Baselga A (2010) Partitioning the turnover and nestedness components of beta diversity. \emph{Global Ecology and Biogeography} 19: 134–143.

Diserud OH, Ødegaard F (2007) A multiple–site similarity measure. \emph{Biology Letters} 3: 20–22.

Harrison S, Ross SJ, Lawton JH (1992) Beta-diversity on geographic gradients in Britain. \emph{Journal of Animal Ecology} 61: 151–158.

Jurasinski G, Jentsch A, Retzer V, Beierkuhnlein C (submitted) Assessing gradients in species composition with multiple plot similarity coefficients. \emph{Ecography}

Lande R (1996) Statistics and partitioning of species diversity, and similarity among multiple communities. \emph{Oikos} 76: 5–13.

Legendre P, Borcard D, Peres-Neto P (2005) Analyzing beta diversity: partitioning the spatial variation of community composition data. \emph{Ecological Monographs} 75: 435–450.

Williams PH (1996) Mapping variations in the strength and breadth of biogeographic transition zones using species turnover. \emph{Proceedings of the Royal Society of London Series B–Biological Sciences} 263: 579–588.

Whittaker RH (1960) Vegetation of the Siskiyou Mountains, Oregon and California. \emph{Ecological Monographs} 30: 279–338.
}

\author{ Gerald Jurasinski \email{gerald.jurasinski@uni-rostock.de}, with contributions by Vroni Retzer \email{vroni.retzer@gmx.de} }

\note{
\code{rin} is not optimized and could perhaps profit from some C code. So when \code{test = TRUE} it takes a while because of the permutations.
}

\seealso{
\code{\link{sim}}, \code{\link[vegan]{vegdist}}, \code{\link[labdsv]{dsvdis}} for pairwise similarity measures.
}

\examples{
\dontrun{
# load the data that comes with the package
data(abis)

# calculate a multiple plot similarity index 
# (Sørensen sensu Baselga) for whole dataset
mpd(abis.spec, method="sorensen")

# calculate a multiple plot similarity index
# (Sørensen sensu Baselga) for each plot and 
# its neighborhood
abis.mpd.so <- rin(abis.spec, coord=abis.env[,1:2], 
dn=100, func="mpd(x, method='sorensen')")

# plot the grid of plots and show the calculated 
# multiple plot dissimilarity value through the 
# size of the symbol and the sign of the value
# with a superimposed "+" or "-"
with(abis.mpd.so , {
plot(abis.env[,1:2], cex=symbol.size(dis), pch=c(21,1)[sig], bg="grey50", xlab="", ylab="")
subs <- sig == "*"
points(abis.env[subs,1:2], pch=c("-", "+")[sig.prefix[subs]])
})

# calculate a multiple plot similarity index
# that takes care of the species composition
# on the focal plot
rin(abis.spec, coord=abis.env[,1:2], test=FALSE,
dn=100, func="mps.f(x, foc=foc)")
}
}

\keyword{ multivariate }
\keyword{ htest }
\keyword{ array }
