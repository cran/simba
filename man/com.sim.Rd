\encoding{UTF-8}
\name{com.sim}
\alias{com.sim}
\title{ Compare mean similarity between subsets of data }
\description{
  Related to \code{\link[vegan]{mrpp}}. Are the differences in mean similarity between data subsets significant? Function takes the whole data-set (species matrix) and a subsetting vector and computes a specified similarity between all sampling units (rows). Then subsets are compared regarding their mean similarity. Statistical inference is obtained through permutation.
}
\usage{
com.sim(veg, subs, simil = "soerensen", binary = TRUE, 
    permutations = 1000, alpha = 0.05, bonfc = TRUE, ...)
}

\arguments{
  \item{veg}{ Species matrix with columns = sites, rows = species. Deliver presence/absence data or abundance data. However, \code{binary} has to be set accordingly. }
  \item{subs}{ Vector containing the subset definition. Same entries are understood to indicate belonging to the same subset (can be characters, factors or numerics). For each subset similarities/distances are calculated. Then all subsets are compared regarding mean and variance of the similarities/distances. }
  \item{simil}{ Sets the coefficient to be used for calculating similarities/distances. If \code{binary} = TRUE, see \code{\link[simba]{sim}}, otherwise see \code{\link[vegan]{vegdist}} for possible choices. }
  \item{binary}{ Changes the function used for the calculation of similarity/distance. If binary species data ist provided in \code{veg} keep the default (\code{binary} = TRUE). In this case \code{\link[simba]{sim}} is used to calculate the similarities. Set to FALSE when abundance or frequency data is provided. This calls \code{\link[vegan]{vegdist}} to calculate the distances between sites in species similarity space. }
  \item{permutations}{ Number of permutations performed to obtain the statistical inference. See Details. }
  \item{alpha}{ Initial alpha level to test against. Defaults to 0.05. }
  \item{bonfc}{ Shall Bonferroni correction be applied? Defaults to true. }
  \item{\dots}{ Further arguments to functions. }
}
\details{
  Entries of similarity/distance matrices are not independent. Therefore normal statistics might fail. One possibility is the application of permutation procedures. This means that the statistical distribution against which significance is tested is derived from the data. 
  
  Here it is implemented as follows: For each subset the similarities/distances between all sites (plots) are calculated with the specified coefficient. Then the resulting similarity/distance matrices are compared with \code{\link[simba]{diffmean}}. This is done for the comparison of each subset with each other subset. If specified (defaults to TRUE), Bonferroni correction is applied (to correct for multiple testing).
  
  Depending on the number of subsets and the number of sites per subset it may take some seconds to be computed.
}

\value{
  Returns an object of class \code{cslist} containing the call to the function, the used method for similarity/distance calculation, a comparison matrix showing the connections between data-subsets (rows and columns connected with "*" are significantly different), the number of subsets involved, the number of permutations and a matrix giving information about the following components for each comparison between subsets:
  
  \item{X }{ Subset identifier for one of the compared subsets }
  \item{Y }{ Subset identifier for the other compared subset }
  \item{mean.x }{ Average distance/similarity for subset X. }
  \item{mean.y }{ Average distance/similarity for subset Y. }
  \item{diff }{ Difference in average distance/similarity for this comparison }
  \item{sig }{ Significance of the difference in mean of the similarities. }
  \item{sigs }{ Significance flag for the comparison ("*" means significant differences, "ns" means that the differences are not significant). }
  \item{Fval }{ F-value for the Comparison. }
  \item{sigF }{ Is F significant? }
  \item{sigsF }{ Significance flag for F. }

}

\author{ Gerald Jurasinski }

\seealso{ \code{\link[vegan]{mrpp}} for an anova like approach for comparing the differences of species data subsets. }

\examples{
data(abis)

## see environmental data (see documentation on data for details)
abis.env

## calculate the difference in similarities for the three major  
## vegetation types
## therefore create a vector from the data expressing belonging
## to the vegetation types:
tcs.sub <- rep(0, 61)
tcs.sub[abis.env[,29]==1] <- 1
tcs.sub[abis.env[,30]==1] <- 2
tcs.sub[abis.env[,31]==1] <- 3

## calulate differences with Bray-Curtis as the distance measure
com.sim(abis.spec, tcs.sub, simil="bray", binary=FALSE)

## calculate differences with Soerensen as the similarity measure
com.sim(abis.spec, tcs.sub)


}

\keyword{ methods }
\keyword{ multivariate }
