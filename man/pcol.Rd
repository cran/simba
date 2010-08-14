\encoding{UTF-8}
\name{pcol}
\alias{pcol}
\alias{permcor}
\alias{permcor2}
\alias{mancor}
\alias{plot.permcor}
\title{ Permuted Correlation (on strata) }
\description{
The function is a wrapper for several functions related to the permuted correlation between matrices or vectors: It calls \code{\link{permcor2}} to calculate permuted correlation between vectors and \code{\link{permcor}} to calculate permuted correlation on strata. This can be useful to obtain data-points for a multivariate Mantel correlogram. Two matrices or vectors and a variable dividing these vectors into strata (levels) are to be specified. If the last is not given only permuted correlation between the two matrices (\code{\link[stats]{dist}}-objects) is done. If the second matrix/vector is a subsetting object the correlation can be done on the first object for each of the strata with \code{\link{mancor}}.
}

\usage{
pcol(x, y, z = NULL, method="pearson", permutations=1000, 
	solo=FALSE, ...)

permcor(x, y, subsetter, method="pearson", permutations=1000, 
	alpha=0.05, trace=FALSE, ...)

permcor2(x, y, method="pearson", permutations=1000, subset=NULL, 
	complete=TRUE, loop=FALSE, ...)

mancor(dis, classes, width=NULL, method="pearson", 
	permutations=1000, alpha=0.05, trace=FALSE, ...)

\method{plot}{permcor}(x, y, ...)
}
\arguments{
  \item{x}{ Matrix (\code{dist}-objekt) or vector of numeric values (atomic) containing distances or similarities returned by \code{\link[simba]{sim}}, \code{\link[vegan]{vegdist}}, \code{\link[stats]{dist}}, \code{\link[ade4]{dist.binary}} or a similar matrix. Conversion is done automatically and triggered by \code{\link[base]{is.vector}}. If it is not a vector it is assumed to be a \code{dist}-objekt or a similar matrix (with nrow==ncol). For the plotting method a \code{permcor}-object. }
  \item{y}{ If \code{solo}=FALSE an object of the same qualities and dimensions (or length) as \code{x} to correlate to. See \code{Details} for the case \code{solo}=TRUE. For the plotting method an optional object. Not needed here. }
  \item{z}{ Vector or \code{dist}-objekt of a variable which defines the strata, or levels for which the permuted correlation shall be calculated. Doesn't have to be a factor but it has to be convertible into factor. see \code{Details} for more.}
  \item{subsetter}{ Vector of a variable which defines the strata, or levels for which the permuted correlation shall be calculated. Usually conversion from \code{dist}-object to vector is done in \code{pcol}. If \code{permcor} is run separately it has to be a vector (same holds for \code{x}, \code{y}, and \code{z} if the background functions are run separately). Doesn't have to be a factor but it has to be convertible into factor. see \code{Details} for more.}
  \item{method}{ Method of correlation, as it is done by \code{\link[stats]{cor.test}}, see help there for details. Defaults to Pearson correlation coefficients. Other options are Kendall and Spearman rank correlations.}
  \item{permutations}{ number of permutations, defaults to 1000 to get a significance level of p = 0.001. }
  \item{solo}{ If TRUE \code{y} is assumed to be a subsetting matrix, \code{dist}-object or vector giving strata to correlate \code{x} with and calculate the data-points for a Multivariate Mantel Correlogram for one distance-matrix.}
  \item{width}{ If \code{solo}=TRUE the subsetting object is assumed to contain classes already. If \code{width} is specified (defaults to NULL), the classes are defined inside the function and \code{width} gives the class width.}
  \item{trace}{ Set to TRUE to follow the runs of the for-loops in functions \code{permcor} and \code{mancor}. See \code{details} for when it is appropriate to set via \code{pcol}.}
  \item{complete}{ Should only complete cases be considered? Defaults to TRUE. }
  \item{alpha}{ The initial alpha-level against which should be tested. Depending on sub-function it is internally changed via Bonferroni-correction if necessary. }
  \item{subset}{ If Case is 1 (see \code{details}) a subset of cases from \code{x} and \code{y} can be defined for correlation.}
  \item{loop}{ Triggers the method for permutation inside the function. Shall it be looped (\code{for}-loop, \code{loop = TRUE}) or be done by an \code{\link[base]{apply}} method (\code{loop = FALSE})? Determines speed. For the most, reasonably huge data sets, the latter will be better. }
  \item{dis}{ Same as \code{x} for \code{mancor}. }
  \item{classes}{ If \code{mancor} shall be run, second item is a vector or \code{dist}-objekt of a variable which defines the strata, or levels for which the permuted correlation shall be calculated. Doesn't have to be a factor but it has to be convertible into factor. see \code{Details} for more.}
  \item{permcor}{ (In \code{plot.permcor}) an object returned by \code{permcor2} is easily plotted with \code{plot.permcor}. The actual correlation value is plotted against an histogram of the distribution of the permuted values. }
  \item{\dots}{ Arguments to other functions, for instance to \code{\link[stats]{cor.test}} regarding specifications of the test, however only the correlation value is taken from this function. but here you could change from pearson to kendall for instance. }
}

\details{
    \code{pcol} is a wrapper for the other functions. Depending on the input and the setting of \code{solo} the following functions are invoked (They can also be run separately. In this case \code{x}, \code{y}, and \code{z} must be given as vectors).
    
    \enumerate{
    \item If \code{x} and \code{y} are \code{\link[stats]{dist}}-objects, or vectors containing distance or similarity values and everything else is set to defaults a simple permuted correlation with \code{permcor2} is run. This corresponds to a Mantel test. The two data-objects are correlated with \code{\link[stats]{cor.test}}, then the \code{y} is permuted and with \code{\link[stats]{cor}} the correlation is calculated again and written to a vector. This is repeated \code{permutation} times. Finally, the initial correlation value is compared to the permuted values. The number of times, the permuted values exceed the initial value is divided by the number of permutations to obtain a significance value. Thus, with 1000 permutations a minimum p of 0.001 can be tested.
    
    \item If a \code{subsetter} is given in \code{z}, the permuted correlation is done for every stratum or level given by the subsetting object - this could e.g. be direction or distance classes flagging which plots share a similar distance and therefore fall into the same class. The resulting data-points can be used to plot a correlogram which allows for the analysis of non-stationarity in the relationships between \code{x} and \code{y}.
    
    \item If \code{y} is itself a subsetting object (distance classes or so) you have to set \code{solo}=TRUE. Than the matrix or vector in \code{x} is correlated against this classes. This is handled by \code{mancor}. The parameter \code{width} allows for the calculation of classes inside the function. If for instance a distance-matrix with geographical distances is given, \code{width} specifies the width of the distance classes, they are computed and used to correlate the data in \code{x} against. This produces the data-points for a multivariate Mantel correlogram in the sense of Oden & Sokal (1986) (see also Legendre & Legendre 1998 for a comprehensive coverage of the subject).
    }
}

\value{
  Returns different objects, depending on given arguments and triggers.
  
\item{case 1 }{a \code{permcor}-object with the following items is returned:
  
  \itemize{
  \item{call }{ The call to the function. }
  \item{method }{ The correlation method as used by \code{\link[stats]{cor.test}}. }
  \item{statistic }{ The initial correlation value which is tested against the permuted values. }
  \item{signif }{ The significance of the calculation. }
  \item{n }{ The number of cases. }
  \item{permutations }{ The number of permutations as specified by \code{permutations}. }
  \item{perms }{ The result of the permuted runs. It is not printed by default but can be accessed via \code{result\$perms}. The correlation value can be plotted against an histogram of the distribution of the permuted values to visualize the significance with the default plotting method.}
  }}
  
\item{ case 2}{ a \code{pclist}-object with the following items (in this case it might be nice to set \code{trace}=TRUE to display the progress of the calculation) is returned:
  
  \itemize{
  \item{call }{ The call to the function. }
  \item{method }{ The correlation method as used by \code{\link[stats]{cor.test}}. }
  \item{gesN }{ The total number of cases. }
  \item{strata }{ The number of strata (or levels) for which permutation has been done. }
  \item{permutations }{ The number of permutations as specified by \code{permutations}. }
  \item{out }{ A \code{data.frame} with 3 columns containing the result for each stratum in the rows: \code{statistic} contains the correlation values for the corresponding stratum, \code{sig} the obtained significance, and \code{nop} the number of cases found and used for permutation on this very level. }
  }}
  
\item{ case 3 }{ a \code{pclist}-object with the same items as in Case 2 (in this case it might be as well interesting to set \code{trace}=TRUE to display the progress of the calculation) is returned. }

}

\references{ 
    Legendre, P, & Legendre, L. (1998) \emph{Numerical Ecology}. 2nd English Edition. Elsevier.
    
    Oden, N. L. & Sokal, R. R. (1986) Directional Autocorrelation: An Extension of Spatial Correlograms to Two Dimensions. \emph{Systematic Zoology} \bold{35}: 608-617.
    }

\author{ Gerald Jurasinski }

\note{ Depending on the background-function and the size of the matrices or vectors it may take a while to calculate. The slowest is \code{mancor} (case 3). }

\seealso{ \code{\link[vegan]{mantel}} for a different implementation of Mantel tests, \code{\link[stats]{cor.test}} }

\examples{
    
data(abis)

## calulcate soerensen of species data
abis.soer <- sim(abis.spec)
## calculate distance (Euclidean) regarding some disturbance 
## variables (feces counts)
abis.pert <- dist(abis.env[,19:25])
## are compositional similarity and dissimilarity of disturbance related?
pcol(abis.soer, abis.pert)
## the relationship is significant, but not very strong    

}

\keyword{ methods }
\keyword{ htest }
