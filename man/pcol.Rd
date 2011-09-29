\encoding{UTF-8}
\name{pcol}
\alias{pcol}
\alias{mantl}
\alias{plot.mantl}
\title{ Permuted Correlation (on strata) }
\description{
The function is a wrapper for several functions related to the permuted correlation between distance matrices: calculate permuted correlation between vectors or calculate permuted correlation on strata. This can be useful to obtain data-points for a multivariate Mantel correlogram. Two matrices and a matrix dividing these into strata (levels) are to be specified.
}

\usage{
pcol(x, y = NULL, z = NULL, width = NULL, bins = 5, method = "pearson", 
permutations = 1000, alpha = 0.5, trace = FALSE, ...)

mantl(x, y, method = "pearson", permutations = 1000, sub = NULL,
loop = FALSE, ...)

\method{plot}{mantl}(x, y, ...)
}

\arguments{
  \item{x}{ \code{dist}-object containing distances or similarities returned by \code{\link[simba]{sim}}, \code{\link[vegan]{vegdist}}, \code{\link[stats]{dist}}, \code{\link[ade4]{dist.binary}} or a complete resemblance matrix (nrow==ncol). For the plotting method a \code{mantl}-object. }
  
  \item{y}{ A similar object like x with the same dimensions representing resemblance between the same objects as for x but based on other variables. Defaults to \code{NULL}. See details}
  
  \item{z}{ A similar object like x containing distances or distance classes. If the latter is true set \code{width} to 1 (\code{bins} is without effect then). If \code{y} is provided the function calculates Mantel statistic for the correlation between \code{x} and \code{y} on the strata that are defined by \code{z}. If \code{y} is not given the function correlates the \code{x} against the classes in \code{z} leading to data points for a multivariate Mantel correlogram in the sense of Olden & Sokal (1986). See details. }
  
  \item{width}{ Numeric. If \code{width}=1 \code{z} is assumed to contain classes already. If \code{width} is \code{NULL} (default), the classes are defined inside the function by using \code{bins} to derive \code{n = bins} distance classes that span the same distance range. If \code{width} is specified this gives the bin width instead of a number of bins.}
  
    \item{bins}{ Numeric. If \code{width} is \code{NULL} (default), the distance classes derived from \code{z} are defined inside the function by using \code{bins} to derive \code{n = bins} distance classes that span the same distance range. Defaults to 5 bins. }
  
  \item{method}{ Method of correlation, as it is done by \code{\link[stats]{cor.test}}, see help there for details. Defaults to Pearson correlation coefficients. Other options are Kendall and Spearman rank correlations.}
  
  \item{permutations}{ Integer giving the number of permutations in \code{mantl}, defaults to 1000 to get a significance level of p = 0.001. }
  
  \item{alpha}{ Numeric. The initial alpha-level against which should be tested. In case of testing on strata (\code{z != NULL})it is internally corrected using Bonferronis method. }

  \item{trace}{ Logical. Set to TRUE to follow the calculation succession in case \code{x}, \code{y}, and \code{z} are specified.}

  \item{\dots}{ Arguments to other functions, for instance to \code{\link[stats]{cor.test}} regarding specifications of the test, however only the correlation value is taken from this function. but here you could change from pearson to kendall for instance. }
  
  \item{sub}{ If Case is 1 (see \code{details}) a subset of cases from \code{x} and \code{y} can be defined for correlation. Therefore, \code{sub} has to be a logical matrix with the same dimensions as a matrix derived from \code{x} and \code{y}. }
  
  \item{loop}{ Triggers the method for permutation inside the function \code{mantl}. Shall it be looped (\code{for}-loop, \code{loop = TRUE}) or be done by an \code{\link[base]{apply}} method (\code{loop = FALSE})? Determines speed. For many reasonably huge data sets, the latter will be faster. However, when the datasets get really huge it may run faster on \code{for}-loops. }
  

}

\details{
    \code{pcol} is doing the handling whereas all permutations are done with \code{mantl}. Depending on what is given to \code{y}, \code{z}, and \code{width} and \code{bins} respectively, the following is carried out:
    
    \enumerate{
    \item If \code{x} and \code{y} are given but \code{z = NULL} a simple permuted correlation with \code{mantl} is run. This corresponds to a Mantel test. The two data-objects are correlated with \code{\link[stats]{cor}}, then the rows and corresponding rows in \code{y} are permuted and with \code{\link[stats]{cor}} the correlation is calculated again. This is repeated \code{permutation} times. Finally, the initial correlation value is compared to the permuted values. The number of times, the permuted values exceed the initial value is divided by the number of permutations to obtain a significance value. Thus, with 1000 permutations a minimum p of 0.001 can be tested. A diagnostic plot of the resulting object of class \code{permcor} can be plotted with the corresponding plot function. 
    
    \item If \code{x}, \code{y}, and \code{z} are given, the permuted correlation is done for every stratum or level given by z - this could e.g. be direction or distance classes flagging which plots share a similar distance and therefore fall into the same class. If \code{z} is a distance matrix or \code{dist}-object \code{width} or \code{bins} have to be specified to obtain distance classes. If run with defaults the function finds 5 classes (\code{bins}) of equal distance range. The resulting data-points can be used to plot a correlogram which allows for the analysis of non-stationarity in the relationships between \code{x} and \code{y}.
    
    \item If \code{x} and \code{z} are specified and \code{y = NULL}, the matrix or vector in \code{x} is correlated against the classes given in or derived from \code{z}. This produces the data-points for a multivariate Mantel correlogram in the sense of Oden & Sokal (1986) (see also Legendre & Legendre 1998 for a comprehensive coverage of the subject).
    }
}

\value{
  Returns different objects, depending on given arguments and triggers.
  
In case 1 a \code{permcor}-object with the following items is returned:

\item{call }{ The call to the function. }
\item{method }{ The correlation method as used by \code{\link[stats]{cor.test}}. }
\item{statistic }{ The initial correlation value which is tested against the permuted values. }
\item{signif }{ The significance of the calculation. }
\item{n }{ The number of cases. }
\item{permutations }{ The number of permutations as specified by \code{permutations}. }
\item{perms }{ The result of the permuted runs. It is not printed by default but can be accessed via \code{result\$perms}. The correlation value can be plotted against an histogram of the distribution of the permuted values to visualize the significance with the default plotting method.}
  
In cases 2 and 3 a \code{pclist}-object with the following items is returned. It might be as well worth to set \code{trace} = TRUE to display the progress of the calculation because it can take a while:
  
\item{call }{ The call to the function. }
\item{method }{ The correlation method as used by \code{\link[stats]{cor.test}}. }
\item{gesN }{ The total number of cases. }
\item{strata }{ The number of strata (or levels) for which permutation has been done. }
\item{permutations }{ The number of permutations as specified by \code{permutations}. }
\item{out }{ A \code{data.frame} with 3 columns containing the result for each stratum in the rows: \code{statistic} contains the correlation values for the corresponding stratum, \code{sig} the obtained significance, and \code{nop} the number of cases found and used for permutation on this very level. }

}

\references{ 
    Legendre, P, & Legendre, L. (1998) \emph{Numerical Ecology}. 2nd English Edition. Elsevier.
    
    Oden, N. L. & Sokal, R. R. (1986) Directional Autocorrelation: An Extension of Spatial Correlograms to Two Dimensions. \emph{Systematic Zoology} \bold{35}: 608-617.
    }

\author{ Gerald Jurasinski \email{gerald.jurasinski@uni-rostock.de} }

\note{ Depending on what is done and the size of the matrices it may take a while to calculate. The slowest is case 3. }

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

## compare one resemblance matrix with several others
# we compare bray-curtis against this selection of indices:
indices <- c("soerensen", "jaccard", "ochiai", "mountford", "whittaker", 
"lande", "wilsonshmida", "cocogaston", "magurran", "harrison")
# we use mantl() inside a sapply call
t(sapply(indices, function(x) unlist(mantl(vegdist(abis.spec), sim(abis.spec, method=x))[3:5])))


}

\keyword{ methods }
\keyword{ htest }
