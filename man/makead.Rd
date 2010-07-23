\encoding{UTF-8}
\name{makead}
\alias{makead}
\alias{ads}
\alias{ads.hot}
\alias{ads.fbg}
\title{ Create artificial data set (species matrix). }
\description{ 
  The functions allow for the automated creation of artificial data (species matrix). The user can choose between random organization or a gradient. The gradient can be defined via a gradient vector which allows for fine tuning of the gradient. \code{\link[simba]{ads}} has a different implementation and produces better results for gradients.
}

\usage{
makead(nspec, nplots, avSR = NULL, anc = NULL, grad.v = NULL, 
cf = 0.2, puq = 0.01)

ads(nspec, nplots, avSR = NULL, anc = NULL, grad.v = NULL, 
reord = TRUE, cf = 0.2, puq = 0.01)

ads.hot(nspec, nplots, avSR = NULL, anc = NULL, grad.v = NULL, 
frac=0.5, reord=TRUE, cf=0.2, puq=0.01)

ads.fbg(nspec, nplots, grad.v, n.iter = 100, method = "ads", \dots)
}

\arguments{
  \item{nspec}{ Numbers of species you want to be in the data-set. Meaningless if \code{anc} != NULL. }
  \item{nplots}{ Numbers of plots you want to be in the data-set. Meaningless if \code{anc} != NULL. }
  \item{avSR}{ Average species richness. If \code{anc} is given, it is calculated from the data when the default is not changed. If \code{avSR} != NULL, the given value is taken instead. In the actual version not implemented in \code{ads}.}
  \item{anc}{ If a model species matrix is available (either a real data-set, or another artificial data-set) on which creation should be based, give it here. Rows must be plots and columns be species. The first three parameters are then obtained from this set. However average species richness (\code{avSR}) can still be given by the user. }
  \item{grad.v}{ A numeric vector describing the gradient, or - in case of \code{ads.hot} - the hotspot. Must have the same length as \code{nplots} (or \code{nrow(anc)} respectively). See details. }
  \item{cf}{ Determines the probability of the species to occur on the plots. In other words, it changes the shape of the species accumulation curve. Set to NULL if no natural species accumulation should be applied (may sometimes increase the visibility of the gradient)}
  \item{puq}{ Percentage of ubiquitous species. Set to NULL if the produced gradients seem to be unclear or if you don't want ubiquitous species to be in the data-set. Only used if a gradient vector is given (which is then not applied to the given percentage of species). }
  \item{reord}{ Triggers reordering of the columns in the produced gradient matrix (see details). May considerably change the resulting matrix. Defaults to TRUE.}
  \item{frac}{ Numeric between 0 and 1 giving the percentage of species which should occur on the hotspot-gradient only (see details). }
  \item{n.iter}{ Number of iterations when \code{ads.fbg} is used for finding the species matrix representing best the prescribed gradient (see details). }
  \item{method}{ Which method of \code{makead}, \code{ads}, \code{ads.hot} should be used? }
  \item{\dots}{ Further arguments to the function specified in \code{method} }
}

\details{
  There are three different implementations to create an artificial species matrix and a fourth function \code{ads.fbg} that allows to use either of the three possibilities to find a "best" gradient. 
  
  \code{makead} first applies the \bold{n}atural \bold{s}pecies \bold{a}ccumulation curve, the gradient for each species is represented by a vector containing numerics between 0 and 1. Both matrices are added so that values between 0 and 2 result. Through an iteration procedure a breakvalue is defined above which all entries are converted to 1. Values below are converted to 0 resulting in a presence/absence matrix. However the random element seems to be too strong to get evident gradient representations.
  
  Therefore \code{ads} is implemented. It works different. First, a gradient is applied. As with \code{makead} the gradient is always applied in two directions so that half of the species are more likely to occur on plots on one side of the gradient, whereas the others are more likely to occur on the other side of the gradient. Subsequently, species occurrence for all species will oscillate around \code{nplots}/2. 
  
  If \code{puq} is specified the given percentage of species is divided from the whole matrix before the gradient is applied. With the parameter \code{cf} a vector is produced representing quasi-natural occurrence of the species on the plots: Most species are rare and few species are very common. This is described by a power function \eqn{y = \frac{1}{x^{cf}}}{y = 1/x^cf} with \code{x} starting at 2 and gives a vector of length \code{nspec} representing the number of times each species is occuring. 
  
  These numbers are applied to the gradient matrix and from the species occurrences only as many as specified by the respective number are randomly sampled. In cases were the occurrence number given by the vector exceeds the occurrences resulting from the gradient matrix, the species in the gradient matrix is replaced by a new one for which occurrence is not following the gradient and represents the number of occurrences given by the vector. The idea behind this is, that also in nature a species occuring on more than about half of the plots will likely be independent from a specific gradient.
  
  In both cases (\code{makead} and \code{ads}) a totally random species matrix (under consideration of natural species occurrence, see \code{cf}) is obtained by randomly shuffling these occurrences on the columns (species) of the "natural species occurrence" matrix.
  
  Contrarily to the other two functions, \code{ads.hot} allows for the creation of an artficial data-set including a hotspot of species richness and composition. In this case, \code{frac} can be used to specify which proportion of the total number of species should only occur on the hotspot gradient. All other species occur randomly on the plots. However, with the hotspot-gradient (\code{grad.v}) you can influence the explicitness of the hotspot. 
  
  The function \code{ads.fbg} allows for finding the best gradient representation with one of the above functions. A gradient is considered to be represented best, when the correlation between the first axis scores of a DCA (which is calculated with \code{\link{decorana}} of package \code{vegan} and the gradient positions as described by the gradient vector \code{grad.v} are maximized. \code{ads.fbg} just runs the specified \code{makead} function \code{n.iter} times and gives out the best result matrix and the \code{r2.adjust} value that has been obtained.  
}

\value{
  The three functions for creating an artificial species matrix each return a presence/absence species matrix with rows representing plots/sampling units and columns representing species. \code{ads.fbg} returns a list with 
  \item{mat }{The species matrix as for the three artificial data set functions.}
  \item{r2.adj }{The adjusted r2 value for the regression of the first axis DCA scores of the resulting species matrix against the position on the prescribed gradient as described by the gradient vector \code{grad.v}.}   
}

\author{ Gerald Jurasinski, Vroni Retzer }

\examples{
## create a random data-set with 200 species on 60 plots
artda <- makead(200, 60, avSR=25)

## create a gradient running from North to South (therefore you 
## need a spatially explicit model of your data which is obtained 
## with hexgrid())
coor <- hexgrid(0, 4000, 200)
coor <- coor[order(coor$ROW),] #causes coordinates to be in order.
## then the gradient vektor can easily be generated from the ROW names
gradvek <- as.numeric(coor$ROW)
## check how many plots your array has
nrow(coor)
## create a data-set with 200 species
artda <- ads(200, 100, grad.v=gradvek)
## see the species frequency distribution curve
plot(sort(colSums(artda)))

}

\keyword{ datagen }
