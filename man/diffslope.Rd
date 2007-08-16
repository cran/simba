\encoding{UTF-8}
\name{diffslope}
\alias{diffslope}
\alias{diffslope2}
\alias{plot.dsl}
\title{ Calculate the difference in slope of two regression lines }
\description{The function can be used to calculate the difference in slope between two datasets containing each two vectors. Follows an idea of Nekola & White (1999) for calculating the statistical inference of the difference in slope between two regression lines. diffslope2 has the same purpose as diffslope but implementation is without for-loop. The plot method allows easy plotting of the actual difference in slope against the distribution of permuted values.
}

\usage{
diffslope(x1, y1, x2, y2, permutations = 1000, resc.x = FALSE, 
        resc.y = TRUE, trace=FALSE, ...)
        
diffslope2(x1, y1, x2, y2, permutations = 1000, resc.x = FALSE, 
        resc.y = TRUE, ...)
        
\method{plot}{dsl}(x, y, ...)
}

\arguments{
  \item{x1}{ vector containing an independent variable (for instance distance between plots). }
  \item{y1}{ vector containing a variable dependent on \code{x1} (for instance similarity between the plots. must have the same length as \code{x1}. }
  \item{x2}{ vector containing a second independent variable (for instance distance between plots). can be the same as in x1. }
  \item{y2}{ vector containing a variable dependent on \code{x2} (for instance similarity between the plots. must have the same length as \code{x2}. }
  \item{permutations}{ number of permutations }
  \item{resc.x}{ Shall the values of the independent variables be rescaled to a common mean? }
  \item{resc.y}{ Shall the values of the dependent variables be rescaled to a common mean? Defaults to TRUE (Nekola & White 1999). }
  \item{trace}{ Set to true if progress shall be printed with increasing numbers. Defaults to FALSE }
  \item{\dots}{ Arguments to other functions (for instance to \code{\link[stats]{lm}}, which is used to calculate the regression lines). }
  \item{x}{ \code{dsl}-object (given back by \code{diffslope}) which is to be plotted. }
  \item{y}{ Plotting object, usually not necessary }
}

\details{
As the function was initially build to easily calculate the difference in slope between the regression lines of distance decay plots, the independent vectors are meant to contain distance values whereas the dependent vectors should represent similarity values. But you can use it for anything else, as you wish. The vectors belonging together are formed into a {\link[base]{data.frame}}. For each permutation run the rows are interchanged randomly between the two \code{\link[base]{data.frame}}s and the difference in slope calculated thereafter is calculated and collected into a vector. The p-value is then computed as the ratio between the number of cases where the differences in slope exceed the difference in slope of the inital configuration and the number of permutations.

If the difference in slope returns negative, the slope (distance decay) of the second relationship is less pronounced, if it returns positive, the second relationship exhibits a stronger distance decay (slope) than the first. This holds for distance decay relationships. If \code{y} increases with \code{x}, it is vice versa.

As it uses a \code{for} loop, it takes a while to calculate. So get a coffee while it is running, or set \code{trace} to TRUE to avoid being bored ...
}

\value{
  Returns a list giving the function call, the difference in slope, the significance of this difference, and the number of permutations. If you want to change the way \code{\link[stats]{lm}} is computed you must send the arguments to \code{\link[stats]{lm}} via \dots. Per default it is calculated with the default arguments of \code{\link[stats]{lm}}.
}

\references{
    Nekola, J. C. and White, P. S. (1999) The distance decay of 
    similarity in biogeography and ecology. Journal of Biogeography 
    26: 867-878.
    
    Steinitz, O., Heller, J., Tsoar, A., Rotem, D. and Kadmon, R. 
    (2005) Predicting Regional Patterns of Similarity in Species 
    Composition for Conservation Planning. Conservation Biology 19: 
    1978-1988.
    
    Steinitz, O., Heller, J., Tsoar, A., Rotem, D. and Kadmon, R. 
    (2006) Environment, dispersal and patterns of species similarity. 
    Journal of Biogeography 33: 1044-1054.
    }

\author{ Gerald Jurasinski }

\seealso{ \code{\link[stats]{lm}}, \code{\link[base]{sample}} }

\examples{
data(abis)
names(abis.env) ##take a look at the data
pert.dist <- 1-vegdist(abis.env[,19:25]) 
##calculate the similarity (Bray-Curtis) between the plots 
##regarding disturbance variables

soil.dist <- 1-vegdist(abis.env[,c(6,27:28)]) 
##calculate the similarity (Bray-Curtis) between the plots 
##regarding soil parameters

coord.dist <- dist(abis.env[,1:2]) ##calculate geographical distance between plots

##transform all distance matrices into list format:
struc.dist.ls <- liste(pert.dist, entry="BC.struc")
soil.dist.ls <- liste(soil.dist, entry="BC.soil")
coord.dist.ls <- liste(coord.dist, entry="dist")

##create a data.frame containg plot information, geographical 
##distance,similarity of soil parameters, and similarity of 
##structural parameters:

df <- data.frame(coord.dist.ls, soil.dist.ls[,3], struc.dist.ls[,3])
names(df) ##see names

##give better names:
names(df)[4:5] <- c("soil","struc")
attach(df)

##prepare graphics device:
par(mfrow=c(2,1))

##plot and compare distance decay (decrease of similarity with 
##distance):
plot(dist, soil)
plot(dist, struc)
##remove problematic zero entries:
df <- subset(df, struc != 0)

##plot again, this time with regression lines (in red for better 
##visability):
detach(df)
attach(df)
plot(dist, soil)
abline(lm(soil~dist), col="red4")
plot(dist, struc)
abline(lm(struc~dist), col="red4")
##is the slope significantly different?
res <- diffslope(dist, soil, dist, struc)

##go for a coffee, as it takes a while...

}

\keyword{ methods }
\keyword{ univar }
