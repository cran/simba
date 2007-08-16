\encoding{UTF-8}
\name{diffmean}
\alias{diffmean}
\alias{plot.dmn}
\title{ Calculate the difference in Mean between two vectors }
\description{The function can be used to calculate the difference in mean between two vectors. Statistical inference is obtained through permutation. F-ratio is also calculated. For data which is not normally distributed or lacks independence. The plotting method plots the actual values of the difference in mean and F against an histogram of the results of the permuted runs.
}

\usage{
diffmean(x, y, permutations = 1000)
\method{plot}{dmn}(x, y, which=3, two=2, ...)
}

\arguments{
  \item{x}{ Numeric vector. For the plotting method the \code{dmn}-object which should be printed (results from a \code{diffmean} operation). Plotting object in the plotting method. }
  \item{y}{ Numeric vector. Plotting object in the plotting method, optional when \code{x} has appropriate structure}
  \item{permutations}{ Number of permutations. }
  \item{which}{ which histogram should be plotted? 1 triggers the histogram for difference in mean, 2 the one for F. It defaults to 3: both histograms are plotted. If it is changed from default, the next argument (\code{two}) is automatically set to 1! }
  \item{two}{ Should the histograms be printed on a divided display? And how? Can only be set if \code{which} is set to 3. Defaults to 2, which means that the display is divided in two halfs and the histogram-plots are plotted side by side. 3 causes histograms to be plotted one on top of the other. If \code{two} = 1, the display is NOT automatically divided. Might be useful if more than one \code{dmn}-objekt is to be plotted on one display. Otherwise the function overrides the actual display settings.}
  \item{\dots}{ Further arguments to the plotting method. }
}

\details{
The two vectors do not need to share the same length but they should not be too different. Otherwise the function might give spurious results.
}

\value{
  Returns a list giving the function call, the difference in Mean, the mean of vector x and y, the mean of means, the F-value, the significance of the difference in Mean and the significance of F, as well as the number of permutations. The results of the permutation runs can be retrieved with \code{result\$bootsM} (for the difference in mean) and \code{result\$bootF} (for the F-values). There is a plot method for easily illustrating the test. The difference is plotted against an histogram displaying the distribution of the permuted values.
}

\author{ Gerald Jurasinski }

\seealso{ \code{\link[simba]{diffslope}}, \code{\link[simba]{diffmich}} }

\examples{

data(abis)

## create subsetting vector describing the belonging to different
## vegetationtypes
tcs.sub <- rep(0, 61)
tcs.sub[abis.env[,29]==1] <- 1
tcs.sub[abis.env[,30]==1] <- 2
tcs.sub[abis.env[,31]==1] <- 3

## check distribution
summary(as.factor(tcs.sub))

## compare vegetation types "shrubby vegetation" (shrub=2) and
## "protected by snowcover" (protect=3) regarding difference in 
## similarities
abis2.soer <- sim(abis.spec[tcs.sub==2,])
abis3.soer <- sim(abis.spec[tcs.sub==3,])
abis.23cmp <- diffmean(abis2.soer, abis3.soer)

}

\keyword{ methods }
\keyword{ univar }
