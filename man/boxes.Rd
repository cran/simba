\encoding{UTF-8}
\name{boxes}
\alias{boxes}
\title{ An adaption of boxplot.n. }
\description{
  Uses \code{\link[graphics]{boxplot}} to produce a boxplot, which is then annotated with the number of obbservations in each group. Does allow for more flexibility compared to \code{\link[gplots]{boxplot.n}}. Default size of the text is bigger and per default the numbers are not plotted directly on the x-axis and their distance from the axis can be changed.
}
\usage{
boxes(..., top = FALSE, shrink = 1, textcolor = NULL, yadj = NULL)
}

\arguments{
  \item{\dots}{ Parameters passed to \code{boxplot}. }
  \item{top}{ Should the numbers of observations be printed below or above the boxes? Defaults to below (top = FALSE). }
  \item{shrink}{ Parameter to scale the size of the numbers of observations. Above 1 increases size, below 1 until 0 decreases size. }
  \item{textcolor}{ Color of the text. Defaults to NULL which than uses the actual plotting colour of the graphics device. }
  \item{yadj}{ Can be used to adjust the vertical plotting positions of the numbers of observations. Defaults to NULL - no adjustment. }
}

\author{ Gerald Jurasinski }

\seealso{ \code{\link[graphics]{boxplot}}, \code{\link[graphics]{plot}}, \code{\link[gplots]{boxplot.n}} }

\examples{

data(abis)

## see environmental data (see documentation on data for details)
abis.env

## calculate the difference in similarities for the three major  
## vegetation types
## therefore created a vector from the data expressing belonging
## to the vegetation types:
tcs.sub <- rep(0, 61)
tcs.sub[abis.env[,29]==1] <- 1
tcs.sub[abis.env[,30]==1] <- 2
tcs.sub[abis.env[,31]==1] <- 3

## calculate similarity (Jaccard) between all pairs of plots
abis.jacc <- sim(abis.spec, method="jaccard")

## make boxplots regarding the similarities for each vegetation
## type, including the number of pairs contained in each box.
boxes(as.matrix(abis.jacc)~tcs.sub, shrink=1.2)

## prettier
boxes(as.matrix(abis.jacc)~tcs.sub, notch=TRUE, col="grey50", 
shrink=1.2, ylim=c(-0.1, 0.9))

}

\keyword{ hplot }
