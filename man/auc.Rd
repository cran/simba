\encoding{UTF-8}
\name{auc}
\alias{auc}
\title{
Calculate the area under a line(curve).
}
\description{
Calculates the \strong{a}rea \strong{u}nder a \strong{c}urve (integral) by decomposing the curve (line) into rectangles and adding their area.
}

\usage{
auc(x, y, below.zero = TRUE)
}

\arguments{
  \item{x }{ Numerical vector giving the x cordinates of the points of the line (curve). }
  \item{y }{ Numerical vector giving the x cordinates of the points of the line (curve). One can calculate the integral of a fitted line through giving a vector to \code{x} that spans \code{xlim} with small intervalls and predicting the y coordinates with \code{\link[stats]{predict}} and that \code{x}-vector as \code{newdata}. See example. }
  \item{below.zero }{ How to handle y-data below zero? When below zero data represent proper data you'll want to substract the areas below the zero line from the area above the zero line to integrate the area under the curve. When below zero makes no sense for your question, you are able to set this to \code{FALSE}. Then, all 'negative' areas are just set to 0. }
}

\value{
Returns a numeric value that expresses the area under the curve. The unit depends from the input.
}

\author{
Gerald Jurasinski, \email{gerald.jurasinski@uni-rostock.de}
}

\examples{
## Construct a data set (Imagine 2-hourly ghg emission data
## (methane) measured during a day).
## The emission vector (data in mg CH4 / m2*h) as a time series.
ghg <- ts(c(12.3, 14.7, 17.3, 13.2, 8.5, 7.7, 6.4, 3.2, 19.8, 
22.3, 24.7, 15.6, 17.4), start=0, end=24, frequency=0.5)
## Have a look at the emission development.
plot(ghg)
## Calculate what has been emitted that day
## Assuming that emissions develop linearly between
## measurements
auc(time(ghg), ghg)

## The effect of below.zero:
## Shift data, so that we have negative emissions (immissions)
ghg <- ghg-10
## See the difference
plot(ghg)
abline(h=0)
## With below.zero=TRUE the negative emissions are substracted
## from the positive emissions
auc(time(ghg), ghg)
## With below.zero=TRUE the negative emissions are set to 0
## and only the emissions >= 0 are counted.
auc(time(ghg), ghg, below.zero=FALSE)

}

\keyword{ math }
\keyword{ models }
\keyword{ ts }


