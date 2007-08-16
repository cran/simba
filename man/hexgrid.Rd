\name{hexgrid}
\alias{hexgrid}
\title{ Produces the nodes of an equidistant grid. }
\description{
  Given the coordinates of a starting point (left upper point of the grid), the function produces the nodes of an equidistant grid. Extent and distance between plots can be specified.
}
\usage{
hexgrid(x, y, r = 100, nro = 10, nco = 20)
}

\arguments{
  \item{x}{ \code{x}-value of the starting point. Defaults to 0. }
  \item{y}{ \code{y}-value of the starting point. Defaults to 0.}
  \item{r}{ Distance between nodes. Defaults to 100. }
  \item{nro}{ Number of rows in the grid. Defaults to 10}
  \item{nco}{ Number of columns in the grid. Defaults to 20. They are doubled (see the grid) compared to the rows. That's why 2*\code{nro} produces a quadratic grid. }
}

\details{
  If the overall shape of the grid is not square, the user has to delete by hand the superfluous units. Might get more flexible in future versions.
}

\value{ Returns a data.frame giving informations on the produced point/unit/plot locations with the following columns:

  \item{ROW }{ Number of row in the grid to which the point/unit/plot belongs. }
  \item{COL }{ Number of column in the grid to which the point/unit/plot belongs. }
  \item{X }{ x-coordinate of the point. }
  \item{Y }{ y-coordinate of the point. }
}

\references{ \url{http://homepage.mac.com/terhorab/gerald/downloads/whyhexaagons.pdf} }

\author{ Gerald Jurasinski }

\examples{
## produces a grid with r=400:    
test.grd <- hexgrid(456000, 7356700, r=400)
## for plotting the following is recommended as it preserves 
## real positions:
library(geoR)
points.geodata(coords=test.grd[,3:4], data=rnorm(nrow(test.grd)))
}

\keyword{ methods }
\keyword{ datagen }
