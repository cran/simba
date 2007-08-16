\encoding{UTF-8}
\name{direct}
\alias{direct}
\alias{direct2}
\title{ Obtain Direction Classes from Geographic Coordinates }
\description{
The functions calculates direction classes from geographic coordinates (not lat/lon). All possible connections between these points are established and the direction of each link is calculated. This is followed by a designation of direction-classes.
}

\usage{
direct(coord, listout=FALSE)

direct2(coord, listout=FALSE)
}
\arguments{
  \item{coord}{ A \code{\link[base]{data.frame}} containing coordinates. Should have the same number of points as the data for which the direction-classes are calculated. }
  \item{listout}{ Logical value, indicating whether the result is given back in (\code{\link[base]{data.frame}})-format instead of returning a \code{\link[stats]{dist}}-object. }
}

\value{
  Returns a matrix containing the direction-classes of the connections between the coordinates as a \code{\link[stats]{dist}}-object. If \code{listout} = TRUE, the result is given as a list (\code{\link[base]{data.frame}}).
  
  \code{direct} returns 4 directions (North-South, Northwest-Southeast, West-East, Northeast-Southwest).
  
  \code{direct2} returns 6 directions.
}

\author{ Gerald Jurasinski }

\note{ as with \code{\link[vegan]{mantel}} it takes a while to calculate
}

\seealso{ \code{\link[vegan]{mantel}}, \code{\link[stats]{cor.test}} }
\examples{
data(abis)
dirclass <- direct(abis.env[,1:2])
dirclass
dirclass.ls <- direct(abis.env[,1:2], listout=TRUE)
dirclass.ls
}

\keyword{ methods }
