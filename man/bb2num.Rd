\encoding{UTF-8}
\name{bb2num}
\alias{bb2num}
\title{
Transform Braun-Blanquet scale data to percentage cover values and vice versa
}
\description{
The function allows for an easy transformation of Braun-Blanquet (or any other phytosciological scale) data to numeric values for numerical analysis or vice versa. Be aware that this transformation is not losless regarding the information content.
}

\usage{
bb2num(dat, from = c("r", "+", "1", "2", "3", "4", "5"), 
to = c(0.1, 1, 5, 15, 37.5, 62.5, 87.5))
}

\arguments{
  \item{dat }{ A species matrix. }
  \item{from }{ Definition of the imput scale. Per default the transformation is done from the classic Braun-Blanquet scale to cover percentages. For transformation from numerical values to a phytosociological scale see details and example. }
  \item{to }{ Definition of the output scale. The default are the recommended class centers when transforming BB to percentage cover values. Can be changed according to your needs. See details. }
}

\details{
When transforming from a phytosociological scale to Braun-Blanquet\code{from} and \code{to} have to be the same length. In case of transformation from numerical cover values to a phytosociological scale the function expects a numerical vector in \code{from} that, for each class, gives the lower and upper limit. Thus, \code{length(from) == 2*length(to)} in this case. 
}

\value{
Returns a species matrix in the specified format.
}

\author{
Gerald Jurasinski \email{gerald.jurasinski@uni-rostock.de}
}

\examples{
## Create a species that occurs on 7 plots
## with all the different possibilities of
## the BB scale
spec <- c("r", "+", "0", "1", "2", "3", "4", "5")

## Create a highly artificial species matrix.
## All species are the same for simplicity 
dat.bb <- data.frame(spec, spec, spec, spec, spec, spec)

## Transform from BB scale to percentage values
dat.proc <- bb2num(dat.bb)

## When transforming back the class definitions are a little
## more complicated. Just give the lower and upper limits for
## each class in the from vector class for class.
from <- c(0, 0.1, 0.1, 2, 2, 5, 5, 25, 25, 50, 50, 75, 75, 100)
to <- c("r", "+", "1", "2", "3", "4", "5")
bb2num(dat.proc, from=from, to=to)

}

\keyword{ manip }
\keyword{ utilities }
