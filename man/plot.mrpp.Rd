\encoding{UTF-8}
\name{plot.mrpp}
\alias{plot.mrpp}
\title{ plot an mrpp-object }
\description{There is no mrpp plotting function in the vegan package. For convenience it is provided here.
}

\usage{
plot.mrpp(x, y, ...)
}

\arguments{
  \item{x}{ a mrpp-object as computed with \code{\link[vegan]{mrpp}} }
  \item{y}{ Optional plotting object, not needed here. }
  \item{\dots}{ Arguments to the plotting function }
}

\value{
  Returns a histogram with the distribution of the permutation values plotted against delta: For explanations see \code{\link[vegan]{mrpp}}
}

\author{ Gerald Jurasinski }

\seealso{ \code{\link[vegan]{mrpp}}, \code{\link[vegan]{anosim}} }

\keyword{ methods }