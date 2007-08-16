\encoding{UTF-8}
\name{sim.tmp}
\alias{sim.tmp}
\title{ Calculate binary similarity in time }
\description{
  The function applies one of 56 similarity measures for binary data to calculate compositional similarity of plots between time steps.
}
\usage{
sim.tmp(x, y, method = "soer", normalize = FALSE, adjust = TRUE, ...)
}

\arguments{
  \item{x}{ Vegetation data, either as matrix with rows = plots 
  and columns = species, or as \code{\link[base]{data.frame}} with 
  first three columns representing plots, species and occurence 
  information respectively.  All further columns are dumped before 
  calculation.  Occurence is only considered as binary. If your list 
  or matrix contains abundances or frequencies they are transformed 
  automatically. }
  \item{y}{ Same as \code{x} for time-step two. }
  \item{method}{ One of 42 similarity measures for binary data. The function uses the same indices as \code{\link{sim}}. See details there. Per default \code{soerensen} similarity is calculated. }
  \item{normalize}{ Logical value indicating whether the values for 
  \code{a}, \code{b} and \code{c} which are calculated in the process should be normalized to 100\% (per row, which means per plot comparison). If normalize = TRUE an asymmetric index must be chosen (for details see \code{\link{sim}}). }
  \item{adjust}{ Do not change the default behaviour (TRUE) unless you know what you do. Would spare some calculation time if set to FALSE, when your species data do not need adjustment, which means that in both or all time steps, there are exactly the same species and the same plots. However in most cases it will be more convenient  to rely on the function (see details). }
  \item{\dots}{ Other arguments to \code{sim} }
}

\details{
  If you compare species data among time steps there will be most likely different numbers of species (and often also different numbers of plots for which information is available). The function takes care of this and you can give any species matrices you want. If one plot is the same, it will calculate what changed on this plot. There will be an error message if no plot is shared. The function relies on plot and species names!! As in a database - they must be unique!!
}

\value{
  Returns a named vector with the similarities for each site between time steps for each plot.
}

\references{ See references in \code{\link{sim}} }

\author{ Gerald Jurasinski \email{terhorab@mac.com} }

\seealso{ See Also as \code{\link{sim}} (where you can find a much more elaborate see also section as well). }

\examples{
% still have to be implemented
}

\keyword{ methods }
\keyword{ multivariate }
