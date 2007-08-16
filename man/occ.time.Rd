\encoding{UTF-8}
\name{occ.time}
\alias{occ.time}
\alias{occ.tmp}

\title{ Track species occurrence }
\description{
  Calculate the change in occurence of species on plots in general or on specific plots. Allows for the tracking of temporal changes in species abundance throughout an area or the simple quantification and comparison of species occurences between time steps.
}
\usage{
occ.time(x, y, times = NULL, adjust = TRUE, gen.occ = FALSE, 
perc = TRUE, nc.acc = FALSE, ...)

occ.tmp(x, y, adjust=TRUE, gen.occ=FALSE, perc=TRUE, 
nc.acc=FALSE, ...)
}

\arguments{
  \item{x}{ Species data in matrix or database-format representing species occurrence at time step one or throughout a time series. The latter means that you have a table with three columns where the columns represent \code{plots}, \code{species} and \code{occurence} information (in this order!). These are typically exported from a database. When \code{times} are given and data represents more than one time steps it has to be in database format. Conversion is handled automatically - if three columns are in the table it is assumed to be in database format. When there are only three species give the data in database-format. If there are many plots and/or species, internal matrix conversion might be quite slow. }
  \item{y}{ Species data in matrix or database-format representing species occurrence at time step two. Obsolete when \code{times} are given. Otherwise the same as for \code{x} applies. }
  \item{times}{ A vector describing the timesteps which has to be coercible to a factor. If your data comes from a database and contains species records for different time-steps, just export the time information with the species data. If you have single matrices for each time step, you could reshape them to database format via \code{\link{liste}} and concatenate these or calculate each time step alone. }
  \item{adjust}{ Do not change the default behaviour (TRUE) unless you know what you do. Would spare some calculation time if set to FALSE, when your species data do not need adjustment, which means that in both or all time steps, there are exactly the same species and the same plots. However in most cases it will be more convenient  to rely on the function (see details). }
  \item{gen.occ}{ Triggers if general occurrence is regarded or specific occurrence. The latter is default (gen.occ=FALSE) and it means that it is calculated on which exact plots a species is changing. When set to TRUE only general occurence is regarded and it is calculated on how many plots a species occurs more or less then before. See details. }
  \item{perc}{ If output shall be in percentage of species. Defaults to TRUE. }
  \item{nc.acc}{ Per default, species which are not changing on a plot are counted as single species (also when they do not change on more than one plot). This can be changed when setting \code{nc.acc} = TRUE. Then each occurence of species which has not changed is counted. }
  \item{\dots}{ Further arguments to functions. }
}

\details{
  If you compare species data among time steps there will be most likely different numbers of species (and often also different numbers of plots for which information is available). The function takes care of this and you can give any species matrices you want. If one plot is the same, it will calculate what changed on this plot. There will be an error message if no plot is shared. The function relies on plot and species names!! As in a database - they must be unique!!
  
  With the resulting named vector or table it is easy to draw a barplot tracking the changing occurrence of plants. Walter et al. (2005) used such plots to illuminate the changing (and increasing) occurrence of plant species on Alpine summits due to climate change.
  
  However they only considered change in general occurrence. We added the possibility to track changes on the specific plots. If a species is occuring on 5 plots at time one and on 4 plots at time two one can't be shure that this species changed occurrence on only one plot. If it occurrs partly on different plots then before it will contribute to loss and gain respectively in this function when \code{gen.occ} is set to FALSE.
}

\value{
  Returns a named vector or a table (when multiple time steps are evaluated at once) with information on change in species occurence. The names tell on how many plots a certain species has changed. The values tell how many species (or percent of species) exhibit this change. Additionally the number of matching plots and species for each comparison are given back.
  
  Per default nice output is given. However, the table can be accessed for printing whith *\$bac (see Example for Details). A plotting method will be added in the near future.
}

\references{ 
    Walther, G.-R., Beißner, S. & Burga, C. A. 2005. Trends in the upward shift of alpine plants. \emph{Journal of Vegetation Science} \bold{16}: 541-548.
}

\author{ Gerald Jurasinski \email{terhorab@mac.com} \url{http://homepage.mac.com/terhorab/gerald/} }

\seealso{ to calculate similarity based on plant species occurrence between time steps see \code{\link{sim.tmp}} }

\examples{
% Still to be added
}

\keyword{ methods }
\keyword{ multivariate }
