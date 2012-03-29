\encoding{UTF-8}
\name{mama}
\alias{mama}
\title{A (convenience) wrapper function to make matrix from a data.frame}
\description{The function \code{mama} uses \code{\link[stats]{reshape}} 
	to transpose species data given in database list format (where each line 
	represents a species in a plot, so the list has three columns containing 
	information on plot, species and information on occurence) into a plot 
	species matrix (where rows represent plots and columns represent species) 
	for further use with other functions on vegetational data.
}

\usage{
mama(dat, spl = TRUE)
}
\arguments{
  \item{dat}{Species data in list format. The columns have to represent plot, species, occurence information (presence/absence or abundances). Column names may differ but they must be in that order! The last column can be missing when these are presence/absence data}
  \item{spl}{Logical. If TRUE (default) \code{mama} assumes that the input is species data and formats the output accordingly}
}

\details{
  You could reach the same result with \code{\link[stats]{reshape}}. I was just always quite confused with this. That's why i decided to do this little wrapper for convenience. It needs quite a while to run though, but this is due to \code{\link[stats]{reshape}}.
}

\value{
  Returns a \code{\link[base]{data.frame}} which contains the presence/absence or abundance data of the species list. Rows represent plots, columns represent species. If you want to have it vice versa you have to use the function on a list with columns \code{species}, \code{plots}, \code{occurrence information} (in that order).
}

\author{Gerald Jurasinski}

\seealso{\code{\link[stats]{reshape}}, \code{\link[base]{data.frame}}}
\examples{
    
data(abis)
abis.spcls <- liste(abis.spec, splist=TRUE)
## see the list, it like what you get from a database
## and return to matrix-format:
abis.test <- mama(abis.spcls)

}

\keyword{ manip }
