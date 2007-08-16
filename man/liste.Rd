\name{liste}
\alias{liste}
\title{ Convert dist-objects and matrices to database format }
\description{
  Transposes \code{dist} objects to database list format (where each line represents a similarity value calculated between two plots, so the list has three columns containing information on plot x, plot y and information on similarity/dissimilarity). This might be useful if further database processing is intended. If the given matrix is not of class \code{dist}, the whole matrix is converted. This might be handy if species matrices are to be converted to database format.
}

\usage{
liste(obj, x="NBX", y="NBY", entry=NULL, factorize=TRUE, 
splist=FALSE)
}

\arguments{
  \item{obj}{ A distance object as it is returned from \code{\link[simba]{sim}}, \code{\link[stats]{dist}}, \code{\link[vegan]{vegdist}} or \code{\link[ade4]{dist.binary}} or a similar matrix with class="dist". If the matrix is not of class \code{dist} a \code{data.frame} is expected and all entries are converted. Species matrices are assumed to contain sites (or plots) in rows and species in columns.}
  \item{x}{ How the second column should be named, standard is that it contains \code{"NBX"} the neighbour X. Obsolete when \code{splist} = TRUE. }
  \item{y}{ How the second column should be named, standard is that it contains \code{"NBY"} the neighbour Y. Obsolete when \code{splist} = TRUE. }
  \item{entry}{ How the third column should be named. If there is nothing given, it is named "we" (whatever). Obsolete when \code{splist} = TRUE. }
  \item{factorize}{ If naming (first two) columns should be given as factors. Defaults to TRUE. }
  \item{splist}{ Set to TRUE if a species matrix is transformed to a database list format. Automatically removes entries with zero occurrence of the species' and names columns correspondingly (see Value). }
}

\details{
  Column x represents the column names of the input matrix. So if you want to reshape a species matrix, \code{x} will be the species names and \code{y} will be the plot names. If it is needed vice versa, you have to change column order. For convenience you can set \code{splist} = TRUE and you will get a species list in database format with columns representing the plot, species and occurrence information. Furthermore zero occurrences are already omitted.
}

\value{
  Returns a \code{data.frame} with three columns:
  
  \item{Col1 }{ The column names of the input matrix. If it is a \code{dist}-object only the lower triangle is used. Named \code{plot} when a species matrix is transformed with \code{splist} = TRUE.}
  \item{Col2 }{ The row names of the input matrix. Named \code{spec} when a species matrix is transformed with \code{splist} = TRUE. }
  \item{Col3 }{ The respective matrix entries. Named \code{occ} when a species matrix is transformed with \code{splist} = TRUE. }
}

\author{ Gerald Jurasinski }

\seealso{ \code{\link[stats]{reshape}}, \code{\link[base]{data.frame}}. It resembles functionality of reshape - which is more flexible (but also more complex). }

\examples{

data(abis)
## there are empty species entries:
sum(colSums(abis.spec)==0)
## remove empty species
abis.spec <- abis.spec[,colSums(abis.spec)!=0]
abis.spec.ls <- liste(abis.spec, splist=TRUE)

}

\keyword{ manip }
