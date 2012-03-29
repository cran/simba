\encoding{UTF-8}
\name{mad}
\alias{mad}
\title{
Make binary dummy variables out of a factor vector
}
\description{
Make binary dummy variables out of a factor vector
}

\usage{
mad(x, red = TRUE)
}

\arguments{
  \item{x}{
A factor vector. Or a vector that is convertible to a factor.
}
  \item{red}{
Logical. Shall redundancy be retained? When a factor variable is broken down to binary dummy variables it is typically not wanted to transfer all levels to 0/1 vectors but to skip the last level because the information is already contained in the sum of the other variables.
}
}

\value{
Returns a data.frame with binary data (0/1) with as many (n) rows as x has levels (with \code{red}=TRUE) or with n-1 levels when \code{red}=FALSE. 
}

\references{
Legendre & Legendre 1998. Numerical ecology. Elsevier
}

\author{
Gerald Jurasinski, \email{gerald.jurasinski@uni-rostock.de}
}

\seealso{
\code{\link{sim}}, \code{\link{mps}}
}

\examples{
## load bernina data
data(bernina)
## have a look at summit$year
summits$year
## make binary dummies from that
mad(summits$year)
}

\keyword{ datagen }
