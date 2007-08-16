\encoding{UTF-8}
\name{ads.ternaries}
\alias{ads.ternaries}
\docType{data}
\title{ Artificial data-set for studying the mathematical behavior of asymmetric similarity coefficients  }

\usage{
  data(ads.ternaries)
}

\description{
Artificial data-set as utilized in Koleff et al. 2003, and Jurasinski 2007 to study the mathematical behavior of asymmetrical similarity indices. The corresponding values of all indices computable with \code{\link{sim}} are contained. See example! 
}

\format{
  \itemize{
    \item{ads.ternaries}{\code{data.frame} with the three matching components of asymmetric binary similarity measures \code{(a, b, c)} with all possible combinations of these components derived from a virtual data-set with 100 variables (species). These are the first three columns. The preceding columns contain the values of the similarity coefficients computable with \code{\link{sim}} according to the three matching components. This information can be used to study the mathematical behavior of the indices. See example}
}
}

\source{
  Jurasinski, G. (2007) Spatio-Temporal Patterns of Biodiversity and their Drivers - Method Development and a Case Study from Northeastern Morocco. PhD-Thesis, Department of Biogeography, University of Bayreuth
  
  Koleff, P., Gaston, K. J. & Lennon, J. J. (2003) Measuring beta diversity for presence-absence data. \emph{Journal of Animal Ecology} \bold{72}: 367-382.
}

\examples{
data(ads.ternaries)
library(plotrix)
##take any index you want to study, see the help for sim() for available 
##asymmetric indices or the names of the data.frame:
names(ads.ternaries)

##make a tmp from the index you want to study (we perform a transformation 
##to obtain values between 0 and 1). if you want another index, 
##just change the name in the next line:
tmp <- ads.ternaries$mountford
tmp <- (tmp-min(tmp))/max(tmp)
triax.plot(ads.ternaries[,c(2,3,1)], main="mountford", 
col.symbols=grey(seq(0.1,1,0.1))[floor((tmp*100)/5)+1], pch=16)

##don't wonder: mountford is strange, just try another one:
##this time with rainbow-colors
tmp <- ads.ternaries$soerensen
tmp <- (tmp-min(tmp))/max(tmp)
triax.plot(ads.ternaries[,c(2,3,1)], main="sørensen", 
col.symbols=rainbow(10)[floor((tmp*100)/10)+1], pch=16)

##and an interesting shape: routledge in greyscale...
tmp <- ads.ternaries$routledge
tmp <- (tmp-min(tmp))/max(tmp)
triax.plot(ads.ternaries[,c(2,3,1)], main="routledge", 
col.symbols=grey(seq(0.1,1,0.1))[floor((tmp*100)/5)+1], pch=16)

}

\keyword{datasets}
