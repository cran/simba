\encoding{UTF-8}
\name{trudi}
\alias{trudi}
\title{
Calculate true diversity of any order
}
\description{
Calculates the gamma, alpha and beta components of true species diversity of a species x sites matrix.
}

\usage{
trudi(mat, q = 0, weights = TRUE, base = exp(1))
}

\arguments{
  \item{mat}{
Vegetation data. Sites x species matrix. May be binary or quantitative data.
}
  \item{q}{
The order of the diversity. Defaults to 0 which means that rare species have more emphasis. With q = 1 all species contribute exactly according to their relative abundance. For details see references.
}
  \item{weights}{
Logical. Shall the contribution of the sites to the total abundances in the dataset be acknowledged? Per default it is done so (see Tuomisto 2010a,b for details)
}
  \item{base}{
Internally the logarithm of a term is calculated (like in Shannon index) and here it is possible to change the base of that logarithm.
}
}

\details{
In recent years there has been an intense debate about the calculation of species diversity from vegetation data. Jost (2006) brought the fairly old concept of Hill's (1973) effective number of species back into the debate and elucidated the important difference between entropy and diversity. Combined with the scales concept of Whittaker (alpha, beta, gamma; Jurasinski & Koch 2011) the apologists of strict diversity definition coined the term "true diversity". 

The function calculates true diversity components for alpha, beta, and gamma.   
}

\value{
Returns a named vector with the values for gamma, beta, and alpha diversity.
}

\references{
	Hill, M.O. (1973) Diversity and evenness: a unifying notation and itsconsequences. \emph{Ecology} \bold{54}:427–432

	Jost, L. (2006) Entropy and diversity. \emph{Oikos} \bold{113}:363–375	Jost, L. (2007) Partitioning diversity into independent alpha and betacomponents. \emph{Ecology} \bold{88}:2427–2439	
	Jost, L. (2009) Mismeasuring biological diversity: response toHoffmann and Hoffmann (2008). \emph{Ecological Economics} \bold{68}:925–992	
	Jost, L. (2010) Independence of alpha and beta diversities. \emph{Ecology}\bold{91}:1969–1974

	Jurasinski, G.; Koch, M. (2011) Commentary: do we have a consistent 
terminology for species diversity? We are on the way- \emph{Oecologia} 
DOI 10.1007/s00442-011-2126-6
	
	Routledge, R.D. (1979) Diversity indices: which ones are admissible? 
\emph{Journal of Theoretical Biology} \bold{76}:503–515	
	Tuomisto, H. (2010a) A diversity of beta diversities: straightening up 
a concept gone awry. Part 1. Defining beta diversity as a function of alpha 
and gamma diversity. \emph{Ecography} \bold{33}:2–22	
	Tuomisto, H. (2010b) A diversity of beta diversities: straightening up 
a concept gone awry. Part 2. Quantifying beta diversity and related phenomena. 
\emph{Ecography} \bold{33}:23–45
	
	Tuomisto, H. (2010c) A consistent terminology for species diversity? Yes, 
it does exist. \emph{Oecologia} \bold{164}:853–860
	
	Tuomisto, H. (2011) Commentary: do we have a consistent terminology for 
species diversity? Yes, if we choose to use it \emph{Oecologia}	
	Whittaker, R.H. (1960) Vegetation of the Siskiyou Mountains, Oregon and California. \emph{Ecological Monographs} \bold{30}:279–338

	Wolda, H. (1981) Similarity indices, sample size and diversity. \emph{Oecologia} \bold{50}: 296-302.
}

\author{ Gerald Jurasinski \email{gerald.jurasinski@uni-rostock.de} }

\seealso{
\code{\link[vegan]{diversity}} for diversity indices.
}

\examples{
# load data
data(bernina)
# run trudi with defaults
trudi(veg)
# testing another q
trudi(veg, 2)

## do something more complex: calculating the diversity components
## for the three time steps in the Bernina data
# a vector of q's
qs <- c(0:10)
# calculating diversity components per q per sampling year
div <- by(veg.lst, years, function(x) data.frame(t(sapply(c(0:10), 
function(y) trudi(mama(x), y)))))
# plot the results
par(mfrow=c(1,3), cex=1.1, las=1)
for(i in c(1:3)){
	plot(gamma ~ qs, data=div[[i]], ylim=c(0,80), type="l", 
	main=names(div)[i])
	lines(alpha ~ qs, data=div[[i]], lty=2)
	lines(beta*10 ~ qs, data=div[[i]], col="red4")
}

}

\keyword{ methods }
\keyword{ multivariate }
