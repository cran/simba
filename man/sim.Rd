\encoding{UTF-8}
\name{sim}
\alias{sim}
\title{Calculate similarities for binary vegetation data}
\description{
	One of 56 (dis)similarity measures for binary data can be set to calculate (dis)similarities.  The vegetational data can be in either database (list) or matrix format. Same holds for the output. Simultaneous calculation of geographical distances between plots and the virtual position of the calculated similarity values between the parental units can be achieved if a \code{data.frame} with coordinates is given.
}

\usage{ 
sim(x, coord=NULL, method = "soer", dn=NULL, normalize = FALSE, 
listin = FALSE, listout = FALSE, ...) 
}

\arguments{
  \item{x}{Vegetation data, either as matrix with rows = plots 
  and columns = species (similarities are calculated between rows!), or as \code{\link[base]{data.frame}} with 
  first three columns representing plots, species and occurence 
  information respectively.  All further columns are dumped before 
  calculation.  Occurence is only considered as binary. If your list 
  or matrix contains abundances or frequencies they are transformed 
  automatically.}
  \item{coord}{ A \code{data.frame} with two columns containing the coordinate values of the sampling units. If given, it triggers the simultaneous calculation of the geographical distances between the sampling units, the coordinates of virtual centre-points between all possible pairs of plots, and the geographical distances in either x- or y-direction. If \code{coord} is given, output is always in \code{database} format (no matrix). }
  \item{method}{ Binary Similarity index (see \code{Details} for references and formulae), partial match to \code{"soerensen"}, \code{"jaccard"}, \code{"ochiai"}, \code{"mountford"}, \code{"whittaker"}, \code{"lande"}, \code{"wilsonshmida"}, \code{"cocogaston"}, \code{"magurran"},    \code{"harrison"}, \code{"cody"}, \code{"williams"},    \code{"williams2"}, \code{"harte"}, \code{"simpson"},    \code{"lennon"},   \code{"weiher"}, \code{"ruggiero"},    \code{"lennon2"}, \code{"rout1ledge"}, \code{"rout2ledge"},    \code{"rout3ledge"}, \code{"sokal1"},    \code{"dice"},    \code{"kulcz1insky"}, \code{"kulcz2insky"}, \code{"mcconnagh"},    \code{"manhattan"}, \code{"simplematching"}, \code{"margaleff"},    \code{"pearson"}, \code{"roger"}, \code{"baroni"},    \code{"dennis"}, \code{"fossum"}, \code{"gower"}, \code{"legendre"},    \code{"sokal2"}, \code{"sokal3"}, \code{"sokal4"}, \code{"stiles"}, \code{"yule"}, \code{"michael"}, \code{"hamann"}, \code{"forbes"}, \code{"chisquare"}, \code{"peirce"}, \code{"eyraud"}, \code{"simpson2"}, \code{"legendre2"}, \code{"fager"}, \code{"maarel"}, \code{"lamont"}, \code{"johnson"}, \code{"sorgenfrei"}, \code{"johnson2"}. See details. 
  }
  \item{dn}{ Neighbor definition. A geographic distance represented by a numeric or a two value vector defining a ring around each plot. Only takes effect when \code{coord} != NULL. If specified, the output does only contain similarities between neighboring plots. A plot is a neighbour to any given plot if it is within the range of the neighbor definition. See details.}
  \item{normalize}{ Logical value indicating whether the values for 
  \code{a}, \code{b} and \code{c} which are calculated in the process should be normalized to 100\% (per row, which means per plot comparison). If normalize = TRUE an asymmetric index must be chosen (see details).}
  \item{listin}{ if \code{x} is given in database (list) format this must be set to TRUE (there is no automatic detection of the format)}
  \item{listout}{ If output is wanted in database format rather than as a \code{\link[stats]{dist}}-object set this to TRUE. Output is automatically given in database-format, when \code{coord} is specified. }
  \item{\dots}{ Arguments to other functions }
}

\details{
All binary similarity indices are based on the variables \code{a}, \code{b} and \code{c} (or can be expressed as such).  Some of them also use \code{d}.  Where \code{a} is the number of species shared by two compared plots, \code{b} is the number of species found only in one of the compared plots, and \code{c} is the number of species only found in the other of the compared plots.  \code{d} refers to species which are absent from both the compared plots but present in the whole dataset.  Indices incorporating \code{d} are discussed critically by Legendre & Legendre (1998) and elsewhere.  They are called symmetric and expose a "double zero" problem as they take species into account which are absent from both compared units.  Absence of species from a sampling site might be due to various factors, it does not necessarily reflect differences in the environment.  Hence, it is preferable to avoid drawing ecological conclusions from the absence of species at two sites (Legendre & Legendre 1998). The indices presented here come from various sources as indicated. Comparative reviews can be found in e.g. Huhta (1979), Wolda (1981), Janson & Vegelius (1981), Shi (1993), Koleff et al. (2003), Albatineh (2006)

The indices considerably differ in their behaviour. For classification purposes and in ecology, Jaccard and Sørensen have been found to give robust and meaningful results (e.g. Janson & Vegelius 1981, Shi 1993). For other purposes other indices might be better suited. However, you are invited to use (at least with the asymmetric indices) ternary plots as suggested by Koleff et al. 2003. The matching components \code{a, b,} and \code{c} can be displayed in a ternary.plot to evaluate the position of the plots in similarity space. When output is in database-format, the matching components are always given and \code{\link[plotrix]{triax.plot}} can be used to plot them into a triangle-plot. Koleff et al. (2003) used an artificial set of matching components including all possibilities of values that \code{a, b,} and \code{c} can take from 0 to 100 to display the mathematical behavior of indices. An artificial data-set with this properties - together with the values for the asymmetric indices included here - is part of this package (\code{\link{ads.ternaries}}) and can be used to study the behavior of the indices prior to analysis. See details and examples there. 

If \code{coord} is given, the geographic distances between plots/sampling units are calculated automatically, which may be of value when the display or further analyses of distance decay (sensu Tobler 1970, Nekola & White 1999) is in focus. For convenience the \code{dn}-trigger can be used to tell the function to only return similarities calculated between neighboring plots. Similarities between neighboring plots in an equidistant array are not subjected to the problem of auto-correlation because all plots share the same distance (Jurasinski & Beierkuhnlein 2006). Therefore, any variation occurring in the data are most likely caused by environmental differences alone.

In the following formulae...

\code{a} = number of shared species

\code{b} = number of species only found on one of the compared units

\code{c} = number of species only found on the other of the compared units

\code{d} = number of species not found on the compared plots but in the dataset

\code{N} = \eqn{a+b+c+d}{a+b+c+d}

with \eqn{(n_1 \leq n_2)}{(n1 <= n2)}...

\eqn{n_1}{n1} = number of species of the plot with fewer species \eqn{(a+b)}{(a+b)} or \eqn{(a+c)}{(a+c)}

\eqn{n_2}{n2} = number of species of the plot with more species \eqn{(a+b)}{(a+b)} or \eqn{(a+c)}{(a+c)}

Computable asymmetric indices:
 \tabular{lll}{
    \code{soerensen}
    \tab \eqn{sim = \frac{2a}{2a + b + c}}{sim = (2*a)/((2*a) + b + c)}
    \tab Soerensen (1948)
    \cr
    \code{jaccard}
    \tab \eqn{sim = \frac{a}{a + b + c}}{sim = a / (2a + b + c)}
    \tab Jaccard (1912)
    \cr
    \code{ochiai}
    \tab \eqn{sim = \frac{a}{\sqrt{(a+b)(a+c)}}}{sim = a / sqrt((a + b) * (a + c))}
    \tab Ochiai (1957), Shi (1993)
    \cr
    \code{mountford}
    \tab \eqn{sim = \frac{2a}{(a(b+c)+2bc)}}{sim = (2 * a) / (a * (b + c) + (2 * b * c))}
    \tab Mountford (1962), Shi (1993)
    \cr
    \code{whittaker}
    \tab \eqn{sim = \frac{a+b+c}{\frac{2a+b+c}{2}}-1}{sim = ((a + b + c) / ((2 * a + b + c)/2))-1}
    \tab Whittaker (1960), Magurran (1988)
    \cr
    \code{lande}
    \tab \eqn{sim = \frac{b+c}{2}}{sim = (b + c)/2}
    \tab Lande (1996)
    \cr
    \code{wilsonshmida }
    \tab \eqn{sim = \frac{b+c}{2a+b+c}}{sim = (b + c)/((2*a) + b + c)}
    \tab Wilson & Shmida (1984)
    \cr
    \code{cocogaston}
    \tab \eqn{sim = \frac{b+c}{a+b+c}}{sim = (b + c)/(a + b + c)}
    \tab Colwell & Coddington (1948), Gaston et al. (2001)
    \cr
    \code{magurran}
    \tab \eqn{sim = (2a+b+c)(1-\frac{a}{a+b+c})}{sim = ((2*a) + b + c)*(1- (a / (a + b + c)))}
    \tab Magurran (1988)
    \cr
    \code{harrison}
    \tab \eqn{sim = \frac{min(b,c)}{max(b,c)+a}}{sim = min(b, c)/(max(b, c) + a)}
    \tab Harrison et al. (1992), Koleff et al. (2003)
    \cr
    \code{cody}
    \tab \eqn{sim = 1-\frac{a(2a+b+c)}{2(a+b)(a+c)}}{sim = 1 - ((a*((2*a) + b + c))/(2 * ( a + b ) * ( a + c )))}
    \tab Cody (1993)
    \cr
    \code{williams}
    \tab \eqn{sim = \frac{min(b,c)}{a + b + c}}{sim = min(b, c) / (a + b + c)}
    \tab Williams (1996), Koleff et al. (2003)
    \cr
    \code{williams2}
    \tab \eqn{\frac{(bc)+1}{\frac{(a+b+c)^2-(a+b+c)}{2}}}{sim = ((b*c) + 1) / ((((a + b + c) * Exp(2) ) - (a + b + c)) / 2)}
    \tab Williams (1996), Koleff et al. (2003)
    \cr
    \code{harte}
    \tab \eqn{1-\frac{2a}{2a+b+c}}{sim = 1 - ((2*a) / ((2*a) + b + c))}
    \tab Harte & Kinzig (1997), Koleff et al. (2003)
    \cr
    \code{simpson}
    \tab \eqn{\frac{min(b,c)}{min(b,c)+a}}{sim = min(b,c) / (min(b,c) + a)}
    \tab Simpson (1949), Koleff et al. (2003)
    \cr
    \code{lennon}
    \tab \eqn{\frac{2|b-c|}{2a+b+c}}{sim = (2* abs(b-c)) / ((2*a) + b + c)}
    \tab Lennon et al. (2001), Koleff et al. (2003)
    \cr
    \code{weiher}
    \tab \eqn{sim = b+c}{sim = b + c}
    \tab Weiher & Boylen (1994)
    \cr
    \code{ruggiero }
    \tab \eqn{sim = \frac{a}{a+c}}{sim = a / (a + c)}
    \tab Ruggiero et al. (1998), Koleff et al. (2003)
    \cr
    \code{lennon2}
    \tab \eqn{sim = 1 - \left[ \frac{log \left( \frac{2a+b+c}{a+b+c} \right) }{log2} \right]}{sim = 1 - (log((2*a + b + c)/(a + b + c)) / log(2))}
    \tab Lennon et al. (2001), Koleff et al. (2003)
    \cr
    \code{rout1ledge}
    \tab \eqn{sim = \frac{(a+b+c)^2}{(a+b+c)^2-2bc}-1}{sim = (((a + b + c) * Exp(2))/(((a + b + c)*Exp(2))-(2*b*c))) - 1}
    \tab Routledge (1977), Magurran (1988)
    \cr
    \code{rout2ledge}
    \tab \eqn{too long, see below}{too long, see below}
    \tab Routledge (1977), Wilson & Shmida (1984)
    \cr
    \code{rout3ledge}
    \tab \eqn{sim = e^{rout2ledge}-1}{sim = exp(routledge2) - 1}
    \tab Routledge (1977)
    \cr
    \code{sokal1}
    \tab \eqn{sim = \frac{a}{a+2(b+c)}}{sim = a / (a + (2*(b + c)))}
    \tab Sokal & Sneath (1963)
    \cr
    \code{dice}
    \tab \eqn{sim = \frac{a}{min \left( (b+a),(c+a) \right)}}{sim = a / (min((b + a),(c + a)))}
    \tab Association index of Dice (1945), Wolda (1981)
    \cr
    \code{kulcz1insky}
    \tab \eqn{sim = \frac{a}{b+c}}{sim = a / (b + c)}
    \tab Oosting (1956), Southwood (1978)
    \cr
    \code{kulcz2insky}
    \tab \eqn{sim = \frac{\frac{a}{2}(2a+b+c)}{(a+b)(a+c)}}{sim = ((a/2) * ((2*a) + b +c)) / ((a + b) * (a + c))}
    \tab Oosting (1956), Southwood (1978)
    \cr
    \code{mcconnagh}
    \tab \eqn{sim = \frac{a^2-bc}{(a+b)(a+c)}}{sim = ((a^2)-(b * c)) / ((a + b) * (a + c))}
    \tab Hubalek (1982)
    \cr
    \code{simpson2}
    \tab \eqn{sim = \frac{a}{a+b}}{sim = a / (a + b)}
    \tab Simpson (1960), Shi (1993)
    \cr
    \code{legendre2}
    \tab \eqn{sim = \frac{3a}{3a + b + c}}{sim = (3*a)/((3*a) + b + c)}
    \tab Legendre & Legendre (1998)
    \cr
    \code{fager}
    \tab \eqn{sim = \frac{a}{\sqrt{n_1n_2}} - \frac{1}{2*\sqrt{n_2}}}{sim = (a / sqrt(n1*n2)) - (1/(2*sqrt(n2)))}
    \tab Fager (1957), Shi (1993)
    \cr
    \code{maarel}
    \tab \eqn{sim = \frac{2a - (b+c)}{2a + b+ c}}{sim = ((2*a) - (b+c)) / ((2*a) + b +c)}
    \tab van der Maarel (1969)
    \cr
    \code{lamont}
    \tab \eqn{sim = \frac{a}{2a + b+ c}}{sim = a / ((2*a) + b +c)}
    \tab Lamont and Grant (1979)
    \cr
    \code{johnson}
    \tab \eqn{sim = \frac{a}{2b}}{sim = a / (2*b)}
    \tab Johnson (1971)
    \cr
    \code{sorgenfrei}
    \tab \eqn{sim = \frac{a^2}{(a+b)(a+c)}}{sim = a^2 / ((a+b)*(a+c))}
    \tab Sorgenfrei (1959)
    \cr
    \code{johnson2}
    \tab \eqn{sim = \frac{a}{a+b}+\frac{a}{a+c}}{sim = (a/(a+b))+(a/(a+c))}
    \tab Johnson (1967)
	}

Computable symmetric indices (including unshared species):
  \tabular{lll}{
    \code{manhattan}
    \tab \eqn{sim = \frac{b+c}{a+b+c+d}}{sim = (b + c) / (a + b + c + d)}
    \tab Mean Manhattan, Legendre & Legendre (1998)
    \cr
    \code{simplematching}
    \tab \eqn{sim = \frac{a+d}{a+b+c+d}}{sim = (a+d) / (a+b+c+d)}
    \tab Sokal & Michener 1958
    \cr
    \code{margaleff}
    \tab \eqn{sim = \frac{a(a+b+c+d)}{(a+b)(a+c)}}{sim = (a* (a + b + c + d)) / ((a + b) * (a + c))}
    \tab Clifford & Stevenson (1975)
    \cr
    \code{pearson}
    \tab \eqn{sim = \frac{ad-bc}{\sqrt{(a+b)(a+c)(d+b)(d+c)}}}{sim = ((a*d) - (b*c)) / Sqrt((a + b)*(a + c)*(d + b)*(d + c))}
    \tab Phi of Pearson, Gower & Legendre (1986), 
    Yule (1912)
    \cr
    \code{roger}
    \tab \eqn{sim = \frac{a+d}{a+2(b+c)+d}}{sim = (a + d) / (a + 2*(b + c) +d)}
    \tab Rogers & Tanimoto (1960), 
    Gower & Legendre (1986)
    \cr
    \code{baroni}
    \tab \eqn{sim = \frac{\sqrt{ad}+a}{\sqrt{ad}+a+b+c}}{sim = ((sqrt(a*d)) + c) / ((sqrt(a*d)) + b + c + a)}
    \tab Baroni-Urbani & Buser (1976), Wolda (1981)
    \cr
    \code{dennis}
    \tab \eqn{sim = \frac{ad-bc}{\sqrt{(a+b+c+d)(a+b)(a+c)}}}{sim = ((a + d) - (b*c)) / (Sqrt((a + b + c + d) * (a + b) * (a + c)))}
    \tab Holliday et al. (2002), Ellis et al. (1993)
    \cr
    \code{fossum}
    \tab \eqn{sim = \frac{(a+b+c+d)\left(-\frac{a}{2}\right)^2}{(a+b)(a+c)}}{sim = ((a + b + c + d) * (-1 * ((a/2)^2))) / ((a + b) * (a + c))}
    \tab Holliday et al. (2002), Ellis et al. (1993)
    \cr
    \code{gower}
    \tab \eqn{sim = \frac{a-(b+c)+d}{a+b+c+d}}{sim = (a - (b + c) + d) / (a + b + c + d)}
    \tab Gower & Legendre (1986)
    \cr
    \code{legendre}
    \tab \eqn{sim = \frac{a}{a+b+c+d}}{sim = a / (a + b + c + d)}
    \tab Gower & Legendre (1986), Russell/Rao in Ellis et al. (1993) 
    \cr
    \code{sokal2}
    \tab \eqn{sim = \frac{ad}{\sqrt{(a+b)(a+c)(d+b)(d+c)}}}{sim = (a*d) / Sqrt((a + b) * (a + c) * (d + b) * (d + c))}
    \tab Sokal & Sneath (1963)
    \cr
    \code{sokal3}
    \tab \eqn{sim = \frac{2a+2d}{(a+d+(a+b+c+d)}}{sim = ((2*a) + (2*d)) / (a + d + (a + b + c + d))}
    \tab Sokal & Sneath (1963)
    \cr
    \code{sokal4}
    \tab \eqn{sim = \frac{a+d}{b+c}}{sim = (a + d) / (b + c)}
    \tab Sokal & Sneath (1963)
    \cr
    \code{stiles}
    \tab \eqn{sim = log\frac{(a+b+c+d) \left( |ad-bc|-\frac{a+b+c+d}{2}\right)^2 }{(a+b)(a+c)(b+d)(c+d)}}{sim = log(((a + b + c + d) * (( abs((a*d) - (b*c)) - ( (a + b + c + d) / 2))^2) / ((a + b) * (a + c) * (b + d) * (c + d))))}
    \tab Stiles (1946)
    \cr
    \code{yule}
    \tab \eqn{sim = \frac{ad-bc}{ad+bc}}{sim = ((a*d) - (b*c)) / ((a*d) + (b*c))}
    \tab Yule & Kendall (1973)
    \cr
    \code{michael}
    \tab \eqn{sim = \frac{4(ad-bc)}{(a+d)^2+(b+c)^2}}{sim = 4(ad-bc) / ((a+d)^2 + (b+c)^2)}
    \tab Michael (1920), Shi (1993)
    \cr
    \code{hamann}
    \tab \eqn{sim = \frac{(a+d)-(b+c)}{N}}{sim = ((a+d)-(b+c)) / N}
    \tab Hamann (1961)
    \cr
    \code{forbes}
    \tab \eqn{sim = \frac{(aN-2n_2)}{(Nn_1-2n_2)}}{sim = (a*N - 2*n2) / ((N*n1) - (2*n2))}
    \tab Forbes (1925), Shi (1993)
    \cr
    \code{chisquare}
    \tab \eqn{sim = \frac{(a+b+c+d)(ad-bc)^2}{(a+b)(a+c)(b+d)(c+d)}}{sim = ((a+b+c+d)*(ad - bc)^2) / (a+b)(a+c)(b+d)(c+d)}
    \tab Yule & Kendall (1950)
    \cr
    \code{peirce}
    \tab \eqn{sim = \frac{(ad-bc)}{(a+c)(b+d)}}{sim = (ad-bc) / ((a+c)(b+d))}
    \tab Peirce (1884)
    \cr
    \code{eyraud}
    \tab \eqn{sim = \frac{a-(a+b)(a+c)}{(a+b)(a+c)(b+d)(c+d)}}{sim = (a - ((a+b)(a+c))) / ((a+b)(a+c)(b+d)(c+d))}
    \tab Eyraud (1936) in Shi (1993)
    \cr
    \code{euclidean}
    \tab \eqn{sim = \frac{\sqrt{b+c}}{a+b+c+d}}{sim = (sqrt(b+c)) / (a+b+c+d)}
    \tab Mean Euclidean in Ellis et al. (1993)
    \cr
    \code{divergence}
    \tab \eqn{sim = \frac{\sqrt{b+c}}{sqrt{a+b+c+d}}}{sim = (sqrt(b+c)) / sqrt(a+b+c+d)}
    \tab Ellis et al. (1993)
  }
\code{rout2ledge} formula (Routledge, 1977; Koleff et al. 2003):

\eqn{\beta _{R2}  = \log(2a + b + c) - \left( {\frac{1}{{2a + b + c}}2a\log 2} \right) - \left( {\frac{1}{{2a + b + c}}((a + b)\log (a + b) + (a + c)\log (a + c))} \right)}{sim = log(2*a + b + c) - ((1/(2*a + b + c))*2*a*log(2)) - ((1/(2*a + b + c))*((a + b) * log(a + b) + (a + c) * log(a + c)))}

}

\value{
    If listout = FALSE a distance matrix of class \code{\link[stats]{dist}} is returned. If listout = TRUE, a \code{\link[base]{data.frame}} is returned with 7 columns giving the names of the compared plots in the first two and the calculated similarity measure in the third column. The rest of the columns give the values for \code{a, b, c,} and \code{d} (in this order). Naming of the first three columns can be changed but defaults to \code{NBX} (one of the compared plots), \code{NBY} (the other one), \code{used index} (the values of the calculated index). If \code{coord} != NULL, the following columns are given in addition and the columns \code{a:d} shift to the end of the data.frame. 
    
    \item{distance }{ Geographical distance between compared plots }
    \item{X }{ For plotting purposes, the x-coordinate of the virtual position of the calculated similarity value in the center between the two compared plots }
    \item{Y }{ For plotting purposes, the y-coordinate of the virtual position of the calculated similarity value in the center between the two compared plots }
    \item{xdist }{ Geographical distance between compared plots, on the x-axis only }
    \item{ydist }{ Geographical distance between compared plots, on the y-axis only }
}

\references{
    Albatineh, A. N., Niewiadomska-Bugaj, M. & Mihalko, D. (2006) On Similarity Indices and Correction for Chance Agreement. \emph{Journal of Classification} \bold{V23}: 301-313.

    Baroni-Urbani, C. & Buser, M. W. (1976) Similarity of Binary Data. \emph{Systematic Zoology} \bold{25}: 251-259.

    Clifford, H. T. & Stephenson, W. (1975) \emph{An introduction to numerical classification}. Academic Press, New York, San Francisco, London.

    Cody, M. L. (1993) Bird diversity components within and between habitats in Australia. - In: Ricklefs, R. E. & Schluter, D. (eds.), \emph{Species Diversity in Ecological Communities: historical and geographical perspectives}, pp. 147-158, University of Chicago Press, Chicago

    Colwell, R. K. & Coddington, J. A. (1994) Estimating terrestrial biodiversity through extrapolation. \emph{Philosophical Transactions of the Royal Society of London Series B-Biological Sciences} \bold{345}: 101-118.

    Dice, L. R. (1945) Measures of the amount of ecological association between species. \emph{Ecology} \bold{26}: 297-302.
    
    Ellis, D., Furner-Hines, J., Willett, P. (1993) Measuring the degree of similarity between objects in text retrieval systems. \emph{Perspectives in Information Management} \bold{3}(2): 128-149

    Fager, E. W. (1957) Determination and analysis of recurrent groups. \emph{Ecology} \bold{38}: 586-595.

    Faith, D. P., Minchin, P. R. & Belbin, L. (1987) Compositional dissimilarity as a robust measure of ecological distance. \emph{Plant Ecology} \bold{69}: 57-68.

    Gaston, K. J., Rodrigues, A. S. L., van Rensburg, B. J., Koleff, P. & Chown, S. L. (2001) Complementary representation and zones of ecological transition. \emph{Ecology Letters} \bold{4}: 4-9.

    Gower, J. C. & Legendre, P. (1986) Metric and Euclidean properties of dissimilarity coefficients. \emph{Journal of Classification} \bold{3}: 5-48.

    Hajdu, L. J. (1981) Graphical comparison of resemblance measures in phytosociology. \emph{Plant Ecology} \bold{V48}: 47-59.

    Harrison, S., Ross, S. J. & Lawton, J. H. (1992) Beta diversity on geographic gradients in Britain. \emph{Journal of Animal Ecology} \bold{61}: 151-158.

    Harte, J. & Kinzig, A. (1997) On the implications of species-area relationships for endemism, spatial turnover and food web patterns. \emph{Oikos} \bold{80}.

    Holliday, J. D., Hu, C.-Y. & Willett, P. (2002) Grouping of Coefficients for the Calculation of Inter-Molecular Similarity and Dissimilarity using 2D Fragment Bit-Strings. \emph{Combinatorial Chemistry & High Throughput Screening} \bold{5}: 155-166.

    Hubalek, Z. (1982) Coefficients of association and similarity, based on binary (presence-absence) data: An evaluation. \emph{Biological Reviews of the Cambridge Philosophical Society} \bold{57}: 669-689.

    Huhta, V. (1979) Evaluation of different similarity indices as measures of succession in arthropod communities of the forest floor after clear-cutting. \emph{Oecologia} \bold{V41}: 11-23.

    Jaccard, P. (1901) Etude comparative de la distribution florale d’une portion des Alpes et du Jura. \emph{Bulletin de la Societé Vaudoise des Sciences Naturelles} \bold{37}: 547-579.

    Jaccard, P. (1912) The distribution of the flora of the alpine zone. \emph{New Phytologist} \bold{11}: 37-50.

    Johnson, J. G. (1971) A quantitative approach to faunal province analysis. \emph{American Journal of Science} \bold{270}: 257-280.

    Johnson, S. C. (1967) Hierarchical clustering schemes. \emph{Psychometrika} \bold{32}: 241-254.

    Jurasinski, G. & Beierkuhnlein, C. (2006) Spatial patterns of biodiversity - assessing vegetation using hexagonal grids. \emph{Proceedings of the Royal Irish Academy - Biology and Environment} \bold{106B}: 401-411.

    Jurasinski, G. & Beierkuhnlein, C. (submitted) Distance decay and non-stationarity in a semi-arid Mediterranean ecosystem. \emph{Journal of Vegetation Science}.

    Koleff, P., Gaston, K. J. & Lennon, J. J. (2003) Measuring beta diversity for presence-absence data. \emph{Journal of Animal Ecology} \bold{72}: 367-382.

    Lamont, B. B. & Grant, K. J. (1979) A comparison of twenty-one measures of site dissimilarity. - In: Orlóci, L., Rao, C. R. & Stiteler, W. M. (eds.), \emph{Multivariate Methods in Ecological Work}, pp. 101-126, Int. Coop. Publ. House, Fairland, MD

    Lande, R. (1996) Statistics and partitioning of species diversity and similarity along multiple communities. \emph{Oikos} \bold{76}: 25-39.

    Legendre, P. & Legendre, L. (1998) \emph{Numerical Ecology}. Elsevier, Amsterdam.

    Lennon, J. J., Koleff, P., Greenwood, J. J. D. & Gaston, K. J. (2001) The geographical structure of British bird distributions: diversity, spatial turnover and scale. \emph{J Anim Ecology} \bold{70}: 966-979.

    Magurran, A. E. (1988) \emph{Ecological Diversity and its Measurement}. Chapman & Hall, London.

    Mountford, M. D. (1962) An index of similarity and its application to classification problems. - In: Murphy, P. W. (ed.) \emph{Progress in Soil Zoology}, pp. 43-50, Butterworths
    
    Ochiai, A. (1957) Zoogeographical studies on the soleoid fishes found in Japan and its neighbouring regions. \emph{Bulletin of the Japanese Society of Fisheries Science} \bold{22}(9): pp. 526-530
    
    Oosting, H. J. (1956) \emph{The study of plant communities: an introduction to plant ecolog}. W. H. Freeman, San Francisco.

    Rogers, D. J. & Tanimoto, T. T. (1960) A computer program for classifying plants. \emph{Science} \bold{132}: 1115-1118.

    Routledge, R. D. (1977) On Whittaker’s components of diversity. \emph{Ecology} \bold{58}: 1120-1127.

    Ruggiero, A., Lawton, J. H. & Blackburn, T. M. (1998) The geographic ranges of mammalian species in South America: spatial patterns in environmental resistance and anisotropy. \emph{Journal of Biogeography} \bold{25}: 1093-1103.

    Shi, G. R. (1993) Multivariate data analysis in palaeoecology and palaeobiogeography--a review. \emph{Palaeogeography, Palaeoclimatology, Palaeoecology} \bold{105}: 199-234.

    Simpson, E. H. (1949) The measurement of diversity. \emph{Nature} \bold{163}: 688.

    Simpson, G. G. (1960) Notes on the measurement of faunal resemblance. \emph{American Journal of Science} \bold{258-A}: 300-311.

    Sokal, R. R. & Michener, C. D. (1958) A statistical method for evaluating systematic relationships. \emph{University of Kansas Science Bulletin} \bold{38}: 1409-1438.

    Sokal, R. R. & Sneath, P. H. A. (1963) \emph{Principles of numerical taxonomy}. W. H. Freeman, San Francisco.

    Sørensen, T. (1948) A method of establishing groups of equal amplitude in plant sociology based on similarity of species content. \emph{Biologiske Skrifter} \bold{5}: 1-34.

    Sorgenfrei, T. (1959) Molluscan assemblages from the marine middle Miocene of South Jutland and their environments. \emph{Danmark Geologiske Undersøgelse. Serie 2} \bold{79}: 403-408.

    Southwood, T. S. (1978) \emph{Ecological Methods}. Chapman and Hall, London.
    
    Stiles (1961) The association factor in information retrieval. \emph{Journal of the Association for Computing Machinery.} \bold{8} 271-279

    Weiher, E. & Boylen, C. W. (1994) Patterns and prediction of alpha and beta diversity of aquatic plants in Adirondack (New York) lakes. \emph{Canadian Journal of Botany-Revue Canadienne De Botanique} \bold{72}: 1797-1804.

    Whittaker, R. H. (1960) Vegetation of the Siskiyou Mountains, Orgeon and California. \emph{Ecological Monographs} \bold{30}: 279-338.

    Williams, P. H. (1996) Mapping variations in the strength and breadth of biogeographic transition zones using species turnover. \emph{Proceedings of the Royal Society of London Series B-Biological Sciences} \bold{263}: 579-588.

    Williams, P. H., Klerk, H. M. & Crowe, T. M. (1999) Interpreting biogeographical boundaries among Afrotropical birds: spatial patterns in richness gradients and species replacement. \emph{J Biogeography} \bold{26}: 459-474.
    
    Wilson, M. V. & Shmida, A. (1984) Measuring beta-diversity with presence-absence data. \emph{Journal of Ecology} \bold{72}: 1055-1064.
    
    Wolda, H. (1981) Similarity indices, sample size and diversity. \emph{Oecologia} \bold{50}: 296-302.
    
    Yule, G. U. & Kendall, M. G. (1973) \emph{An introduction to the theory of statistics}. Griffin, London.
    
    Yule, G. U. (1912) \emph{On the methods of measuring association between two attributes}. Journal of the Royal Statistical Society \bold{75}(6): 579-642
}
  
\author{ Gerald Jurasinski \email{gerald.jurasinski@uni-rostock.de} }

\note{ In general, concepts of data-handling are taken from \code{\link[vegan]{vegdist}} and the calculation of \code{a}, \code{b}, \code{c} and \code{d} is taken from \code{\link[ade4]{dist.binary}}. Thanks to Jari Oksanen for his \code{vegan} package. The indices were collected from the literature and are applicable in different fields of research.
}

\seealso{ 
	\code{\link[vegan]{vegdist}}, \code{\link[ade4]{dist.binary}}, 
	\code{\link[labdsv]{dsvdis}}, \code{\link[stats]{dist}} 
	}
	
\examples{
data(abis)
##calculate jaccard similarity and output as dist-object
jacc.dist <- sim(abis.spec, method="jaccard") 

##calculate Whittaker similarity (with prior normalisation) and 
##output as data.frame
whitt.list <- sim(abis.spec, method="whittaker", normalize=TRUE, 
listout=TRUE) 

##calculate similarity from a database list after Harte & Kinzig (1997) 
##and output as dist-object
abis.spec.ls <- liste(abis.spec, splist=TRUE)
hart.dist <- sim(abis.spec.ls, method="harte", listin=TRUE) 

## calculate the geographic distances between sites simultaneously
## and return only similarities calculated between neighboring plots
abis.soer <- sim(abis.spec, coord=abis.env[,1:2], dn=100)

## in an equidistant array
## you can plot this nice between the original positions of the
## sites (with the size of the dots expressing number of species
## for the sites, and value of the Sørensen coefficient in between)
require(geoR)
points.geodata(coord=abis.env[,1:2], data=abis.env$n.spec, 
cex.min=1, cex.max=5)
points.geodata(coord=abis.soer[,5:6], data=abis.soer$soerensen, 
cex.min=1, cex.max=5, col="grey50", add=TRUE)

}

\keyword{ methods }
\keyword{ multivariate }
