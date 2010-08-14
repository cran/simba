\encoding{UTF-8}
\name{abis}
\alias{abis}
\alias{abis.spec}
\alias{abis.env}
\docType{data}
\title{ Vegetation and environmental data recorded in a Tundra ecosystem in Northern Sweden (Southeast of Abisko). }

\usage{
  data(abis)
}

\description{
Vegetation and related data from field sites in a Tundra Ecosystem in Northern Sweden. \code{abis.spec} holds abundance information of 158 species on 61 sites.
}

\format{
  \itemize{
    \item{abis.env}{ contains some environmental variables from the Abisko field sites. These are:
    
    \itemize{    
    \item{\code{X} }{ Numeric: The x-coordinates of the field sites (utm). The sites are arranged in a regular equidistant grid. Each site is a hexagon plot with 5m radius. }
    \item{Y }{ Numeric: The y-coordinates of the geographic position of the field sites. }
    \item{alt }{ Integer: Height a.s.l. of the field sites. }
    \item{aspect }{ Exposition of the sites. In degree from 0 to 180/-180. Counts negative in westward direction and positive in eastward direction. }
    \item{slope }{ Numeric: the inclination of the sites in degree }
    \item{pH }{ Numeric: pH value of soil samples taken on the sites (mixed samples from three  }
    \item{n.spec }{ Integer: Number of species on the site. }
    \item{shannon }{ Numeric: Shannon index calculated from the species data. }
    \item{even }{ Numeric: Evenness calculated from the species data. }
    \item{simps }{ Numeric: Simpson diversity index for the site. }
    \item{ephwtrs1 }{ Numeric: Cover of ephemeral waters }
    \item{ephwtrs2 }{ Numeric: Cover of dried out ephemeral waters? }
    \item{roughn }{ Numeric: Roughness index for the site. Obtained through calculating the ratio between the length of the straight lines dissecting the hexagonal plot and the lengths of lines along this dissections but exactly following the surface. }
    \item{stones.cov }{ Numeric: Proportion of area of site covered by rocks which are covered with vegetation. }
    \item{stones.uncov }{ Numeric: Proportion of area of site with uncovered, bare rocks. }
    \item{bare.soil }{ Numeric: Open soil (most often sandy substrates). }
    \item{water }{ Numeric: Proportion of area covered by permanent water bodies. }
    \item{bog }{ Numeric: Proportion of area covered with boggy depressions. }
    \item{lemming }{ Numeric: Counts of Lemming feces. }
    \item{elk }{ Numeric: Counts of Elk feces. }
    \item{ren }{ Numeric: Counts of Ren feces. }
    \item{ripa }{ Numeric: Counts of Ripa feces. }
    \item{fox }{ Numeric: Counts of Fox feces. }
    \item{grubbings }{ Numeric: Amount of mole grubbings. }
    \item{tread }{ Numeric: Amount of hoof tread. }
    \item{betula }{ Numeric: Cover of \emph{Betula}. }
    \item{CEC }{ Numeric: Cation Exchange Capacity measured from the mixed soil sample taken in the field. }
    \item{base.satur }{ Numeric: Base saturation. }
    \item{heath }{ Binary: Does the site belong to the vegetation type 'heath'? }
    \item{shrubs }{ Binary: Does the site belong to the vegetation type 'shrubby vegetation'? }
    \item{protect }{ Binary: Does the site belong to the vegetation type 'protected snow heath'? }
    }}  
}
}

\source{
  Jurasinski G, Jentsch A, Retzer V, Beierkuhnlein C (submitted) Assessing gradients in species composition with multiple plot similarity coefficients. \emph{Ecography}
  
  Rettenmaier, N. 2004. Räumliche Muster der Biodiversität in der skandinavischen Tundra - Diploma thesis (unpublished), Department of Biogeography, University of Bayreuth, pp. 96.
}

\examples{
data(abis)
}

\keyword{datasets}
