\encoding{UTF-8}
\name{aslopect}
\alias{aslopect}
\title{ Calculate similarity of plots based on slope aspect and inclination }
\description{ Allows for the comparison of plots regarding the two variables slope aspect and slope inclination at once. To obtain a distance measure integrating aspect and inclination the model of a unit sphere is used and great-circle distances between virtual locations are calculated. For each plot a virtual location on the sphere is defined using the values for aspect as longitude and 90°-inclination as latitude. See \code{details} for more...
}

\usage{
aslopect(asp, slo, names=rownames(asp), fc=FALSE, listout = FALSE)
}

\arguments{
  \item{asp}{ Numeric vector with aspect values, given in degree. Expects values between 0\eqn{^\circ}{°} (North) and 180\eqn{^\circ}{°}/-180\eqn{^\circ}{°} (South). Eastward directions count positive, westward directions count negative. Alternatively the aspect values can be given in degrees of a full circle up to 359\eqn{^\circ}{°} starting from 0\eqn{^\circ}{°} (North). In the latter case fc has to be set to \code{TRUE}. }
  \item{slo}{ Numeric vector with slope inclination values. Expects values between 0\eqn{^\circ}{°}(flat) and 90\eqn{^\circ}{°} (vertical wall). }
  \item{names}{ Plot names, defaults to the rownames of \code{asp}, but a separate vector can be specified. Its length has to match the length of \code{asp} and \code{slo}. }
  \item{fc}{ Triggers conversion from full circle degrees to half circle degrees. Set this to \code{TRUE} if full circle degrees (from 0\eqn{^\circ}{°} to 360\eqn{^\circ}{°}) are given in \code{asp}. }
  \item{listout}{ Shall the results be given in list-format (\code{data.frame}). Defaults to FALSE which means that a matrix of class \code{\link[stats]{dist}} is returned. }
}

\details{
To obtain a distance measure integrating aspect and inclination the model of a unit sphere is used and great-circle distances between virtual locations are calculated. For each plot a virtual location on the sphere is defined using the values for aspect as longitude and 90\eqn{^\circ}{°}-inclination as latitude. This means that as long as the inclination is low the virtual points are located in the pole region so that, regardless of aspect, plots with low inclination are rather close to each other regarding these qualities. The idea behind is, that solar radiation, wind or other factors highly depending on aspect and inclination are not really differing between plots with different aspect as long as the slope is low. The longitude values on the unit sphere are derived from the values of slope aspect. The equator of the sphere is thought as the compass circle. The Prime Meridian of the virtual sphere is the great circle through North and South of the compass. As in geographic terms longitude counts positive in Eastern and negative in Western direction. With \eqn{\phi} = latitude = 90\eqn{^\circ}{°}-inclination and \eqn{\lambda} = longitude = aspect the great-circle distance between A and B can be calculated with the following formula.

\eqn{sim = \zeta =\arccos\Big(\sin(\phi_A)  \cdot \sin(\phi_B) + \cos(\phi_A) \cdot \cos(\phi_B) \cdot \cos(\lambda_B - \lambda_A) \Big)}{sim = arccos(sin(phiA)*sin(phiB) + cos(phiA)*cos(phiB)*cos(lambdaB-lambdaA))}
}

\value{
  Returns a \code{dist} object or a \code{data.frame} (depending if listout = FALSE or TRUE). As a unit sphere is used, the maximum distance between two inclination/aspect pairs is perimeter/2 of the sphere which is by definition \eqn{\pi}{Pi}. To scale the possible distances between 0 and 1 the results are divided by \eqn{\pi}{Pi}. Thus, a great-circle distance of 1 is rather scarce in the real world, however, two vertical rock walls with opposite aspect would share it. If listout = TRUE a \code{data.frame} with the following variables returns.
  \item{NBX}{ one of the compared sampling units }
  \item{NBY}{ the other part of the pair }
  \item{x}{ The returned aslopect value }
}

\author{ Gerald Jurasinski }

\examples{
data(abis)
## identify columns with slope and aspect data
names(abis.env)

## calculate aslopect
abis.aslop <- aslopect(abis.env[,4], abis.env[,5])

}

\keyword{ methods }
\keyword{ univar }
