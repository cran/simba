\encoding{UTF-8}
\name{bernina}
\alias{summits}
\alias{veg}
\alias{veg.lst}
\alias{years}
\docType{data}
\title{ Repeated vegetation records from 7 Alpine summits of the Bernina range. }

\usage{
  data(bernina)
}

\description{
Repeated species records from 7 Alpine summits of the Bernina range. The data contains species data from repeated field recordings that were carried out on the summits between 1905 and 1907 (Ruebel 1912), in 1985 (Hofer 1992), and in 2003 (Walther et al. 2005). Data was compiled by Walther et al. (2005) and used in Jurasinski & Kreyling (2007) to investigate homogenization of Alpine summit floras.
}

\format{
  	\itemize{
  		\item{\code{veg}}{ Complete pecies matrix that contains the species records from the repeated samplings. Integer entries of '0' and '1'. Hence, the matrix represents presence/absence information. }
  	}
  	
  	\itemize{
  		\item{\code{veg.lst}}{ Complete pecies data in list format that contains the species records from the repeated samplings. Has three columns \dots. 
  		
  		\itemize{
  			\item{\code{plot}}{ Factor: The summit names. Repeat for each species and time period. }
  			\item{\code{spec}}{ Factor: The species names. }
  			\item{\code{occurrence}}{ Integer: The occurrence information. Because it is presence/absence it is '1' for all species in the list.}
  		}}
  	}
  	
  	\itemize{
    	\item{\code{summits}}{ Information on the repeatedly sampled summits of the Bernina range. Most information repeats. However, the year, and the number of species change from record year to record year.
    
    	\itemize{    
    		\item{\code{year} }{ Numeric: The year of the recording. }
    		\item{\code{number} }{ Numeric: The summit number. Can be used for efficient reference to the summits. }
    		\item{\code{summit} }{ Factor: Names of the summits. Repeat per recording period. }
    		\item{\code{altitude} }{ Numeric: Altitudes of the summits. Repeat per recording period. }
    		\item{\code{northing} }{ Numeric: UTM coordinates of the summit. Repeat per recording period. }
    		\item{\code{easting} }{ Numeric: UTM coordinates of the summit. Repeat per recording period. }
    		\item{\code{n.spec} }{ Numeric: Number of species found on the summit. Changes with recording period. }
    		
    	}}  
	}
	
	\itemize{
		\item{\code{years}}{ Character: A vector of repeated character strings that give the information from which year the resepective parts of \code{veg.lst} are. Therefore \code{length(years) == nrow(veg.lst)}. }
	}
}

\source{
  Hofer HR, 1992. Veraenderungen in der Vegetation von 14 Gipfeln des Berninagebietes zwischen 1905 und 1985. \emph{Ber. Geobot. Inst. Eidg. Tech. Hochsch. Stift. Ruebel Zuer.} 58: 39-54.

  Jurasinski G & Kreyling J, 2007. Upward shift of alpine plants increases floristic similarity of mountain summits. \emph{Journal of Vegetation Science} 18: 711–718.
  
  Ruebel E, 1912. Pflanzengeographische Monographie des Bernina-Gebietes. \emph{Engelmann, Leipzig, DE}.
  
  Walther G-R, Beißner S, Burga CA, 2005. Trends in the upward shift of alpine plants. \emph{Journal of Vegetation Science} 16: 541-548.
}

\examples{
data(bernina)
}

\keyword{datasets}
