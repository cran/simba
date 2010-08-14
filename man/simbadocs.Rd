\encoding{UTF-8}
\name{simbadocs}
\alias{simbadocs}

\title{Display Package Documentation}

\description{
  Display package documentation using the \code{pager} or \code{pdfviewer}
  defined in \code{\link{options}}.
}
\usage{
simbadocs(doc = c("NEWS", "ChangeLog", "mps-coefficients.pdf", "simba_0.3-2_man.pdf"))
}

\arguments{
  \item{doc }{ The name of the document to view (partial match, case sensitive). }
}

\author{Jari Oksanen, adapted by Gerald Jurasinski }

\note{
  Since \R does not have this facility Jari Oksanen provided a workaround for his \code{vegan} package (\code{\link[vegan]{vegandocs}}). This here is just adapted to \code{simba}. 
}

\seealso{\code{\link{vignette}}. }

\examples{
\dontrun{
simbadocs("Change")
}
}

\keyword{ utilities }
\keyword{ documentation }
