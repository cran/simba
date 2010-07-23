\encoding{UTF-8}
\name{simba-internal}
\alias{print.cslist}
\alias{print.diffmich}
\alias{print.dmn}
\alias{print.dsl}
\alias{print.dsl2}
\alias{print.occtmp}
\alias{print.pclist}
\alias{print.permcor}
\title{ Internal functions of the package simba that are usually not called (or only implicitly) by the user }
\description{
Mainly print functions for giving nice output.
}

\usage{
\method{print}{cslist}(x, digits = max(3, getOption("digits") - 3), ...)
\method{print}{diffmich}(x, digits = max(3, getOption("digits") - 3), ...)
\method{print}{dmn}(x, digits = max(3, getOption("digits") - 3), ...)
\method{print}{dsl}(x, digits = max(3, getOption("digits") - 3), ...)
\method{print}{dsl2}(x, digits = max(3, getOption("digits") - 3), ...)
\method{print}{occtmp}(x, digits = max(3, getOption("digits") - 3), ...)
\method{print}{pclist}(x, digits = max(3, getOption("digits") - 3), ...)
\method{print}{permcor}(x, digits = max(3, getOption("digits") - 3), ...)
}

\arguments{
  \item{x}{ object to be printed }
  \item{digits}{ number of digits }
  \item{\dots}{ Further arguments. }
}

\details{
    These are just printing methods to give nice output. Usually they are not to be called by the user.
}

\value{
  prints the output of a function and invisibly gives back the original object
}

\author{ Gerald Jurasinski }

\keyword{ internal }
