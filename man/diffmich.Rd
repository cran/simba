\encoding{UTF-8}
\name{diffmich}
\alias{diffmich}
\alias{plot.diffmich}
\alias{fitmich}
\title{ Calculate the difference in parameters of a Michaelis-Menten kinetik fitted to (PAM) data }
\description{ The function can be used to calculate the difference in the two parameters of the Michaelis-Menten Kinetik \eqn{y=\frac{a*x}{b+x}}{y=(a*x)/(b+x)} between two datasets containg each two vectors. Through permutation it is possible to compute significance of the difference. \code{\link[simba]{fitmich}} is used to calculate the Michaelis-Menten fit to the data. With the corresponding plot method a plot of the actual difference in the parameters against a histogram of the permuted values can easily be achieved.
}

\usage{
diffmich(x1, y1, x2, y2, permutations = 1000, a=3, b=0.5, trace=FALSE, ...)
fitmich(x, y, a=3, b=0.5)
\method{plot}{diffmich}(x, y, which=3, two=2, ...)
}

\arguments{
  \item{x1}{ Vector containing an independent variable, for instance PAR measurements. }
  \item{y1}{ Vector containing a variable dependent on \code{x1} (for instance ETR measurements). Must have the same length as \code{x1}. }
  \item{x2}{ Vector containing a second independent variable (for instance PAR measurements). }
  \item{y2}{ Vector containing a variable dependent on \code{x2} (for instance ETR measurements). Must have the same length as \code{x2}. }
  \item{permutations}{ Number of permutations. }
  \item{a}{ start value for parameter a, defaults to 3, usually there is no change necessary, but if the function gets trapped in the first run, changing the parameters might solve the problem. }
  \item{b}{ Start value for parameter b, defaults to 0.5. }
  \item{trace}{ set to TRUE for displaying the progress of the calculation }
  \item{\dots}{ Arguments to other functions (for instance to \code{\link[stats]{lm}}, which is used to calculate the regression lines) }
  \item{x}{ Vector containing an independent variable, for instance PAR measurements. Function \code{fitmich} is usually called only internally by \code{diffmich}. Plotting object in the plot method. }
  \item{y}{ Vector containing a variable dependent on \code{x1} (for instance ETR measurements). Must have the same length as \code{x1}. Plotting object in the plotting method, optional when \code{x} has appropriate structure}
  \item{which}{ Which histogram should be plotted? 1 triggers the histogram for parameter a, 2 the one for parameter b. It defaults to 3: both histograms are plotted. If it is changed from default the next argument (\code{two}) is automatically set to 1! }
  \item{two}{ Should the histograms be printed on a divided display? And how? Can only be set if \code{which} is set to 3. Defaults to 2, which means that the display is divided in two halfs and the histogram-plots are plotted side by side. 3 causes histograms to be plotted one on top of the other. If \code{two} = 1, the display is NOT automatically divided. Might be useful if more than one \code{diffmich}-objekt is to be plotted on one display. Otherwise the function overrides the actual display settings. }
}

\details{
As the function was initially built to easily calculate the difference of parameters of the Michaelis-Menten Kinetik for PAM measurements, the independent vectors are meant to contain PAR values whereas the dependent vectors should represent ETR values. But you can use it for anything else which can be fitted with Michaelis-Menten. The vectors belonging together are formed into a \code{\link[base]{data.frame}}. For each permutation run the rows are interchanged randomly between the two \code{\link[base]{data.frame}}s and the difference in the parameters is calculated and collected into a vector. The p-value is then computed as the ratio between the number of cases where the differences in Parameter exceed the difference in parameter of the inital configuration and the number of permutations.

As it uses a \code{for} loop it takes a while to calculate. So get a coffee while it is running, or set \code{trace} = TRUE to avoid boring moments ...
}

\value{
  Returns a \code{diffmich}-object with the function call, the difference in the two parameters and their significance. Furthermore the number of permutations. If you want to change the way \code{\link[simba]{fitmich}} is computed you can change the starting values. Per default it is calculated with starting values a=3 and b=0.5. There's no change needed unless the function gets trapped.
}

\author{ Gerald Jurasinski }

\seealso{ \code{\link[stats]{nls}}, \code{\link[base]{sample}} }

\examples{

}

\keyword{ methods }
\keyword{ univar }
