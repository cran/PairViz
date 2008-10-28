\name{pv_pcp}
\alias{pv_pcp}   

\title{Enhanced parallel coordinate plot }
\description{
This function draws a parallel coordinate plot of data. Variables
may be reordered and panels colored in the display. It is a modified
version of \code{parcoord {MASS}}.
}
\usage{
pv_pcp(data, order = NULL, panel.colors = NULL, col = 1, lty = 1, horizontal = TRUE, mar = NULL,scale=TRUE,axis.width=0, ...)
}

\arguments{
\item{data}{a numeric matrix }
  \item{order}{the order of variables. Default is the order in data.}
  \item{panel.colors}{either a vector or a matrix of panel colors. If a 
  vector is supplied, the ith color is used for the ith panel. If a matrix, dimensions 
  should match those of the variables. Diagonal entries are ignored. }
  \item{col}{ a vector of colours, recycled as necessary for each observation.  }
  \item{lty}{ a vector of line types, 
recycled as necessary for each observation.  }
  \item{horizontal}{ If TRUE, orientation is horizontal. }
  \item{mar}{ margin parameters, passed to \code{par}. }
  \item{scale}{ If \code{TRUE} each column of data is scaled to unit interval prior to plotting.}
  \item{axis.width}{ A vector of widths (recycled as needed) used to stretch the axes.}
  \item{\dots}{ graphics parameters which are passed to matplot.}
}
\details{
  If \code{panel.colors} is a matrix and \code{order} is supplied, \code{panel.colors} is 
  reordered.}

\references{}
\author{ Catherine B. Hurley }

\seealso{\code{\link{guided_pcp}}}


\examples{

}

\keyword{multivariate }
\keyword{color }
\keyword{hplot }
