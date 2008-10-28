\name{eseq}
\alias{eseq} 
\alias{eseqa} 
\alias{kntour_drop} 
\alias{kntour_add} 

\title{Construct eulerian paths on the complete graph where nodes are integers 1..n.}

\description{
Constructs an eulerian 
 on  the complete graph where nodes are integers 1..n. The result in an euler tour for odd \code{n}. For even  \code{n} the result is not exactly an euler tour or path because (n-2)/2 edges must be visited twice.
  


}
\usage{
eseq(n)
eseqa(n)
kntour_drop(e)
kntour_add(e)
}
\arguments{
  \item{n}{a positive integer.}
  \item{e}{an euler tour on Kn where n is odd.}
}

\value{a numeric vector.}

\details{The algorithm used for eseq builds up an euler tour on 1..n  by appending extra edges on to the tour on 1..(n-2). 

The function eseqa constructs tours on 1..n using an alternative algorithm.

The function kntour\_drop removes instances of n from the euler tour, creating an open  approximately eulerian path on the complete graph with n-1 nodes.

The function kntour\_add inserts an extra node n+1 into an euler tour
on the complete graph on n nodes. It adds a detour to the tour visiting all edges joining nodes 1..n to n+1. The result is an open  approximately eulerian path on the complete graph with n+1 nodes.


	}
\references{see \link{overview}}

\author{ C.B. Hurley and R.W. Oldford }

\seealso{\code{\link{hpaths}}, \code{\link{eulerian}}.}

\examples{

require(PairViz)
eseq(5)
eseq(6)


}

\keyword{graphs}

