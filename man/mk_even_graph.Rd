\name{mk_even_graph}
\docType{methods}
\alias{mk_even_graph-methods}
\alias{mk_even_graph,ANY-method}
\alias{mk_even_graph,graphNEL-method}
\alias{mk_even_graph,matrix-method}
\alias{mk_even_graph,numeric-method}
\alias{mk_even_graph,even_graph-method}
\alias{mk_even_graph}

\title{  Constructs an even graph}
\description{
 ~~ Methods for function \code{mk_even_graph}. Each of these return an instance of \code{even_graph}, where all nodes are of even degree. The result  satisfies \code{is_even_graph}.
The resulting graph yields an euler tour.}
\section{Methods}{
\describe{


\item{self = "graphNEL",use\_weights=TRUE,add\_edges=TRUE}{ This is the workhorse method. If \code{self} does not satisfy \code{is_even_graph}, the graph is forced to be even by one of the folowing. If add\_edges is TRUE, the odd nodes are paired off and a new edge added between each  pair, possibly duplicating an existing edge. If add\_edges is a vector of the odd nodes, they are paired off in this order.
	If add\_edges is FALSE a new dummy node is added with
edges going to all odd nodes.  }

\item{self = "matrix",use\_weights=TRUE,add\_edges=TRUE}{ first constructs a complete graph using \code{mk_complete_graph}, which is then augmented to be even. }


\item{self = "numeric",use\_weights=FALSE,add\_edges=TRUE}{ first constructs a complete graph using  \code{mk_complete_graph}, which is then augmented to be even. }

\item{self = "ANY",use\_weights=TRUE,add\_edges=TRUE}{ first constructs a complete graph using  \code{mk_complete_graph}, which is then augmented to be even. }

\item{self = "even\_graph",add\_edges=TRUE}{ returns self.}


}}

\references{see \link{overview}}

\seealso{\code{\link{mk_complete_graph}}, \code{\link{is_even_graph}}}

\keyword{methods}
\keyword{graphs}