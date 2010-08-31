\name{knn_graph}
\Rdversion{1.1}
\alias{knn_graph}
\alias{dn_graph}
\alias{mk_hypercube_graph}
\alias{mk_line_graph}
\alias{graph_product}



%- Also NEED an '\alias' for EACH other topic documented here.
\title{Functions to construct graphs.}
\description{
Functions to construct graphs- see details below.}
\usage{
knn_graph(g, k = 2)
dn_graph(g, d = 1)
mk_hypercube_graph(n)
mk_line_graph(g)
graph_product(g,h, type="cartesian")
}
\arguments{
  \item{g}{a graph}
  \item{h}{a graph}
  \item{n}{a positive integer, or a character vector.}
  \item{k}{a positive integer}
  \item{d}{a maximum edge weight}
  \item{type}{the type of graph product, one of "cartesian", "strong" or "tensor" }
  }
\details{
knn_graph(k)- returns a symmetric k nearest neighbour graph

dn_graph(d)- returns a  graph where edges of weight above d are deleted. Nodes with no edges are also removed.

mk_hypercube_graph(n)- returns a hypercube graph with $2^n$ nodes

mk_line_graph(n)- returns the line graph of g

graph_product(g,h)- returns the graph product of hg and h.
}

\references{
See any Graph Theory text}
\author{ C.B. Hurley and R.W. Oldford }
