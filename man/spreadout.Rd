\name{spreadout}
\alias{rater_spreadout}
\alias{factor_spreadout}

\title{
Functions to prepare for categorical parallel coordinates, drawn by catpcp.
}
\description{
factor_spread spreads out the data at each factor level.
rater_spread spreads out the data at each rating level.
The rater version is appropriate when the variables (factors) have all the same levels.
}
\usage{
factor_spreadout(d)
rater_spreadout(d, levs, minspace = NULL)
}

\arguments{
  \item{d}{A data frame where each variable can be interpreted as a factor.
}
  \item{levs}{The rating levels. Specifying this controls the order of rating levels on each axis.}
  \item{minspace}{The minimum amount of space between the bars.
}
}
