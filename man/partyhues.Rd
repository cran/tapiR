\name{partyhues}
\alias{partyhues}
\title{ Convert Major Party Names to Colours }
\description{
  Converts names of political parties into colours, for plotting
  purposes.  "Lab" becomes "red", "Con" becomes "blue", "LD" becomes
  "yellow", others are "black".
}
\usage{
partyhues(vec)
}
\arguments{
  \item{vec}{ A vector of party names }
}
\value{
  A character vector of colours.
}
\author{ David Firth }
\examples{
partyhues(c("LD", "SNP", "LD", "Con", "Lab"))
}
\keyword{ manip }

