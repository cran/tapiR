\name{collect.MPnames}
\alias{collect.MPnames}
\title{ Gather all MP Names from a Set of Division Lists }
\description{
  Constructs a comprehensive dataset of all MP names found in a 
  set of House of Commons division lists, in alphabetical order and
  with duplicates removed.
}
\usage{
collect.MPnames(divs, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{divs}{ A list, each component representng one House of Commons 
  division.  For the format see \code{\link{Divs0105}}.}
  \item{verbose}{ Logical.  If \code{TRUE}, progress is reported. }
}
\value{
  A data frame with two columns, \code{MPname} and \code{seatname}.
}
\references{
  Firth, D and Spirling, A (2003) Divisions of the United Kingdom House of 
  Commons, from 1992 to 2003 and beyond.  Submitted for publication.  At
  \url{http://www.warwick.ac.uk/go/tapir}.
}
\author{ David Firth }
\note{
This is a utility functon, for use in the construction of a 
dataset such as \code{\link{MPnames0105}}.  
}
\seealso{ \code{\link{uniquify.MPnames}} }
\examples{
data(Divs0105)
##  Collect all names from the first two divisions in June 2001:
collect.MPnames(Divs0105[1:2])
}
\keyword{ manip }
