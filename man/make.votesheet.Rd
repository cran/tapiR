\name{make.votesheet}
\alias{make.votesheet}
\title{ Make a Rectangular Dataset from House of Commons Division Data }
\description{
  Takes one or more House of Commons division lists and records the votes
  in a data frame with one row for each MP and one column for each division.
}
\usage{
make.votesheet(divs, MPnames, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{divs}{ A list, with one component for each division.  Each division
  	is itself a list, with three components \code{url}, 
  	\code{ayes} and \code{noes}. }
  \item{MPnames}{ A data frame of unique MP names, seatnames and parties. }
  \item{verbose}{ Logical.  If \code{TRUE}, progress is reported 
  	on standard output. }
}
\value{
  A data frame.  See \code{\link{Votes0105}} for the format.
}
\references{
  Firth, D and Spirling, A (2003) Divisions of the United Kingdom House of 
  Commons, from 1992 to 2003 and beyond.  Submitted for publication.  At
  \url{http://www.warwick.ac.uk/go/tapir}.
}
\author{ David Firth }
\seealso{ \code{\link{write.votesheet}} }
\examples{
data(Divs0105)
data(MPnames0105)
##  Do the first three divisions in June 2001:
make.votesheet(Divs0105[1:3], MPnames0105)
}
\keyword{ manip }
