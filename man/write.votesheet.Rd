\name{write.votesheet}
\alias{write.votesheet}
\title{Output House of Commons Voting Data Compactly}
\description{
  Writes out a data frame of House of Commons voting data, either 
  to a text file or to standard output.
}
\usage{
write.votesheet(x, file = NULL, keep.b = TRUE, 
	aye = "y", no = "n", novote = "-")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame of the kind made by 
  	\code{\link{make.votesheet}} }
  \item{file}{ An optional file name for the printed representation.
  	If \code{NULL}, printing is to standard output.}
  \item{keep.b}{ Should instances of \code{b} in the voting data,
  	meaning that an MP was recorded as having voted both Aye and No,
  	be retained in the output?  If \code{FALSE}, then \code{b} is
  	converted everywhere to \code{-}.}
  \item{aye}{ A one-character code for an Aye vote. }
  \item{no}{ A one-character code for a No vote. }
  \item{novote}{ A one-character code for neither Aye nor No 
      (i.e., a vote was not recorded) }
}
\value{
	\code{invisible(NULL)}
}
\references{
  Firth, D and Spirling, A (2003) Divisions of the United Kingdom House of 
  Commons, from 1992 to 2003 and beyond.  Submitted for publication.  At
  \url{http://www.warwick.ac.uk/go/tapir}.
}
\author{ David Firth }
\note{ Codes used as input to Keith Poole's OC (optimal classification) program are 
\code{aye = "1", no = "6", novote = "0"} }

\examples{
data(Votes0105)
write.votesheet(Votes0105[1:10, 1:10])
}
\keyword{ IO }
\keyword{ file }
