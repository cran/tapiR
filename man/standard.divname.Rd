\name{standard.divname}
\alias{standard.divname}
\title{ Convert Hansard's Online Division Identifiers to a Standard Format }
\description{
  \emph{Hansard}, the official Parliamentary record, has a system of 
  identifiers for divisions of the House.  This function converts
  the Hansard identifier to one in the format \code{div***.yymmdd},
  where \code{***} is the 3-digit division number and \code{yymmdd}
  the date on which the division took place.
}
\usage{
standard.divname(div.id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{div.id}{ a character vector containing one or more division 
  identifiers as used in the online \emph{Hansard} }
}
\value{
  A character vector.
}
\author{ David Firth }
\references{
  Firth, D and Spirling, A (2003) Divisions of the United Kingdom House of 
  Commons, from 1992 to 2003 and beyond.  Submitted for publication.  At
  \url{http://www.warwick.ac.uk/go/tapir}.
}
\examples{
##  For division number 1 on 26 June 2001:
standard.divname("10626-30_div1")
}
\keyword{ manip }
