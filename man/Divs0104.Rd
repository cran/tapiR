\name{Divs0104}
\alias{Divs0104}
\docType{data}
\title{ UK House of Commons Division Lists, 2001-2004 }
\description{
  A list, with one element for each parliamentary division in the
  second New Labour term of office.  Not complete until that term of office
  ends!  The names of those voting aye and no, as they appear in Hansard but 
  with spelling and typographical errors corrected.
}
\usage{data(Divs0104)}
\format{
	A list, each component of which corresponds to one House of Commons 
	division.   Each division is 
	itself a list, with three components: 
	\describe{
		\item{
		\code{url}}{the URL for the Hansard page containing 
		the division list.
		Character.}
		\item{
		\code{ayes}}{Character vector, the names of those voting AYE.
		}
		\item{
		\code{noes}}{Character vector, the names of those voting NO.
		}
	}
}
\details{
  Some corrections have been made (e.g., spellings) to the original 
  Hansard lists.
}
\source{
  Hansard online.  The URL is supplied for each division.
}
\references{
  Firth, D and Spirling, A (2004) Divisions of the United Kingdom House of 
  Commons, from 1992 to 2004 and beyond.  Submitted for publication.  At
  \url{http://www.warwick.ac.uk/go/tapir}.
}
\examples{
data(Divs0104)
}
\keyword{datasets}
