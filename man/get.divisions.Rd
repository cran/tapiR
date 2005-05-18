\name{get.divisions}
\alias{get.divisions}
\title{Fetch House of Commons Division Lists from a File or from the Web}
\description{
  Visits a list of URLs and extracts division lists in the standard format
  used by \emph{Hansard} online.
}
\usage{
get.divisions(URLs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{URLs}{ A character vector of URLs (uniform resource 
     locators).  These may be local files or \emph{Hansard} web pages. }
}
\details{
  A single URL may point to a file or page containing 
  more than one division list.
}
\value{
  A list, with one component for each division list found.  
  For the format see \code{\link{Divs0105}}.
}
\references{
  Firth, D and Spirling, A (2003) Divisions of the United Kingdom House of 
  Commons, from 1992 to 2003 and beyond.  Submitted for publication.  At
  \url{http://www.warwick.ac.uk/go/tapir}.
}
\author{ David Firth }

\examples{
\dontrun{
##  Get the first two divisions for the session starting in June 2001:
myURLs <- c(
"http://www.parliament.the-stationery-office.co.uk/pa/cm200102/cmhansrd/vo010626/debtext/10626-30.htm", 
"http://www.parliament.the-stationery-office.co.uk/pa/cm200102/cmhansrd/vo010627/debtext/10627-29.htm")
first.two.divs <- get.divisions(myURLs)
}
}
\keyword{ IO }
\keyword{ manip }
