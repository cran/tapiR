\name{find.divisions}
\alias{find.divisions}
\title{ Locate division lists for the UK House of Commons }
\description{
  Searches the online parliamentary record, Hansard, for the URLs of pages 
  containing division lists.  For each division found, returns the URL 
  and Hansard's textual tag indicating the topic under debate.
}
\usage{
find.divisions(session, dates)
}
\arguments{
  \item{session}{ A 4-digit identifier for the parliamentary session
  to be searched, e.g., 0203 for the year 2002-3. }
  \item{dates}{ A numeric vector of dates in the format yymmdd.  Ranges
  may be specified, e.g., 030515:030716 for the period May 15
  to July 16 2003. }
}
\details{
  Requires internet access.
}
\value{
	A two-column character matrix.  
	Columns are \code{debate} and \code{URL}.  
	Rows are indexed by division identifiers.
}
\references{ ~put references to the literature/web site here ~ }
\author{ David Firth }
\seealso{ \code{\link{get.divisions}} }
\examples{
\dontrun{
myURLs <- find.divisions(0203, 030715:030716)
myDivs <- get.divisions(myURLs)
}}
\keyword{ IO }
