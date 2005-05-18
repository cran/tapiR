\name{write.csv}
\alias{write.csv}
\title{ Write a Data Frame in  
	.csv Format }
\description{
  Writes a data frame in \emph{comma-separated values} format, either to
  a file or to standard output.
}
\usage{
write.csv(x, file = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a data frame }
  \item{file}{ a file name, usually ending in \code{.csv} }
}
\details{
  If no file name is supplied, the results appear on standard output.
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
\note{ The inverse operation, to read in data from a \code{.csv} file, is 
	\code{mydata <- read.csv("myfile.csv", row.names = 1) } } 
\seealso{ \code{\link{read.csv}} }
\examples{
data(MPnames0105)
write.csv(MPnames0105)
}
\keyword{ IO }
\keyword{ file }
