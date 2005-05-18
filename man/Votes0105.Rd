\name{Votes0105}
\alias{Votes0105}
\docType{data}
\title{ Voting on House of Commons Divisions between June 2001 and 6 April 2005}
\description{
  A data frame
}
\usage{data(Votes0105)}
\format{
  The votes are organized as a data frame data frame with 657 
  observations (MPs) on the following 1247
  variables (1246 of which are divisions of the House).
  \describe{
    \item{\code{party}}{a factor with levels \code{Con} \code{DUP} \code{Ind} \code{IndCon} \code{LD} \code{Lab} \code{PC} \code{SDLP} \code{SNP} \code{UU}}
    \item{\code{div001.010626}}{a factor with levels \code{-} \code{n} \code{y}}
    \item{...}{1245 further such variables, ending with}
    \item{\code{div155.050406}}{a factor with levels \code{-} \code{n} \code{y}}
  }
}
\details{
  Some divisions have the additional factor level \code{b}, indicating 
  that one or more MPs voted both Aye and No.
}
\source{
  Derived, using \code{\link{get.divisions}}, from the online 
  pages of the official Parliamentary record \emph{Hansard}.
}
\note{
Corresponding data for the 1992-1997 and 1997-2001 parliaments can be found at 
\url{http://www.warwick.ac.uk/go/tapir}
}
\seealso{\code{\link{Topics0105}}}
\references{
  Firth, D and Spirling, A (2003) Divisions of the United Kingdom House of 
  Commons, from 1992 to 2003 and beyond.  Submitted for publication.  At
  \url{http://www.warwick.ac.uk/go/tapir}.
}
\examples{
data(Votes0105)
}
\keyword{datasets}
