\name{Votes0103}
\alias{Votes0103}
\docType{data}
\title{ Voting on House of Commons Divisions between June 2001 and May 2003}
\description{
  A data frame
}
\usage{data(Votes0103)}
\format{
  A data frame with 653 observations (MPs) on the following 543 
  variables (542 of which are divisions of the House).
  \describe{
    \item{\code{party}}{a factor with levels \code{Con} \code{DUP} \code{Ind} \code{IndCon} \code{LD} \code{Lab} \code{PC} \code{SDLP} \code{SNP} \code{UU}}
    \item{\code{div001.010626}}{a factor with levels \code{-} \code{n} \code{y}}
    \item{...}{540 further such variables, then finally}
    \item{\code{div181.030508}}{a factor with levels \code{-} \code{n} \code{y}}
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
\url{http://www.stats.ox.ac.uk/~firth/tapiR/}
}
\references{
  Firth, D and Spirling, A (2003) Divisions of the United Kingdom House of 
  Commons, from 1992 to 2003 and beyond.  Submitted for publication.  At
  \url{http://www.stats.ox.ac.uk/~firth/tapiR/}.
}
\examples{
data(Votes0103)
}
\keyword{datasets}
