\name{MPnames0103}
\alias{MPnames0103}
\docType{data}
\title{ Members of the House of Commons, 2001-2003 }
\description{
  A data frame containing MP name, abbreviated constituency 
  name where used, and party affiliation.
}
\usage{data(MPnames0103)}
\format{
  A data frame with 653 observations on the following 3 variables.
  \describe{
    \item{\code{MPname}}{a character vector, names of MPs}
    \item{\code{seatname}}{a character vector, abbreviated constituency name where used}
    \item{\code{party}}{a factor with levels \code{Con} \code{DUP} \code{Ind} \code{IndCon} \code{LD} \code{Lab} \code{PC} \code{SDLP} \code{SNP} \code{UU}}
  }
}
\references{
  Firth, D and Spirling, A (2003) Divisions of the United Kingdom House of 
  Commons, from 1992 to 2003 and beyond.  Submitted for publication.  At
  \url{http://www.stats.ox.ac.uk/~firth/tapiR/}.
}
\examples{
data(MPnames0103)
}
\keyword{datasets}
