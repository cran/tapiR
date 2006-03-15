\name{uniquify.MPnames}
\alias{uniquify.MPnames}
\title{ Put MP Names into a Unique Form }
\description{
  \emph{Hansard}, the official Parliamentary record, frequently 
  uses different forms for the name of the same MP.  This function
  applies both general rules, and \emph{ad hominem} rules contained 
  in the function supplied as argument \code{namerules}, to convert
  all names to a pre-determined form.
}
\usage{
uniquify.MPnames(ayes.or.noes, namerules = namerules.9203)
}
\arguments{
  \item{ayes.or.noes}{ A character vector of MP names, typically 
  either Ayes or Noes from a \emph{Hansard} division list.  }
  \item{namerules}{ A function implementing particularistic
  	rules by which certain names should be changed.  This function
  	takes two arguments, \code{MPnames} and \code{seatnames}, 
  	character vectors of the same length; it returns a 2-component list 
  	with components named \code{MPnames} and \code{seatnames}, these 
  	being the same vectors after modification by the rules. }
}
\value{
A character matrix, with one row for each element of \code{ayes.or.noes}.  
Two columns, \code{MPname} and \code{seatname}.
}
\author{ David Firth }
\note{
  This function would not normally be called by the user
  of the \emph{tapiR} package; 
  it is used internally by \code{\link{collect.MPnames}}
  and \code{\link{make.votesheet}}.
}
\examples{
uniquify.MPnames(c("Smith, Mr John (This Place)", 
"Jones, Dr Bill (Some Other Place)", "Spratt, Jack"))
}
\keyword{ manip }
