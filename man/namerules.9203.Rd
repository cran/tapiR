\name{namerules.9203}
\alias{namerules.9203}
\title{ Transform MP Names to a Unique Form }
\description{
  A function to implement rules for naming MPs and
  their constituencies unambiguously
}
\usage{
namerules.9203(MPnames, seatnames)
}
\arguments{
  \item{MPnames}{ Character vector }
  \item{seatnames}{ Character vector }
}
\value{
  A list, with components
  \item{MPnames }{A character vector}
  \item{seatnames }{A character vector}
  ...
}
\references{
  Firth, D and Spirling, A (2003) Divisions of the United Kingdom House of 
  Commons, from 1992 to 2003 and beyond.  Submitted for publication.  At
  \url{http://www.warwick.ac.uk/go/tapir}.
}
\author{ David Firth }
\note{ These are the rules that were used in constructing House of
  Commons voting datasets for the period 1992 to 2003.  They may need
  amendment for divisions recorded after May 2003, for example if a new
  member joins the House or if an MP becomes known by a new name. }
\examples{
namerules.9203(c("Bloggs, Joe", "Boswell, Timothy"), c("Deeside", "NA"))
}
\keyword{ manip }

