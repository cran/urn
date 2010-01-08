\name{urn-package}
\alias{urn-package}
\docType{package}
\title{
A package for simulating draws from an urn -- sampling without replacement. 
}
\description{
	Generate repeated samples of the same list of objects without replacement.

}

\details{
\tabular{ll}{
Package: \tab urn\cr
Type: \tab Package\cr
Version: \tab 2.1 \cr
Date: \tab 2010-1-08 \cr
License: \tab AGPL 3.0 \cr
}


\code{urn} creates an urn. \code{sampleu} allows a single sample to be taken without replacement. Call usaple when
repeated samples without replacement are needed. Use sum() to determine population
left in urn, and refill.urn to restore population to originial levels.

}
\references{
There are many references explaining sampling without replacement, this is one example:

\emph{Mathematical Statistics and Data Analysis}, John A. Rice. Wadsworth, 1988, 1995. 

}
\seealso{
	\code{\link{sample}}, \code{\link{urn}}
}
\examples{

library(urn)

# Create urn with 3 items
u<-urn(list("red","green","blue"))

# custom print and summary methods
print(u )
summary(u)

# draw 2 samples from the urn
sample(u,2)
# can't sample more items than in the urn, without refilling:
# sampleu(u,2)
sum(u)
sample(u,1)
# refill
refill(u)  
# Create an urn with 100010 items of two types in ~51:49 proportions
ub<-urn(c(51006,49004))
summary(ub)


# take ten draws each of 10001 items
if (is.R()) {
	reps<-replicate(10, table(sample(ub,10001)), simplify=TRUE)
} else {
	reps<-sapply(integer(10), function(x)table(sample(ub,10001)), simplify=TRUE)
}
print(reps)

# should equal 51006
sum(reps[1,])
\dontrun{
  if (sum(reps[1,])!=51006) {
	warning("self check failed")
  }
}

}

\author{
Micah Altman 
\email{Micah\_Altman\@harvard.edu}  
\url{http://maltman.hmdc.harvard.edu/}
}

\keyword{datagen}
\keyword{distribution}
