\name{urn}
\alias{urn}
\alias{sampleu}
\alias{sample}
\alias{refill}
\alias{sample.default}
\alias{sample.urn}
\alias{refill.urn}
\title{ Repeated Sampling Without Replacement }
\description{ Generate repeated samples of the same list of objects without replacement. }

\usage{
u<-urn(items,prob=NULL)
s<-sample.urn(u,n,replace=TRUE)
s<-sample(x, size, ...) 
s<-sample.default(x, size, replace=FALSE, prob=NULL, ...) 
s<-sample.urn( x, size, replace=TRUE, prob=NULL, ...) 
size<-refill.urn(u)
size<-refill(u)
}

\arguments{
	\item{items}{
Items -- A set of items to be sampled. If 'items' is a list, calls to urn.sample
are treated identically to 'sample' except that (1) repeated calls to urn.sample sample without replacement from items and (2) the probability distribution is defined at urn creation time.

If 'items' is a vector, however, each item in the vector is interpreted
as the frequency of occurence of that type of item in the urn.  
	}
	\item{prob}{ Vector of probability weights corresponding to items. For use with
items lists only.}
	\item{u}{urn to be sampled}
	\item{n}{number of items in sample}
	\item{size}{number of items remaining}
	\item{s}{sample drawn from urn}
}

\details{
	\code{urn} creates an urn. \code{sampleu} allows a single sample to be taken without replacement. Call usaple when
repeated samples without replacement are needed. Use sum() to determine population
left in urn, and refill.urn to restore population to originial levels.

}
\references{
There are many references explaining sampling without replacement, this is one example:

\emph{Mathematical Statistics and Data Analysis}, John A. Rice. Wadsworth, 1988, 1995. 

}
\seealso{
  \code{\link[base]{sample}}
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

}

\author{
Micah Altman 
\email{Micah\_Altman\@harvard.edu}  
\url{http://maltman.hmdc.harvard.edu/}
}

\keyword{datagen}
\keyword{distribution}
