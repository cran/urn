\name{urn}
\alias{urn}
\alias{sampleu}
\alias{refill.urn}
\title{ Repeated Sampling Without Replacement }
\description{ Generate repeated samples of the same list of objects without replacement. }

\usage{
u<-urn(items,prob=NULL)
s<-sampleu(u,n)
size<-refill.urn(u)
}

\arguments{
	\item{items}{
Items -- A set of items to be sampled. If 'items' is a list, calls to urn.sample
are treated identically to 'sample' except that  $\left( 1 \right)$

repeated calls to urn.sample sample without replacement from items
$\left( 2 \right)$ the probability distribution is defined at urn creation time.

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
sample() allows a single sample to be taken without replacement. Call urn.sample when
repeated samples without replacement are needed. Use sum() to determine population
left in urn, and refill.urn to restore population to originial levels.

}
\references{
There are many references explaining sampling without replacement, this is one example:

\emph{Mathematical Statistics and Data Analysis}, John A. Rice. Wadsworth, 1988, 1995. 

}
\seealso{
  \code{\link{sample}}
}
\examples{

library(urn)

# Create urn with 3 items
u<-urn(list("red","green","blue"))

# custom print and summary methods
print(u )
summary(u)

# draw 2 samples from the urn
sampleu(u,2)
# can't sample more items than in the urn, without refilling:
# sampleu(u,2)
sum(u)
sampleu(u,1)
# refill
refill.urn(u)  
# Create an urn with 100010 items of two types in ~51:49 proportions
ub<-urn(c(51006,49004))
summary(ub)


# take ten draws each of 10001 items
if (is.R()) {
	reps<-replicate(10, table(sampleu(ub,10001)), simplify=TRUE)
} else {
	reps<-sapply(intger(10), function(x)table(sampleu(ub,10001)), simplify=TRUE)
}
print(reps)

# should equal 51006
sum(reps[1,])

}

\author{
Micah Altman 
\email{Micah\_Altman\@harvard.edu}  
\url{http://www.hmdc.harvard.edu/micah_altman/}
}

\keyword{datagen}
\keyword{distribution}
