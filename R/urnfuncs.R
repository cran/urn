"urn.R"

# A simple urn for sampling without replacement, repeatedly
#
#    urn.R -- a library for sampling from urns
#    Copyright (C) 2003-6 Micah Altman
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


# urn(v)
#
# Create new urn
#
# v - vector of positive scalars, representing the initial
#	number of each type of 'balls' in the urn

urn<-function  (u, prob=NULL) {
	if (is.null(u)) {
		stop("Argument items missing with no default")
		return (NULL)
	}
	if (!is.vector(u)) {
		stop("u must be a vector")
		return (NULL)
	}
	if (!is.list(u) && sum(u<=0)>0) {
		stop("all elements of a numeric vector specification must be >0")
	}
	if (!is.null(prob) ) {
		if (!is.vector(prob)) {
			stop("Prob must be vector")
		} 
		if (length(prob) != length(u)) {
			stop("Probability vector must have same length as u")
		}
		if (!is.list(u)) {
			stop("Weights are only valid with list type.")
			return(NULL)
		}
		if (sum(prob<0)>=1) {
			stop("Weights must be non-negative")
			return(NULL)
		}
		if (sum(prob)==0) {
			stop("Weights sum to zero.")
			return(NULL)
		}

	}
	if (is.R()) {
	 urnID <-new.env() 
  } else {
    if(!exists(".urn",where=0)) {
      assign(".urn",list(),where=0)
    }
    tmplist<-get(".urn",where=0)
    urnID<-length(tmplist)+1
    tmplist[[urnID]]= list()
    assign(".urn",tmplist,where=0)
  }
  ul<-list(urnID)
	class(ul)<-"urn"
	if (!is.null(prob)) {
   		urnAssign(ul,"p", as.list(prob))
	 	urnAssign(ul,"pc", as.list(prob))
	}
	 urnAssign(ul,"current", u)
	 urnAssign(ul,"initial", u)
  return(ul)
}

###
### Accessor methods...
### 
### Why do I do this?  So that it works in S-Plus and R as
### cleanly as possible.
###
### In order to make urn most useful, the interface should
### allow one to "draw" from an urn. This changes the state
### of the urn. 
###
### Given R/S-Plus's orientation to pass-by-value, there were a few
### possible approaches:
###        1. two steps, explicitly replacing the object:
###             u<-sampleu(u); sample<-get_sample(u)
###        
###         This is awkward.
###
###       2. store state in the user's environment.
###       I take this approach in S-plus, for lack of a better one.
###
###       3. Store state in a new environment
###       Currently I use this with R. It works.
### 
###       4. Store state in a closure in the module's environment
###       This is cleaner than 3. I will eventually do this


urnAssign<-function(self,memberName,memberValue) {
  	if (is.R()) {  
      assign(memberName, memberValue, envir=self[[1]])
    }  else {
        urnID = self[[1]]
        tmplist<-get(".urn",where=0)
        tmpUrn<-tmplist[[urnID]]
        tmpUrn[[memberName]]=memberValue
        tmplist[[urnID]]=tmpUrn
        assign(".urn",tmplist,where=0)
    }
}

urnGet<-function(self,memberName) {
  	if (is.R()) {  
      get(memberName, envir=self[[1]])
    } else {
        urnID = self[[1]]
        tmplist<-get(".urn",where=0)
        tmpUrn<-tmplist[[urnID]]
        tmpUrn[[memberName]]
    }
}

urnExists<-function(self,memberName) {
  	if (is.R()) {  
      exists(memberName, envir=self[[1]],inherits=FALSE)
    } else {
        urnID = self[[1]]
        tmplist<-get(".urn",where=0)
        tmpUrn<-tmplist[[urnID]]
        is.null(tmpUrn[[memberName]])
    }  
}



# print.urn (u)
#
# pretty printing function

print.urn<- function( x, ... ) {
	if (is.list(urnGet(x,"current"))) {
		cat ("List type: \n")
	} else {
		cat ("Vector type: \n")
	}
       cat(	paste("current: ", 
			paste(urnGet(x,"current"), collapse=", ")
		, "\n") 
  	)

	cat(	paste("initial: ", 
			paste(urnGet(x,"initial"), collapse=", ")
		, "\n") 
  	)
	if (urnExists(x,"p")) {
	   cat(	paste("probability weights: ", 
			paste(urnGet(x,"p"), collapse=", ")
		, "\n") 
  	   )
	}

}

sampleuList<-function(u,size) {
	tmpc = urnGet(u,"current")
	s = seq(1,length(tmpc))

	
	if (!urnExists(u,"p")) {
		ind = sample(s,size)
	} else {
		p = urnGet(u,"pc")
		ind = sample(s,size,prob=p)
		p=p[setdiff(1:length(p),ind)]
		urnAssign(u,"pc", p)
	}
	r = tmpc[ind]
	tmpc=tmpc[setdiff(1:length(tmpc),ind)]
	urnAssign(u,"current",tmpc)
	return(r);	
}


sampleuVector<-function(u,size) {

	s = vector(mode="numeric",size)
	
	# for performance, do these outside of loop
	tmpc = urnGet(u,"current")
	n = sum(tmpc); 
	r=runif(size); 
	
	for (i in 1:size) {
		cp = 0
		for (j in 1:length(tmpc)) {
			rn = r[i]*n
			cp = cp + tmpc[j]
			if (rn<=cp) {
				if (rn>0) {
					s[i]=j
					n=n-1
					tmpc[j]=tmpc[j]-1
					break
				}
			}
		}
	}	

	urnAssign(u,"current", tmpc)
	return(s)
}


# sum.urn(u)
#
# u - urn
#
# returns number of balls remaining in urn

sum.urn<-function(u,...,na.rm=FALSE)  {
	tmp<- urnGet(u,"current")
	partialSum=0
	if (length(list(...))>0){
	   partialSum=sum(...,na.rm)
  }
	if (is.list(tmp)) {
		return (length(tmp)+partialSum)
	} else {
		return(sum(tmp)+partialSum)
	}

}

# refill.urn(u)
#
# u - urn
#
# refills urn, restoring initial distribution of balls


refill.urn<-function(u) {
	if (!inherits(u,"urn")) {
		stop("requires an urn")
	}
	urnAssign(u,"current", urnGet(u, "initial"))
	if (urnExists(u, "pc")) {
		 urnAssign(u,"pc", urnGet(u,"p"))
	}
	return(sum(u))
}

# summary.urn(u)
#
# u - urn
#
summary.urn<-function(object, ...) {
	tmpCurrent= urnGet(object,"current")
	if (is.list(tmpCurrent)) {
		return(table(unlist(tmpCurrent)))
	} else {
		# coerce to table by hand, since SPLUS doesn't have "as.table"
		tt = table(tmpCurrent)
		tt[] = as.numeric(tmpCurrent)
		names(tt)= 1:length(tt)
		return(tt)
	}

}
	
# sampleu (u,size)
#
# u - urn to sample from
# size - number of balls to draw as a sample
# 
# Returns a vector, representing the number of balls of
# each type drawn from the urn 

sampleu<- function(u, size) {
	
	if (!inherits(u,"urn")) {
		stop("requires an urn")
	}
	if(missing(size)) {	
		size<-1
	}
	if (size>sum(u)) {
		stop("Can't take a sample larger than the remaining population")
	}
	if (is.list(urnGet(u,"current"))) {
		return(sampleuList(u,size))
	} else {
		return(sampleuVector(u,size))
	}
}



