"urn.R"

# A simple urn for sampling without replacement, repeatedly
#
#    urn.R -- a library for sampling from urns
#    Copyright (C) 2003  Micah Altman
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
		return (null);
	}
	if (!is.vector(u)) {
		stop("u must be a vector")
		return (null);
	}
	if (sum(u<=0)>0) {
		stop("u must be >0");
	}
	tmp <-new.env();
	if (!is.null(prob) ) {
		if (!is.vector(prob)) {
			stop("Prob must be vector");
		} 
		if (length(prob) != length(u)) {
			stop("Probability vector must have same length as u");
		}
		if (!is.list(u)) {
			stop("Weights are only valid with list type.");
			return(null);
		}
		if (sum(prob<0)>=1) {
			stop("Weights must be non-negative");
			return(null);
		}
		if (sum(prob)==0) {
			stop("Weights sum to zero.")
			return(null);
		}
		assign("p", as.list(prob), envir=tmp);
		assign("pc", as.list(prob), envir=tmp);
	} 
	assign("current", u, envir=tmp);
	assign("initial", u, envir=tmp);
	ul<-list(tmp);
	class(ul)<-"urn";
	return(ul);
}

# print.urn (u)
#
# pretty printing function

print.urn<- function( x, ... ) {
	if (is.list(get("current", envir=x[[1]]))) {
		cat ("List type: \n");
	} else {
		cat ("Vector type: \n");
	}
       cat(	paste("current: ", 
			paste(get("current", envir=x[[1]]), collapse=", ")
		, "\n") 
  	);

	cat(	paste("initial: ", 
			paste(get("initial", envir=x[[1]]), collapse=", ")
		, "\n") 
  	);
	if (exists("p", envir=u[[1]], inherits=FALSE)) {
	   cat(	paste("probability weights: ", 
			paste(get("p", envir=x[[1]]), collapse=", ")
		, "\n") 
  	   );
	}

}


sampleuList<-function(u,size) {
	tmpc = get("current", envir=u[[1]]);
	s = seq(1,length(tmpc));

	
	if (!exists("p", envir=u[[1]], inherits=FALSE)) {
		ind = sample(s,size);
	} else {
		p = get("pc", envir=u[[1]]);
		ind = sample(s,size,prob=p);
		p[ind]=NULL;
		assign("pc", p, envir=u[[1]]);
	}
	r = tmpc[ind];
	tmpc[ind]=NULL;
	assign("current", tmpc, envir=u[[1]]);
	return(r);	
}


sampleuVector<-function(u,size) {

	s = vector(mode="numeric",size);
	
	# for performance, do these outside of loop
	tmpc = get("current", envir=u[[1]]);
	n = sum(tmpc); 
	r=runif(size); 
	
	for (i in 1:size) {
		cp = 0;
		for (j in 1:length(tmpc)) {
			rn = r[i]*n;
			cp = cp + tmpc[j];
			if (rn<=cp) {
				if (rn>0) {
					s[i]=j;
					n=n-1;
					tmpc[j]=tmpc[j]-1;
					break;
				}
			}
		}
	}	

	assign("current", tmpc, envir=u[[1]]);
	return(s);
}


# sum.urn(u)
#
# u - urn
#
# returns number of balls remaining in urn

sum.urn<-function(u,...,na.rm)  {
	tmp<- get("current", envir=u[[1]]);
	if (is.list(tmp)) {
		return (length(tmp)+sum(...));
	} else {

		return(sum(tmp)+sum(...));
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
	assign("current", get("initial", envir=u[[1]]), envir=u[[1]]);	
	if (exists("pc", envir=u[[1]], inherits=FALSE)) {
		 assign("pc", get("p", envir=u[[1]]), envir=u[[1]]);
	}
	return(sum(u));
}

# summary.urn(u)
#
# u - urn
#
summary.urn<-function(object, ...) {
	if (is.list(get("current", envir=object[[1]]))) {
		return(table(as.vector(get("current", envir=object[[1]]),mode="numeric")));
	} else {
		return(as.table(as.vector(get("current", envir=object[[1]]),
		mode="numeric")));
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
		size<-1;
	}
	if (size>sum(u)) {
		stop("Can't take a sample larger than the remaining population");
	}
	if (is.list(get("current", envir=u[[1]]))) {
		return(sampleuList(u,size));
	} else {
		return(sampleuVector(u,size));
	}
}


