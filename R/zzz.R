
# Package Load methods


#
# This gets run by R, but not S-Plus, wich doesn't use .onLoad
#

.onLoad <- function(lib, pkg) {
  if (is.R()) {
    try(print(utils::citation("urn")),silent=FALSE)
  } 
  return(TRUE)
}

# These .on.attach and .on.detach are used in Splus

.on.attach <- function(lib, pkg) {
  if (!is.R()) {
    mrequire<-require
    if (mrequire(pkgutils,quietly=TRUE)){
      print(citation("urn"))
    } else  {
      cat("To cite the Urn package in publications use:\n\n",
       paste("Altman, M., (2007)", "(Software version: R Package, Urn, version 2.1)\n\n")
      )
    }
  }
  return(TRUE)
}

.on.detach<-function(...) {
  if(!is.R()) {
	if (exists(".urn",where=0)) {
    		remove(".urn",where=0)
	}
  }
}

