.onLoad <- function(lib, pkg) {
   if((version$major<1) ||(version$minor<6 && version$major==1)) {
    stop("This version for R 1.6 or later")
  }
}

