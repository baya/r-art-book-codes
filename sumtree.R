
sumtree <- function(drtr){
  tot <- 0
  fls <- dir(drtr, recursive=TRUE)
  for(f in fls){
    f <- file.path(drtr, f)
    if(!file.info(f)$isdir){
      tot <- tot + sum(scan(f, quiet=TRUE))
    }
  }
  
  return(tot)
}