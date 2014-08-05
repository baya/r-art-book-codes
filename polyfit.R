polyfit <- function(y, x, maxdeg){
  pwrs <- powers(x, maxdeg)
  lmout <- list()
  class(lmout) <- "polyreg"
  for(i in 1:maxdeg) {
    lmo <- lm(y ~ pwrs[,1:i])
    lmo$fitted.cvvalues <- lvoneout(y, pwrs[,1:i, drop=F])
    lmout[[i]] <- lmo
  }
  lmout$x <- x
  lmout$y <- y
  
  return(lmout)
}

print.polyreg <- function(fits){
  maxdeg <- length(fits) - 2
  n <- length(fit$y)
  tbl <- matrix(nrow=maxdeg, ncol=1)
  colname(tbl) <- "MSPE"
  for(i in 1:maxdeg){
    fi <- fits[[i]]
    errs <- fits$y - fi$fitted.cvvalues
    spe <- crossprod(errs, errs)
    tbl[i,1] <- spe/n
  }
  
  cat("mean squared prediction errors, by degree\n")
  print(tbl)
    
}

powers <- function(x, dg){
  pw <- matrix(x, nrow=length(x))
  prod <- x
  for(i in 2:dg){
    prod <- prod * x
    pw <- cbind(pw, prod)
  }
  return(pw)
}

