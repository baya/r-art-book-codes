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
  n <- length(fits$y)
  tbl <- matrix(nrow=maxdeg, ncol=1)
  colnames(tbl) <- "MSPE"
  for(i in 1:maxdeg){
    fi <- fits[[i]]
    errs <- fits$y - fi$fitted.cvvalues
    spe <- crossprod(errs, errs)
    tbl[i,1] <- spe/n
  }
  
  cat("mean squared prediction errors, by degree\n")
  print(tbl)
    
}

plot.polyreg <- function(fits){
  plot(fits$x, fits$y, xlab="X", ylab="Y")
  maxdg <- length(fits) - 2
  cols <- c("red", "green", "blue")
  dg <- curvecount <- 1
  while(dg < maxdg){
    prompt <- paste("RETURN for XV fit for degree", dg, "or type degree", "or q for quit")
    rl <- readline(prompt)
    dg <- if(rl == "") dg else if (rl != "q") as.integer(rl) else break
    lines(fits$x, fits[[dg]]$fitted.values, col=cols[curvecount%%3 + 1])
    dg <- dg + 1
    curvecount <- curvecount + 1
  }
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

lvoneout <- function(y, xmat){
  n <- length(y)
  predy <- vector(length=n)
  for(i in 1:n){
    lmo <- lm(y[-i] ~ xmat[-i,])
    betahat <- as.vector(lmo$coef)
    predy[i] <- betahat %*% c(1, xmat[i,])
  }
  
  return(predy)
  
}

poly <- function(x, cfs){
  val <- cfs[1]
  prod <- 1
  dg <- length(cfs) - 1
  for(i in 1:dg){
    prod <- prod * x
    val <- val + cfs[i+1] * prod
  }
}
