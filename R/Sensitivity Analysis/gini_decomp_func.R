######### Functions needed to decompose edagger and life expectancy by age

AKm02a0 <- function(m0, sex = "m"){
  sex <- rep(sex, length(m0))
  ifelse(sex == "m", 
         ifelse(m0 < .0230, {0.14929 - 1.99545 * m0},
                ifelse(m0 < 0.08307, {0.02832 + 3.26201 * m0},.29915)),
         # f
         ifelse(m0 < 0.01724, {0.14903 - 2.05527 * m0},
                ifelse(m0 < 0.06891, {0.04667 + 3.88089 * m0}, 0.31411))
  )
}


life.expectancy <- compiler::cmpfun(function(mx,sex = "f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[1]
})


DecompContinuousOrig <-function (func, rates1, rates2, N, ...) 
{
  y1 <- func(rates1,...)
  y2 <- func(rates2,...)
  d <- rates2 - rates1
  n <- length(rates1)
  delta <- d/N
  x <- rates1 + d * matrix(rep(0.5:(N - 0.5)/N, length(rates1)), 
                           byrow = TRUE, ncol = N)
  cc <- matrix(0, nrow = n, ncol = N)
  for (j in 1:N) {
    for (i in 1:n) {
      z <- rep(0, n)
      z[i] <- delta[i]/2
      cc[i, j] <- func((x[, j] + z), ...) - func((x[, j] - 
                                                    z), ...)
    }
  }
  return(rowSums(cc))
}


Decomp   <- function(func, rates1, rates2, N=20,sex="f"){
  Results     <- DecompContinuousOrig(func=func, rates1=rates1 , rates2=rates2, N=N,sex=sex)
  return(Results)
}


edag.function <- function(mx,sex="f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  l <- length(ex)
  ed <- (sum(dx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
  ed
}

gini.func <- function(mx,sex="f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  cx        <- ax-1/2
  lx.1      <- c(lx[-1],0)
  lx.1.sq   <- lx.1^2
  ax.hat    <- (1-(2/3)*qx+cx*(2-qx-(6/5)*cx))/(2-qx)
  ax.hat[1] <- ax[1]*(1-qx[1]*(3+0.831*ax[1])/(2+qx[1]))  
  count     <- lx.1.sq + ax.hat*(lx^2 - lx.1.sq)     
  gini      <- 1-sum(count)/(ex[1]*(lx[1])^2)  
  return(gini)
}

#################################### Function to decompose 5 years age-groups
edag.function5 <- function(mx,sex="f"){
  Ages   <- c(0,1,seq(5,110,5))
  Widths <- diff(Ages)
  N      <- length(mx)
  RADIX     <- 1
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  Widths <-  c(Widths, Widths[N - 1])
  ax        <- mx * 0 + 2.5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  ax[2]     <-2
  qx        <- (Widths*mx) / (1 + (Widths - ax) * mx)
  qx[i.openage]       <- 1
  qx[qx > 1] 	        <- 1
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))[1:N]
  dx 				    <-  c(-diff(lx),lx[N])
  Lx 	                <- c(Widths[1:(N - 1)] * lx[2:N] + ax[1:(N - 1)] * dx[1:(N - 1)], lx[N] * ax[N])
  Lx[is.infinite(Lx)] <- 1
  Lx[is.na(Lx)] 	    <- 0
  Tx 				    <- rev(cumsum(rev(Lx)))
  ex 				    <- Tx / lx
  ex[is.na(ex)] 	    <- 0
  l <- length(ex)
  ed <- (sum(dx[-l]* (ex[-l] + ax[-l]/Widths[-l]*(ex[-1]-ex[-l]))) + ex[l])
  ed
}

edfrommxc            <- function(mxcvec,sex="f"){
  dim(mxcvec) <- c(24,length(mxcvec)/24)
  mx          <- rowSums(mxcvec)
  edag.function5(mx,sex)
}


DecompContinuous_ed_5 <-function (rates1, rates2, N,sex) 
{ 
  #m <- (ac+ad)/2
  d <- rates2 - rates1
  n <- length(rates1)
  delta <- d/N
  x <- rates1 + d * matrix(rep(0.5:(N - 0.5)/N, length(rates1)), 
                           byrow = TRUE, ncol = N)
  cc <- matrix(0, nrow = n, ncol = N)
  for (j in 1:N) {
    for (i in 1:n) {
      z        <- rep(0, n)
      z[i]     <- delta[i]/2
      cc[i, j] <- edfrommxc(mxcvec=(x[, j] + z),sex=sex) - edfrommxc(mxcvec=(x[, j] - 
                                                   z),sex=sex)
    }
  }
  Results <- rowSums(cc)
  return(Results)
}
