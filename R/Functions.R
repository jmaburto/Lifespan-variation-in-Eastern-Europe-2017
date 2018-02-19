
life.expectancy.frommx.5 <- compiler::cmpfun(function(mx,sex = "f"){
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
  ex[6]
})

edag.function.frommx.5 <- compiler::cmpfun(function(mx,sex="f"){
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
  l             <- length(ex)
  
  dx.lx         <- dx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]))+dx[l]*ex[l]
  ed            <- c(rev(cumsum(rev(dx.lx))),dx[l]*ex[l])/lx + ex[l]
  ed[6]
})

  

e.dagger.LT.5X <- function(fx,ex,ax=ax,lx){
  l <- length(ex)
  dx.lx         <- fx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]))+fx[l]*ex[l]
  ed            <- c(rev(cumsum(rev(dx.lx))),fx[l]*ex[l])/lx + ex[l]
  ed[6]
}  


### Functions to reproduce results from Aburto & van Raalte, 2017
labels.cause <- c('Attributable to alcohol','IHD','Stroke','Transportation accidents',
                  'Other external causes','Infectious & respiratory diseases','Cancers',
                  'Other Circulatory','Birth conditions','Rest')

Labels.age      <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                     '40-44','45-49','50-54','55-59','60-64','65-69',
                     "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105-109","110+")
Labels.age2      <- c('0','1-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                     '40-44','45-49','50-54','55-59','60-64','65-69',
                     "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105-109","110+")


Period.labels <- c("Stagnation 1960-1980","Improvements 1980-1988","Deterioration 1988-1994",
                "Divergence 1994-2000","Convergence 2000-present")

get.mat.fun <- function(j,i,k,data=Mx.Cause){
  D1 <- data[data$year==j & data$sex==i & data$country==k,]
  D1 <- acast(D1, age ~ cause,value.var = 'mxc')
  D1
}

cause.decomp.shape <- function(func = edfrommxc5, rates1 = c(get.mat.fun(j=j,i=i,k=k,data=Mx.Cause)),
                               rates2 = c(get.mat.fun(j=j+1,i=i,k=k,data=Mx.Cause)), N=100, sex=Sx,dime=dime,k2=k,j2=j,ind='ed'){
  c.vec      <- Decomp(func = func, rates1 = rates1,rates2 = rates2, N= N, sex = sex)
  dim(c.vec) <- dime
  c.mat      <- melt(c.vec, varnames = c('age','cause'))
  c.mat$sex  <- sex
  c.mat$year <- j2
  c.mat$country <- k2
  c.mat$ind   <- ind
  c.mat
}


get.prop.fun <- function(Data){
    total      <- length(Data$Category2)
    cuadr1     <- length(Data[Data$Category2 == 1,]$Category2)/total
    cuadr2     <- length(Data[Data$Category2 == 2,]$Category2)/total
    cuadr3     <- length(Data[Data$Category2 == 3,]$Category2)/total
    cuadr4     <- length(Data[Data$Category2 == 4,]$Category2)/total
    
    S.E1       <- sqrt(cuadr1*(1-cuadr1)/total)
    S.E2       <- sqrt(cuadr2*(1-cuadr2)/total)
    S.E3       <- sqrt(cuadr3*(1-cuadr3)/total)
    S.E4       <- sqrt(cuadr4*(1-cuadr4)/total)
    
    z          <- qnorm(p = .975,mean = 0,sd = 1)       
    
    prop.cuadr1       <- paste0(round(cuadr1*100,2), ', CI[',round((cuadr1 - z*S.E1)*100,2),',',round((cuadr1 + z*S.E1)*100,2),']')
    prop.cuadr2       <- paste0(round(cuadr2*100,2), ', CI[',round((cuadr2 - z*S.E2)*100,2),',',round((cuadr2 + z*S.E2)*100,2),']')
    prop.cuadr3       <- paste0(round(cuadr3*100,2), ', CI[',round((cuadr3 - z*S.E3)*100,2),',',round((cuadr3 + z*S.E3)*100,2),']')
    prop.cuadr4       <- paste0(round(cuadr4*100,2), ', CI[',round((cuadr4 - z*S.E4)*100,2),',',round((cuadr4 + z*S.E4)*100,2),']')
    
    
    out <- NULL
    out$prop.cuadr1        <- prop.cuadr1
    out$prop.cuadr2        <- prop.cuadr2
    out$prop.cuadr3        <- prop.cuadr3
    out$prop.cuadr4        <- prop.cuadr4
    
    out
  }

get.dif.fun <- function(f, relative = 1){
  #change relative = 2 to get relative changes
  y     <- ifelse(relative==1,1,f[-length(f)])
  dif.f <- diff(f,lag = 1)/y
  dif.f
}


e.dagger.LT.5 <- function(fx,ex,ax=ax,lx){
  l <- length(ex)
  v <- (sum(fx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])/lx[1]
  return(v)         
}  
  
e.dagger.LT <- function(fx,ex,ax=ax){
  l <- length(ex)
  v <- (sum(fx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
  return(v)         
}

Country.HMD.vec        <- c("BLR","BGR","CZE","HUN","POL","RUS","SVK","UKR","SVN","EST","LVA","LTU")
Country.name.vec       <- c("Belarus","Bulgaria","Czech Republic","Hungary","Poland","Russia",
                            "Slovakia","Ukraine","Slovenia","Estonia","Latvia","Lithuania")
names(Country.name.vec) <- Country.HMD.vec

Sexes        <- c('Female','Male')

reshape.data.1   <- function(sex,Data,Country.name.vec){
  Counts         <- dcast(Data, Year+Age+PopName ~ Type, value.var = sex)
  Counts$sex     <- sex
  Counts$mx      <- Counts$Deaths/Counts$Exposures
  Counts$country <- Country.name.vec[Counts$PopName]
  Counts
}

Ro.function      <- function(Data){
  Dx.mat            <- acast(data = Data, formula = Age ~ Year, value.var = 'Deaths')
  Ex.mat            <- acast(data = Data, formula = Age ~ Year, value.var =  'Exposures')
  #Ex.mat[Ex.mat==0] <- 1e-06
  W     		        <- Ex.mat*0
  W[Ex.mat != 0] 	  <- 1
  fitDx             <- Mort2Dsmooth(x = as.numeric(rownames(Dx.mat)), 
                                    y = as.numeric(colnames(Dx.mat)), 
                                    Z = Dx.mat, offset = log(Ex.mat), W = W)
  mx                <- fitDx$fitted.values/Ex.mat
  mx.2              <- mx[, -1]
  mx.1              <- mx[, -(ncol(mx))]
  ro.mat            <- 100 * -log(mx.2/mx.1)
  ro                <- melt(ro.mat, c('Age','Year'),value.name = 'Ro')
  #ro[is.na(ro$Ro),]$Ro <- 0
  #ro[is.infinite(ro$Ro),]$Ro <- 0
  #ro$Ro
  ro
}


col.fig.F1b <- c('#e41a1c','#377eb8','#4daf4a',
                 '#984ea3','#ff7f00','#ffff33',
                 '#a65628','#f781bf',"black",
                 '#e41a1c','#377eb8','#4daf4a')

lwd.fig.F1b <- seq(from=2.5,to=1,length=12)

pch.fig.F1b <- c(9:20)

makeTransparent<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}


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

life.expectancy.frommx <- compiler::cmpfun(function(mx,sex = "f"){
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

edag.function.frommx <- compiler::cmpfun(function(mx,sex="f"){
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
})


Decomp <-function (func, rates1, rates2, N, ...) {
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

e0.from.mx5 <-function(mx=mx,sex='m'){
  Ages   <- c(0,1,seq(5,110,5))
  Widths              <- diff(Ages)
  N                   <- length(mx)
  RADIX               <- 1
  i.openage           <- length(mx)
  OPENAGE             <- i.openage - 1
  Widths              <-  c(Widths, Widths[N - 1])
  ax                  <- mx * 0 + Widths/2
  ax[1]               <- AKm02a0(m0 = mx[1], sex = sex)
  ax[i.openage] <- if (mx[i.openage] == 0) 0.5 else 1/mx[i.openage]
  
  qx                  <- (Widths*mx) / (1 + (Widths - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  qx[qx > 1] 	        <- 1
  
  px 				          <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			            <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))[1:N]
  dx 				          <-  c(-diff(lx),lx[N])
  Lx 	                <- c(Widths[1:(N - 1)] * lx[2:N] + ax[1:(N - 1)] * dx[1:(N - 1)], lx[N] * (1-exp(-10*mx[1]))/mx[1])
  Lx[is.infinite(Lx)] <- 1
  Lx[is.na(Lx)] 	    <- 0
  Tx 				          <- rev(cumsum(rev(Lx)))
  ex 				          <- Tx / lx
  ex[is.na(ex)] 	    <- 0
  ex[i.openage]       <- ifelse(mx[i.openage] == 0, ax[i.openage], {1 / mx[i.openage]})
  ex[1]
}


ed.from.mx5 <- function(mx,sex="m"){
  Ages   <- c(0,1,seq(5,110,5))
  Widths              <- diff(Ages)
  N                   <- length(mx)
  RADIX               <- 1
  i.openage           <- length(mx)
  OPENAGE             <- i.openage - 1
  Widths              <-  c(Widths, Widths[N - 1])
  ax                  <- mx * 0 + Widths/2
  ax[1]               <- AKm02a0(m0 = mx[1], sex = sex)
  ax[i.openage] <- if (mx[i.openage] == 0) 0.5 else 1/mx[i.openage]
  
  qx                  <- (Widths*mx) / (1 + (Widths - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  qx[qx > 1] 	        <- 1
  
  px 				          <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			            <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))[1:N]
  dx 				          <-  c(-diff(lx),lx[N])
  Lx 	                <- c(Widths[1:(N - 1)] * lx[2:N] + ax[1:(N - 1)] * dx[1:(N - 1)], lx[N] * (1-exp(-10*mx[1]))/mx[1])
  Lx[is.infinite(Lx)] <- 1
  Lx[is.na(Lx)] 	    <- 0
  Tx 				          <- rev(cumsum(rev(Lx)))
  ex 				          <- Tx / lx
  ex[is.na(ex)] 	    <- 0
  ex[i.openage]       <- ifelse(mx[i.openage] == 0, ax[i.openage], {1 / mx[i.openage]})
  l <- length(ex)
  ed <- (sum(dx[-l]* (ex[-l] + ax[-l]/Widths[-l]*(ex[-1]-ex[-l]))) + ex[l])
  ed
}

edfrommxc5            <- function(mxcvec,sex="f",...){
  dim(mxcvec) <- c(24,10)
  mx          <- rowSums(mxcvec)
  ed.from.mx5(mx,sex)
}

e0frommxc5            <- function(mxcvec,sex="f",...){
  dim(mxcvec) <- c(24,10)
  mx          <- rowSums(mxcvec)
  e0.from.mx5(mx,sex)
}

