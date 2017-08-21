###################### Life expectancy decomposition and e dagger by causes of death
### Author JM (Horiuchi etal 2008)
###############################################################################

library(data.table)
library(foreach)
library(doParallel)  
library(parallel)
library(reshape2)
detectCores()

setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe")


################ Ready to calculate age-specific mortality rates
### Polonia information is until 2009
load("Data/COD_LT_5Data.RData")
XYZ <- c("BLR","CZE","EST","LTU","LVA","POL","RUS","UKR")
#k <- XYZ[6]
#i <- 1
#j <- 1994

Mx.Cause <- NULL
for (k in XYZ){
  for (i in 1:2){
    for (j in 1994:2010){
      Sx <- ifelse(i==1,'m','f')
      
      D  <- DT.COD.data[DT.COD.data$country == k & DT.COD.data$sex == i & DT.COD.data$year == j,]
      D  <- as.matrix(acast(D,age ~ Class,value.var = 'prop'))
      v1 <- matrix(c(rep(0,54),rep(1,6)),nrow = 6,ncol = 10)
      D <- rbind(D,v1)
      rownames(D) <- unique(HMDL_5$Age)
      
      E  <- HMDL_5[HMDL_5$PopName == k & HMDL_5$Sex == Sx & HMDL_5$Year == j,]
      
      cMx <- D*E$mx
      cause.mx <- melt(cMx)
      names(cause.mx) <- c('age','cause','mxc')
      cause.mx$year <- j
      cause.mx$sex <- i
      cause.mx$country <- k
      
      Mx.Cause <- rbind(Mx.Cause,cause.mx)
    }
  }
}
Mx.Cause <- data.table(Mx.Cause)
gdata::keep(Mx.Cause, sure = T)
source("R/Functions.R")

library(latticeExtra)

F1 <- xyplot(mxc ~ year|factor(age), groups = cause, data = Mx.Cause[Mx.Cause$sex == 1 & 
                                                                       Mx.Cause$cause %in% c(1,4) & Mx.Cause$age > 10 & Mx.Cause$age < 85 & country == 'BLR'],
             type = 'l',auto.key = T)
             
             
for (i in unique(Mx.Cause$country)){

F2 <- xyplot(mxc ~ year|factor(age), groups = cause, data = Mx.Cause[Mx.Cause$sex == 1 & 
                                                                 Mx.Cause$cause %in% c(1,4) & Mx.Cause$age > 10 & Mx.Cause$age < 85 & country == i],
       type = 'l',auto.key = T)
F1 <- F1 + F2
}
F1
####### Decompose everything


XYZ <- unique(Mx.Cause$country)
CoD.Results <- NULL
years       <- 1994:2009
#k <- XYZ[1]
#i <- 1
#j <- 1994

cl <- makeCluster(4)  
registerDoParallel(cl) 

for (k in XYZ){
  for ( i in 1:2){
      Sx <- ifelse(i==1,'m','f')
      dime <- dim(get.mat.fun(j=1994,i=i,k=k,data=Mx.Cause))

      ed.decomp.c <- foreach(j=years,.packages = "reshape2") %dopar% {cause.decomp.shape(func = edfrommxc5, rates1 = c(get.mat.fun(j=j,i=i,
                                                                                                            k=k,data=Mx.Cause)),
                                                                  rates2 = c(get.mat.fun(j=j+1,i=i,k=k,data=Mx.Cause)), 
                                                                  N=100, sex=Sx,dime=dime,k2=k,j2=j,ind='ed')}
      
      
      e0.decomp.c <- foreach(j=years,.packages = "reshape2") %dopar% {cause.decomp.shape(func = e0frommxc5, rates1 = c(get.mat.fun(j=j,i=i,
                                                                                                            k=k,data=Mx.Cause)),
                                                                  rates2 = c(get.mat.fun(j=j+1,i=i,k=k,data=Mx.Cause)), 
                                                                  N=100, sex=Sx,dime=dime,k2=k,j2=j,ind='ex')}
      
      
      d.ed        <- do.call(rbind, lapply(ed.decomp.c, data.frame, stringsAsFactors=FALSE))
      d.e0        <- do.call(rbind, lapply(e0.decomp.c, data.frame, stringsAsFactors=FALSE))
      
      CoD.Results <- rbind(CoD.Results,d.ed,d.e0)
      print(k)
  }
}
stopCluster(cl)


CoD.Results$Country     <- Country.name.vec[CoD.Results$country]
CoD.Results$sex         <- as.factor(CoD.Results$sex)
levels(CoD.Results$sex) <- Sexes
CoD.Results$cause  <- as.factor(CoD.Results$cause)
levels(CoD.Results$cause) <- labels.cause
CoD.Results$age  <- as.factor(CoD.Results$age)
levels(CoD.Results$age) <- Labels.age2
  

save(CoD.Results,file="Data/Decomp_Causes_EE.RData")


