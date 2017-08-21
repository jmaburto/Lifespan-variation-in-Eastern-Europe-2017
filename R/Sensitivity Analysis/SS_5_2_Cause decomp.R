###################### Life expectancy decomposition and e dagger by causes of death
### Author JM (Horiuchi etal 2008)
###############################################################################
#library(DecompHoriuchi)
library(reshape2)
setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")


load("Data/mx_causes.RData")
load("Data/CoD_Structure.RData")
load("Data/EE_5LT.RData")

source("R/edag_decomp_func.R")

#remember Polonia
#An example
D1 <- subset(Mx.Cause, Year==2000 & Sex==1 & Country=="RUS")
m1 <- as.matrix(D1[,6:13])
D2 <- subset(Mx.Cause, Year==2010 & Sex==1 & Country=="RUS")
m2 <- as.matrix(D2[,6:13])


edfrommxc(m2,sex="m")-edfrommxc(m1,sex="m")


############ Lets decompose by causes of death
rates1 <- m1
rates2 <- m2

CoDecomp             <- DecompContinuous_ed_5(rates1 = c(rates1),rates2 = c(rates2), N=50, sex="m")
dim(CoDecomp)        <- dim(rates1)
dimnames(CoDecomp)   <- dimnames(rates1)
row.names(CoDecomp)  <- NULL
sum(CoDecomp)

CoD.Results   <- melt(CoDecomp)
CoD.Results   <- melt(CoDecomp,varnames = c("Row","Cause"))



####### Decompose everything
XYZ <- unique(Mx.Cause$Country)

CoD.Results <- NULL
for (k in XYZ){
  l <- 2009
  if (k=="POL") l <- 2008
  for ( i in 1:2){
    for (j in 1994:l){
      D1 <- subset(Mx.Cause, Year==j & Sex==i & Country==k)
      m1 <- as.matrix(D1[,6:13])
      D2 <- subset(Mx.Cause, Year==j+1 & Sex==i & Country==k)
      m2 <- as.matrix(D2[,6:13])
      
      if (i == 1) Sx <- "m"
      if (i == 2) Sx <- "f"
      
      CoDecomp             <- DecompContinuous_ed_5(rates1 = c(m1),rates2 = c(m2), N=50, sex=Sx)
      dim(CoDecomp)        <- dim(m1)
      dimnames(CoDecomp)   <- dimnames(m1)
      row.names(CoDecomp)  <- NULL
      
      R   <- melt(CoDecomp,varnames = c("Country","Cause"))
      R$Country <- k
      R$Sex     <- Sx
      R$Age     <- rep(c(0,1,seq(5,110,5)),8)
      R$Year    <- j
      
      CoD.Results <- rbind(CoD.Results,R)
    }
  }
}
save(CoD.Results,file="Data/Decomp_Causes_EE.RData")


