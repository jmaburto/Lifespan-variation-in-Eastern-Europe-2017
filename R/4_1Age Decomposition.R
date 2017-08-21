###################### Life expectancy decomposition and e dagger
### Author JM (Horiuchi etal 2008)
###############################################################################
library(reshape2)
library(data.table)
library(foreach)

setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe")

load("Data/HMD_Data.RData")
source("R/Functions.R")
Eastern_HMDL             <- HMDL[HMDL$PopName %in% Country.HMD.vec & HMDL$Year >= 1960,]
Eastern_HMDL$Country     <- Country.name.vec[Eastern_HMDL$PopName]
Eastern_HMDL$Sex         <- as.factor(Eastern_HMDL$Sex)
levels(Eastern_HMDL$Sex) <- Sexes
Eastern_HMDL             <- Eastern_HMDL[with(Eastern_HMDL,order(Country,Sex,Year,Age)),]
gdata::keep(Eastern_HMDL,sure=T)
source("R/Functions.R")


#i <- Country.name.vec[1]
#j <- Sexes[1]

Decomp.results <- NULL
for (k in Country.name.vec){
  for(j in Sexes){
    D1         <- Eastern_HMDL[Eastern_HMDL$Country==k & Eastern_HMDL$Sex == j,]
    years      <- unique(sort(D1$Year))[-1]
    mat        <- acast(D1, Age~Year, value.var="mx")  
    
    dime       <- dim(mat)[2]
    e0.decomp  <- foreach(i=1:(dime-1)) %dopar% {Decomp(func=life.expectancy.frommx, rates1=mat[,i] , rates2=mat[,i+1], N=100,sex=j)}
    ed.decomp  <- foreach(i=1:(dime-1)) %dopar% {Decomp(func=edag.function.frommx, rates1=mat[,i] , rates2=mat[,i+1], N=100,sex=j)}
    
    D.e0        <- do.call(cbind, lapply(e0.decomp, data.frame, stringsAsFactors=FALSE))
    names(D.e0) <- years
    rownames(D.e0) <- 0:110
    D.ed        <- do.call(cbind, lapply(ed.decomp, data.frame, stringsAsFactors=FALSE))
    names(D.ed) <- years
    rownames(D.ed) <- 0:110
    
    DT.e0      <- melt(as.matrix(D.e0), varnames = c('age','year'),value.name = 'Contribution')
    DT.e0$Ind  <- 'e0'
    DT.ed      <- melt(as.matrix(D.ed), varnames = c('age','year'),value.name = 'Contribution')
    DT.ed$Ind  <- 'ed'
    DT.decomp  <- rbind(DT.e0,DT.ed)
    DT.decomp$country <- unique(D1$Country)
    DT.decomp$PopName <- unique(D1$PopName)
    DT.decomp$sex <- unique(D1$Sex)
    
    Decomp.results     <- rbind(Decomp.results,DT.decomp)
    print(k)
  }
}



save(Decomp.results, file = "Data/Age_Decomp_results.Rdata")


