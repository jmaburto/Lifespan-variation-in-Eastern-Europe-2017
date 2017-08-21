###################### Life expectancy decomposition and e dagger
### Author JM (Horiuchi etal 2008)
###############################################################################
library(DecompHoriuchi)
library(reshape2)
setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")

load("Data/HMD_Data.RData")

#### Select countries from eastern Europe
Eastern_HMDL   <- subset(HMDL,(PopName=="BLR" | PopName=="BGR" | PopName=="CZE" | 
                                 PopName=="HUN"| PopName=="POL"| PopName=="RUS" | 
                                 PopName=="SVK" | PopName=="UKR" | PopName=="SVN" 
                               | PopName=="EST" | PopName=="LVA" | PopName=="LTU"))

Eastern_HMDL$Country[Eastern_HMDL$PopName=="BLR"] <- "Belarus"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="BGR"] <- "Bulgaria"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="CZE"] <- "Czech Republic"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="HUN"] <- "Hungary"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="POL"] <- "Poland"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="RUS"] <- "Russia"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="SVK"] <- "Slovakia"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="UKR"] <- "Ukraine"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="SVN"] <- "Slovenia"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="EST"] <- "Estonia"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="LVA"] <- "Latvia"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="LTU"] <- "Lithuania"

Eastern_HMDL$Sex1[Eastern_HMDL$Sex=="f"] <- "Females"
Eastern_HMDL$Sex1[Eastern_HMDL$Sex=="m"] <- "Males"

###Drop data before 1960
Eastern_HMDL <- subset(Eastern_HMDL, Year>=1960)
gdata::keep(Eastern_HMDL,sure=T)
source("R/Sensitivity Analysis/gini_decomp_func.R")

#An example
#Decomposition of fini
lt1        <- subset(Eastern_HMDL, Year==1960 & PopName=="CZE" & Sex=="m")
lt2        <- subset(Eastern_HMDL, Year==2014 & PopName=="CZE" & Sex=="m")

gini.func(lt1$mx,sex="m")
gini.func(lt2$mx,sex="m")
gini.decomp <- Decomp(func=gini.func, rates1=lt1$mx , rates2=lt2$mx, N=100,sex="m")
plot(gini.decomp[-1])
sum(gini.decomp)
gini.func(lt1$mx,sex="m")-gini.func(lt2$mx,sex="m")




############# Decomposing by single age results and first differences
Data <- subset(Eastern_HMDL, select=c(Year, Age, mx, Sex, Country))
nms  <- unique(Data$Country)
#i <- "Russia"
#j <- "f"
#k <- 1
Decomp.results <- NULL
for (i in nms){
  for(j in c("f","m")){
    D1  <- subset(Data, Country==i & Sex==j)
    mat <- acast(D1, Age~Year, value.var="mx")  
    for( k in 1:(dim(mat)[2]-1)){
      ed.decomp    <- Decomp(func=gini.func, rates1=mat[,k] , rates2=mat[,k+1], N=50,sex=j)
      Dr           <- cbind(Name=rep(i,length(ed.decomp)),Age =rep(0:110,1),measure=c(rep("Gini",length(ed.decomp))),
                            value=c(ed.decomp),Year=rep(colnames(mat)[k],length(ed.decomp)),
                            Sex=rep(j,length(ed.decomp)))
      Dr           <- as.data.frame(Dr)
      row.names(Dr)<-NULL
      Dr$value <- as.numeric(levels(Dr$value))[Dr$value]
      Dr$Year <- as.numeric(as.character(Dr$Year))
      Decomp.results     <- rbind(Decomp.results,Dr)
      print(i)
    }
  }
}

save(Decomp.results, file = "R/Sensitivity Analysis/Decomp_results.Rdata")

