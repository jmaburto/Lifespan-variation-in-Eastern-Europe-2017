###################### Life expectancy decomposition and e dagger by cause of death
### Author JM (Horiuchi etal 2008)
###############################################################################
library(DecompHoriuchi)
library(reshape2)
setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")
setwd("C:/Users/Studente/Desktop/Aburto_vanRaalte_2016/")

########## Load data by cause of death from HCoD
BLR <- read.csv(file = "Data/Cause of death data/BLR/BLR_d_interm_idr.csv")
for (i in 7:32) BLR[,i] <- as.numeric(levels(BLR[,i]))[BLR[,i]]
BLR[is.na(BLR)]    <- 0
CZE <- read.csv(file = "Data/Cause of death data/CZE/CZE_d_interm_idr.csv")
EST <- read.csv(file = "Data/Cause of death data/EST/EST_d_interm_idr.csv")
LTU <- read.csv(file = "Data/Cause of death data/LTU/LTU_d_interm_idr.csv")
LVA <- read.csv(file = "Data/Cause of death data/LVA/LVA_d_interm_idr.csv")
POL <- read.csv(file = "Data/Cause of death data/POL/POL_d_interm_idr.csv")
RUS <- read.csv(file = "Data/Cause of death data/RUS/RUS_d_interm_idr.csv")
UKR <- read.csv(file = "Data/Cause of death data/UKR/UKR_d_interm_idr.csv")

##########
#Create an object with all data
COD.Data <- rbind(BLR,CZE,EST,LTU,LVA,POL,RUS,UKR)
gdata:: keep(COD.Data,sure=T)
#Take data from 1994-2010
COD.Data <- subset(COD.Data, year >= 1994 & year <= 2010)
#Take males and females 
# 1 <- males, 2 <- females
COD.Data <- subset(COD.Data, sex < 3)
#Take interm list
COD.Data <- subset(COD.Data, list == "interm")
#Transform variables from factor to numeric
#Keep data to 85+
COD.Data <- COD.Data[,-c(27:32)]
COD.Data <- as.data.frame(COD.Data)


### Causes of death that I am interested, following Grigorev & Andreev 2015.
#1)Accidental poisoning by alcohol X45 <- 97
#2) Cirrhosis of liver K70, K74 <- 77,78
#3) IHD I20-I25 <- 51,52,53
#4) Stroke I60-I67 <- 59,60,61
#5) Transportation accidents V02-V99 <- 93
#6) Other external causes X-Y except X45,V02-V99 <- 96,98,100,101,102,103 (specify in the aper the icd codes)
#7) Rest of causes
### reshape in a more managable way
COD.Data <- subset(COD.Data,select = c("country","year","sex","cause","d0","d1","d5","d10",
                                 "d15","d20","d25","d30","d35","d40","d45","d50","d55",
                                 "d60","d65","d70","d75","d80","d85p"))

Data.COD <- melt(COD.Data,id=1:4,varnames = c("country","year","sex","cause"))

### reclassify as I want
Data.COD$Class <- 7  #Rest of causes

#Mortality amenable to alcohol consumption
Data.COD$Class[Data.COD$cause==40] <- 1 #Alcohol abuse
Data.COD$Class[Data.COD$cause==97] <- 1 #Accidental poisoning by alcohol
Data.COD$Class[Data.COD$cause==77] <- 1 #Cirrhosis of liver
Data.COD$Class[Data.COD$cause==78] <- 1 #Cirrhosis of liver
Data.COD$Class[Data.COD$cause==51] <- 1 #IHD
Data.COD$Class[Data.COD$cause==52] <- 1 #IHD
Data.COD$Class[Data.COD$cause==53] <- 1 #IHD
Data.COD$Class[Data.COD$cause==59] <- 1 #Stroke
Data.COD$Class[Data.COD$cause==60] <- 1 #Stroke
Data.COD$Class[Data.COD$cause==61] <- 1 #Stroke
Data.COD$Class[Data.COD$cause==93] <- 1 #Transportation accidents
Data.COD$Class[Data.COD$cause==96] <- 1 #Other external causes
Data.COD$Class[Data.COD$cause==97] <- 1 #Other external causes
Data.COD$Class[Data.COD$cause==98] <- 1 #Other external causes
Data.COD$Class[Data.COD$cause==100] <- 1 #Other external causes
Data.COD$Class[Data.COD$cause==101] <- 1 #Other external causes
Data.COD$Class[Data.COD$cause==102] <- 1 #Other external causes
Data.COD$Class[Data.COD$cause==103] <- 1 #Other external causes

#Infectious and respiratory diseases
Data.COD$Class[Data.COD$cause==1] <- 2
Data.COD$Class[Data.COD$cause==2] <- 2
Data.COD$Class[Data.COD$cause==3] <- 2
Data.COD$Class[Data.COD$cause==4] <- 2
Data.COD$Class[Data.COD$cause==5] <- 2
Data.COD$Class[Data.COD$cause==6] <- 2
Data.COD$Class[Data.COD$cause==7] <- 2
Data.COD$Class[Data.COD$cause==8] <- 2
Data.COD$Class[Data.COD$cause==9] <- 2
Data.COD$Class[Data.COD$cause==65] <- 2
Data.COD$Class[Data.COD$cause==66] <- 2
Data.COD$Class[Data.COD$cause==67] <- 2
Data.COD$Class[Data.COD$cause==68] <- 2
Data.COD$Class[Data.COD$cause==69] <- 2
Data.COD$Class[Data.COD$cause==70] <- 2
Data.COD$Class[Data.COD$cause==71] <- 2
Data.COD$Class[Data.COD$cause==72] <- 2
Data.COD$Class[Data.COD$cause==73] <- 2

#Cancers
Data.COD$Class[Data.COD$cause==10] <- 3
Data.COD$Class[Data.COD$cause==11] <- 3
Data.COD$Class[Data.COD$cause==12] <- 3
Data.COD$Class[Data.COD$cause==13] <- 3
Data.COD$Class[Data.COD$cause==14] <- 3
Data.COD$Class[Data.COD$cause==15] <- 3
Data.COD$Class[Data.COD$cause==16] <- 3
Data.COD$Class[Data.COD$cause==17] <- 3
Data.COD$Class[Data.COD$cause==18] <- 3
Data.COD$Class[Data.COD$cause==19] <- 3
Data.COD$Class[Data.COD$cause==20] <- 3
Data.COD$Class[Data.COD$cause==21] <- 3
Data.COD$Class[Data.COD$cause==22] <- 3
Data.COD$Class[Data.COD$cause==23] <- 3
Data.COD$Class[Data.COD$cause==24] <- 3
Data.COD$Class[Data.COD$cause==25] <- 3
Data.COD$Class[Data.COD$cause==26] <- 3
Data.COD$Class[Data.COD$cause==27] <- 3
Data.COD$Class[Data.COD$cause==28] <- 3
Data.COD$Class[Data.COD$cause==29] <- 3
Data.COD$Class[Data.COD$cause==30] <- 3
Data.COD$Class[Data.COD$cause==31] <- 3
Data.COD$Class[Data.COD$cause==32] <- 3
Data.COD$Class[Data.COD$cause==33] <- 3
Data.COD$Class[Data.COD$cause==34] <- 3

#Circulatory
Data.COD$Class[Data.COD$cause==48] <- 4
Data.COD$Class[Data.COD$cause==49] <- 4
Data.COD$Class[Data.COD$cause==50] <- 4
Data.COD$Class[Data.COD$cause==54] <- 4
Data.COD$Class[Data.COD$cause==55] <- 4
Data.COD$Class[Data.COD$cause==56] <- 4
Data.COD$Class[Data.COD$cause==57] <- 4
Data.COD$Class[Data.COD$cause==58] <- 4
Data.COD$Class[Data.COD$cause==62] <- 4
Data.COD$Class[Data.COD$cause==63] <- 4
Data.COD$Class[Data.COD$cause==64] <- 4

#Diabetes
Data.COD$Class[Data.COD$cause==35] <- 5

#birth conditions
Data.COD$Class[Data.COD$cause==89] <- 6
Data.COD$Class[Data.COD$cause==90] <- 6
Data.COD$Class[Data.COD$cause==91] <- 6
Data.COD$Class[Data.COD$cause==92] <- 6


### Aggregate by new classification
Data <- aggregate(Data.COD$value, by = list(Age=Data.COD$variable,Data.COD$Class,
                                              Year=Data.COD$year,Sex=Data.COD$sex,
                                            Country=Data.COD$country),FUN=sum)
Data$Age <- rep(c(0,1,seq(5,85,5)),dim(Data)[1]/19)
gdata::keep(Data,sure=T)


for (i in 1:7){
  if (i==1) Data.Prop <- subset(Data, Group.2==i)
  assign(paste("cause",i,sep=""),subset(Data,Group.2==i)$x)
}
Data.Prop$cause1 <- cause1
Data.Prop$cause2 <- cause2
Data.Prop$cause3 <- cause3
Data.Prop$cause4 <- cause4
Data.Prop$cause5 <- cause5
Data.Prop$cause6 <- cause6
Data.Prop$cause7 <- cause7

Data.Prop <- subset(Data.Prop, select = c("Age","Year","Sex","Country","cause1","cause2",
                                          "cause3","cause4","cause5","cause6","cause7"))
Data.Prop$Total <- Data.Prop$cause1+Data.Prop$cause2+Data.Prop$cause3+Data.Prop$cause4+Data.Prop$cause5+Data.Prop$cause6+Data.Prop$cause7
Data.Prop <- as.data.frame(Data.Prop)
row.names(Data.Prop) <- 1:nrow(Data.Prop)
#Ready to calculate proportions by age, the stucture that will be applied to 5 year data
PropC1 <- Data.Prop$cause1/Data.Prop$Total
PropC2 <- Data.Prop$cause2/Data.Prop$Total
PropC3 <- Data.Prop$cause3/Data.Prop$Total
PropC4 <- Data.Prop$cause4/Data.Prop$Total
PropC5 <- Data.Prop$cause5/Data.Prop$Total
PropC6 <- Data.Prop$cause6/Data.Prop$Total
PropC7 <- Data.Prop$cause7/Data.Prop$Total

PropC1[is.na(PropC1)]    <- 0
PropC2[is.na(PropC2)]    <- 0
PropC3[is.na(PropC3)]    <- 0
PropC4[is.na(PropC4)]    <- 0
PropC5[is.na(PropC5)]    <- 0
PropC6[is.na(PropC6)]    <- 0
PropC7[is.na(PropC7)]    <- 0

COD.structure   <- subset(Data.Prop,select = c("Age","Year","Sex","Country"))
COD.structure$P1 <- PropC1
COD.structure$P2 <- PropC2
COD.structure$P3 <- PropC3
COD.structure$P4 <- PropC4
COD.structure$P5 <- PropC5
COD.structure$P6 <- PropC6
COD.structure$P7 <- PropC7

#chech consistency
Tot <- COD.structure$P1 +COD.structure$P2 +COD.structure$P3 +COD.structure$P4 +COD.structure$P5 +COD.structure$P6 +COD.structure$P7 
sum(Tot)

gdata::keep(COD.structure,sure=T)
  ### Causes of death that I am interested, following Grigorev & Andreev 2015.
  #1)Accidental poisoning by alcohol X45 <- 97
  #2) Cirrhosis of liver K70, K74 <- 77,78
  #3) IHD I20-I25 <- 51,52,53
  #4) Stroke I60-I67 <- 59,60,61
  #5) Transportation accidents V02-V99 <- 93
  #6) Other external causes X-Y except X45,V02-V99 <- 96,98,100,101,102,103 (specify in the aper the icd codes)
  #7) Rest of causes
  #8) mortality avobe 85
  ### reshape in a more managable way

#Take care fo the group85+, I won't consider it as cause specific analysis
COD.structure$P1[COD.structure$Age==85] <- 0
COD.structure$P2[COD.structure$Age==85] <- 0
COD.structure$P3[COD.structure$Age==85] <- 0
COD.structure$P4[COD.structure$Age==85] <- 0
COD.structure$P5[COD.structure$Age==85] <- 0
COD.structure$P6[COD.structure$Age==85] <- 0
COD.structure$P7[COD.structure$Age==85] <- 0
COD.structure$P8 <- 0
COD.structure$P8[COD.structure$Age==85] <- 1
  
  
save(COD.structure,file="Data/CoD_Structure.RData")




###### I need data in 5 year age groups 

# now grab all the lifetables and mesh together..
# grab them all
library(HMDHFDplus)
XYZ <- c("BLR","CZE","EST","LTU","LVA","POL","RUS","UKR")
us <- "jmaburto@colmex.mx"
pw <- "kolmogorov"

HMDL_5 <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Males        <- readHMDweb(x,"mltper_5x1",username=us,password=pw)
  Females      <- readHMDweb(x,"fltper_5x1",username=us,password=pw)
  Males$Sex    <- "m"
  Females$Sex  <- "f"
  CTRY         <- rbind(Females, Males)
  CTRY$PopName <- x
  CTRY    
}, us = us, pw = pw))

Eastern_LT_5   <- subset(HMDL_5, Year >= 1994 & Year <= 2010)

save(Eastern_LT_5,file="Data/EE_5LT.RData")


################ Ready to calculate age-specific mortality rates
### Polonia information is until 2009
load("Data/CoD_Structure.RData")
load("Data/EE_5LT.RData")

XYZ <- c("BLR","CZE","EST","LTU","LVA","POL","RUS","UKR")
#k <- XYZ[6]
#i <- 1
#j <- 1994

Mx.Cause <- NULL
for (k in XYZ){
  l <- 2010
  if (k=="POL") l <- 2009
  for (i in 1:2){
    for (j in 1994:l){
      if (i == 1) Sx <- "m"
      if (i == 3) Sx <- "f"
      D  <- subset(COD.structure,Country==k & Sex==i & Year==j)
      E  <- subset(Eastern_LT_5,PopName==k & Sex==Sx & Year==j)
      M1 <- as.matrix(D[,5:12])
      v1 <- matrix(c(rep(0,35),rep(1,5)),nrow = 5,ncol = 8)
      M2 <- rbind(M1,v1)
      cMx <- M2*E$mx
      row.names(cMx)<-NULL
      cMx <- as.data.frame(cMx)
      M <- cbind(Country=rep(k,dim(E)[1]),Sex=rep(i,dim(E)[1]),Year=rep(j,dim(E)[1]))
      M <- as.data.frame(M)
      M$Country <- as.character(M$Country)
      M$Sex <- as.numeric(levels(M$Sex))[M$Sex]
      M$Year <- as.numeric(levels(M$Year))[M$Year]
      M$Age <- E$Age
      M$ax <-E$ax
      Mx <- cbind(M,cMx)
      Mx.Cause <- rbind(Mx.Cause,Mx)
    }
  }
}



save(Mx.Cause,file="Data/mx_causes.RData")

