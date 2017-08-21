###################### Life expectancy decomposition and e dagger by cause of death
### Author JM 
###############################################################################
library(reshape2)
library(data.table)
setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe")
source("R/Functions.R")


########## Load data by cause of death from HCoD
CZE <- read.csv(file = "Data/Cause of death data/CZE/CZE_d_interm_idr.csv",stringsAsFactors = F)[,c(1:25)]
EST <- read.csv(file = "Data/Cause of death data/EST/EST_d_interm_idr.csv",stringsAsFactors = F)[,c(1:25)]
LTU <- read.csv(file = "Data/Cause of death data/LTU/LTU_d_interm_idr.csv",stringsAsFactors = F)[,c(1:25)]
LVA <- read.csv(file = "Data/Cause of death data/LVA/LVA_d_interm_idr.csv",stringsAsFactors = F)[,c(1:25)]
POL <- read.csv(file = "Data/Cause of death data/POL/POL_d_interm_idr.csv",stringsAsFactors = F)[,c(1:25)]
RUS <- read.csv(file = "Data/Cause of death data/RUS/RUS_d_interm_idr.csv",stringsAsFactors = F)[,c(1:25)]
UKR <- read.csv(file = "Data/Cause of death data/UKR/UKR_d_interm_idr.csv",stringsAsFactors = F)[,c(1:25)]
##########
#Create an object with all data
#COD.Data <- BLR
COD.Data <- rbind(CZE,EST,LTU,LVA,POL,RUS,UKR)
COD.Data <- data.table(COD.Data)
gdata:: keep(COD.Data,sure=T)
#Take data from 1994-2010
#Take males and females 
# 1 <- males, 2 <- females
COD.Data <- COD.Data[COD.Data$year >= 1994 & COD.Data$year <= 2010 & COD.Data$sex<3,]
# drop total
COD.Data <- COD.Data[COD.Data$cause>0,-c(7)]
#rename cols to melt into DT
names(COD.Data)<-c(names(COD.Data)[1:6],0,1,seq(5,80,5))
# metl into DT
DT.data     <- melt(COD.Data, id.vars = 1:6,value.name = 'Dx',variable.name = 'age',variable.factor = F)
DT.data$age <- as.integer(DT.data$age)

#Group causes of death
### Causes of death that I am interested, following Grigorev & Andreev 2015.


### reclassify as I want
DT.data$Class <- 10  #Rest of causes

#1) Wholy attributable to alcohol
#2) IHD
#3) Stroke
#4) Transportation accidents
#5) Other external causes
#6) Infectious and respiratory diseases
#7) Cancers
#8) Other Circulatory
#9) birth conditions
#10) Rest of causes


#Wholy attributable to alcohol
DT.data[DT.data$cause==40,]$Class <- 1 #Alcohol abuse
DT.data[DT.data$cause==77,]$Class <- 1 #Cirrhosis of liver
DT.data[DT.data$cause==97,]$Class <- 1 #Accidental poisoning by alcohol
DT.data[DT.data$cause==78,]$Class <- 1 #Cirrhosis of liver


#check <- DT.data[DT.data$cause %in% c(51,52,53),]
#check <- COD.Data[COD.Data$cause %in% c(51,52,53),]
#sum(check$Dx)

#Mortality amenable to alcohol consumption
DT.data[DT.data$cause==51,]$Class <- 2 #IHD
DT.data[DT.data$cause==52,]$Class <- 2  #IHD
DT.data[DT.data$cause==53,]$Class <- 2 #IHD

DT.data[DT.data$cause==59,]$Class <- 3 #Stroke
DT.data[DT.data$cause==60,]$Class <- 3 #Stroke
DT.data[DT.data$cause==61,]$Class <- 3 #Stroke

DT.data[DT.data$cause==93,]$Class <- 4 #Transportation accidents

DT.data[DT.data$cause==94,]$Class <- 5 #Other external causes from Grigorev_PDR
DT.data[DT.data$cause==95,]$Class <- 5 #Other external causes from Grigorev_PDR
DT.data[DT.data$cause==99,]$Class <- 5 #Other external causes from Grigorev_PDR
DT.data[DT.data$cause==104,]$Class <- 5 #Other external causes from Grigorev_PDR

DT.data[DT.data$cause==96,]$Class <- 5 #Other external causes
DT.data[DT.data$cause==98,]$Class <- 5 #Other external causes
DT.data[DT.data$cause==100,]$Class <- 5 #Other external causes
DT.data[DT.data$cause==101,]$Class <- 5 #Other external causes
DT.data[DT.data$cause==102,]$Class <- 5 #Other external causes
DT.data[DT.data$cause==103,]$Class <- 5 #Other external causes

#Infectious and respiratory diseases
DT.data[DT.data$cause==1,]$Class <- 6
DT.data[DT.data$cause==2,]$Class <- 6
DT.data[DT.data$cause==3,]$Class <- 6
DT.data[DT.data$cause==4,]$Class <- 6
DT.data[DT.data$cause==5,]$Class <- 6
DT.data[DT.data$cause==6,]$Class <- 6
DT.data[DT.data$cause==7,]$Class <- 6
DT.data[DT.data$cause==8,]$Class <- 6
DT.data[DT.data$cause==9,]$Class <- 6
DT.data[DT.data$cause==65,]$Class <-6
DT.data[DT.data$cause==66,]$Class <- 6
DT.data[DT.data$cause==67,]$Class <- 6
DT.data[DT.data$cause==68,]$Class <- 6
DT.data[DT.data$cause==69,]$Class <- 6
DT.data[DT.data$cause==70,]$Class <- 6
DT.data[DT.data$cause==71,]$Class <- 6
DT.data[DT.data$cause==72,]$Class <- 6
DT.data[DT.data$cause==73,]$Class <- 6

#Cancers
DT.data[DT.data$cause==10,]$Class <- 7
DT.data[DT.data$cause==11,]$Class <- 7
DT.data[DT.data$cause==12,]$Class <- 7
DT.data[DT.data$cause==13,]$Class <- 7
DT.data[DT.data$cause==14,]$Class <- 7
DT.data[DT.data$cause==15,]$Class <- 7
DT.data[DT.data$cause==16,]$Class <- 7
DT.data[DT.data$cause==17,]$Class <- 7
DT.data[DT.data$cause==18,]$Class <- 7
DT.data[DT.data$cause==19,]$Class <- 7
DT.data[DT.data$cause==20,]$Class <- 7
DT.data[DT.data$cause==21,]$Class <- 7
DT.data[DT.data$cause==22,]$Class <- 7
DT.data[DT.data$cause==23,]$Class <- 7
DT.data[DT.data$cause==24,]$Class <- 7
DT.data[DT.data$cause==25,]$Class <- 7
DT.data[DT.data$cause==26,]$Class <- 7
DT.data[DT.data$cause==27,]$Class <- 7
DT.data[DT.data$cause==28,]$Class <- 7
DT.data[DT.data$cause==29,]$Class <- 7
DT.data[DT.data$cause==30,]$Class <- 7
DT.data[DT.data$cause==31,]$Class <- 7
DT.data[DT.data$cause==32,]$Class <- 7
DT.data[DT.data$cause==33,]$Class <- 7
DT.data[DT.data$cause==34,]$Class <- 7

#Other Circulatory
DT.data[DT.data$cause==48,]$Class <- 8
DT.data[DT.data$cause==49,]$Class <- 8
DT.data[DT.data$cause==50,]$Class <- 8
DT.data[DT.data$cause==54,]$Class <- 8
DT.data[DT.data$cause==55,]$Class <- 8
DT.data[DT.data$cause==56,]$Class <- 8
DT.data[DT.data$cause==57,]$Class <- 8
DT.data[DT.data$cause==58,]$Class <- 8
DT.data[DT.data$cause==62,]$Class <- 8
DT.data[DT.data$cause==63,]$Class <- 8
DT.data[DT.data$cause==64,]$Class <- 8

#birth conditions
DT.data[DT.data$cause==89,]$Class <- 9
DT.data[DT.data$cause==90,]$Class <- 9
DT.data[DT.data$cause==91,]$Class <- 9
DT.data[DT.data$cause==92,]$Class <- 9

# now deal with Belarus and get it int he same shape as DT.data
#Treat Belarus differently to get the same classification
BLR.mx <- read.csv(file = "Data/Cause of death data/BLR/BLR_m_full_idr.csv",stringsAsFactors = F)[,c(1:24)]
BLR.ex <- read.csv(file = "Data/Cause of death data/BLR/BLR_e.csv",stringsAsFactors = F)[,c(1:23)]
BLR.ex <- BLR.ex[,-c(4:5)]
#rename cols to melt into DT
names(BLR.mx)<-c(names(BLR.mx)[1:6],0,1,seq(5,80,5))
names(BLR.ex)<-c(names(BLR.ex)[1:3],0,1,seq(5,80,5))
# not convert to deaths by multiplying with exposures
BLR.mx.melt     <- melt(BLR.mx, id.vars = 1:6,value.name = 'mx',variable.name = 'age',variable.factor = F)
BLR.ex.melt     <- melt(BLR.ex, id.vars = 1:3,value.name = 'Ex',variable.name = 'age',variable.factor = F)
BLR.mx.melt$age <- as.integer(as.character(BLR.mx.melt$age))
BLR.ex.melt$age <- as.integer(as.character(BLR.ex.melt$age))
# merge exposures to the data set with the mx's
BLR.merged      <- merge(x = BLR.mx.melt,y = BLR.ex.melt,by = c('country','year','sex','age'))
BLR.merged$Dx   <- (BLR.merged$mx/100000)*BLR.merged$Ex

BLR <- BLR.merged[BLR.merged$year >= 1994 & BLR.merged$year <= 2010 & BLR.merged$sex<3,]
BLR <- BLR.merged[BLR.merged$cause > 0,]
BLR <- BLR[,-c(8,9)]
BLR <- BLR[,c('country','year','sex','list','agf','cause','age','Dx')]
### Now classify accordingly
BLR$Class <- 10 #Rest of causes

#1) Wholy attributable to alcohol
#2) IHD
#3) Stroke
#4) Transportation accidents
#5) Other external causes
#6) Infectious and respiratory diseases
#7) Cancers
#8) Other Circulatory
#9) birth conditions
#10) Rest of causes

#Wholy attributable to alcohol
BLR[BLR$cause %in% c(106,107,186,187,269),]$Class <- 1


#Mortality amenable to alcohol consumption
BLR[BLR$cause %in% c(130:139),]$Class <- 2 # IHD
BLR[BLR$cause %in% c(144:152),]$Class <- 3 # Stroke
BLR[BLR$cause %in% c(256:262),]$Class <- 4 # Transport accidents

#Other conditions amenable to alcohol consumptopm
BLR[BLR$cause %in% c(268,270,271,272,273,274,275,276,277,263:267),]$Class <- 5 #Other external causes from Grigorev_PDR
#Other Circulatory
BLR[BLR$cause %in% c(124:129,140:143,153:159),]$Class <- 8

#Infectious and respiratory diseases
BLR[BLR$cause %in% c(1:58),]$Class <- 6

#Cancers
BLR[BLR$cause %in% c(59:93),]$Class <- 7


#birth conditions
BLR[BLR$cause %in% c(219:235,236:254),]$Class <- 9

# Aggregate over new categories
DT.data <- rbind(BLR,DT.data)
DT.data <- DT.data[, list(Dx = sum(Dx)), by = list(country,year,sex,Class,age)]

# get proportions
DT.data <- DT.data[, prop:=Dx/sum(Dx) , by = list(country,year,sex,age)]
DT.data[is.na(DT.data$prop),]$prop <- 0
DT.COD.data <- DT.data


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

HMDL_5 <- data.table(HMDL_5)
HMDL_5 <- HMDL_5[HMDL_5$Year >= 1994 & HMDL_5$Year <= 2010,]

save(DT.COD.data,HMDL_5,file="Data/COD_LT_5Data.RData")




