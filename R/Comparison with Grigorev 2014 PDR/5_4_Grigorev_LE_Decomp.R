library(reshape2)
library(latticeExtra)
setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")

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
COD.Data <- subset(COD.Data,COD.Data$cause<105)

sum(rowSums(COD.Data[,8:26])-COD.Data$total)

COD.Data <- as.data.frame(COD.Data)


COD.Data <- subset(COD.Data,select = c("country","year","sex","cause","d0","d1","d5","d10",
                                       "d15","d20","d25","d30","d35","d40","d45","d50","d55",
                                       "d60","d65","d70","d75","d80","d85p"))

Data.COD <- melt(COD.Data,id=1:4,varnames = c("country","year","sex","cause"))

### reclassify as I want
Data.COD$Class <- 4  #Rest of causes

#1) Heart diseases
#2) Cerebrovascular diseases and other cardiovascular diseases
#3) Injury and poisoning
#5) Above age 85

#1) Heart diseases
Data.COD$Class[Data.COD$cause==48] <- 1
Data.COD$Class[Data.COD$cause==49] <- 1
Data.COD$Class[Data.COD$cause==50] <- 1
Data.COD$Class[Data.COD$cause==51] <- 1
Data.COD$Class[Data.COD$cause==52] <- 1
Data.COD$Class[Data.COD$cause==53] <- 1
Data.COD$Class[Data.COD$cause==54] <- 1
Data.COD$Class[Data.COD$cause==55] <- 1
Data.COD$Class[Data.COD$cause==56] <- 1
Data.COD$Class[Data.COD$cause==57] <- 1
Data.COD$Class[Data.COD$cause==58] <- 1

#2) Cerebrovascular diseases and other cardiovascular diseases
Data.COD$Class[Data.COD$cause==59] <- 2
Data.COD$Class[Data.COD$cause==60] <- 2
Data.COD$Class[Data.COD$cause==61] <- 2
Data.COD$Class[Data.COD$cause==62] <- 2
Data.COD$Class[Data.COD$cause==63] <- 2
Data.COD$Class[Data.COD$cause==64] <- 2

#3) Injury and poisoning
Data.COD$Class[Data.COD$cause==93] <- 3 #Transportation accidents
Data.COD$Class[Data.COD$cause==94] <- 3 #Other external causes from Grigorev_PDR
Data.COD$Class[Data.COD$cause==95] <- 3 #Other external causes from Grigorev_PDR
Data.COD$Class[Data.COD$cause==96] <- 3 #Other external causes
Data.COD$Class[Data.COD$cause==97] <- 3 #Accidental poisoning by alcohol
Data.COD$Class[Data.COD$cause==98] <- 3 #Other external causes
Data.COD$Class[Data.COD$cause==99] <- 3 #Other external causes from Grigorev_PDR
Data.COD$Class[Data.COD$cause==100] <- 3 #Other external causes
Data.COD$Class[Data.COD$cause==101] <- 3 #Other external causes
Data.COD$Class[Data.COD$cause==102] <- 3 #Other external causes
Data.COD$Class[Data.COD$cause==103] <- 3 #Other external causes
Data.COD$Class[Data.COD$cause==104] <- 3 #Other external causes from Grigorev_PDR

### Aggregate by new classification
Data <- aggregate(Data.COD$value, by = list(Age=Data.COD$variable,Data.COD$Class,
                                            Year=Data.COD$year,Sex=Data.COD$sex,
                                            Country=Data.COD$country),FUN=sum)

Data$Age <- rep(c(0,1,seq(5,85,5)),dim(Data)[1]/19)
gdata::keep(Data,sure=T)


for (i in 1:10){
  if (i==1) Data.Prop <- subset(Data, Group.2==i)
  assign(paste("cause",i,sep=""),subset(Data,Group.2==i)$x)
}
Data.Prop$cause1 <- cause1
Data.Prop$cause2 <- cause2
Data.Prop$cause3 <- cause3
Data.Prop$cause4 <- cause4

Data.Prop <- subset(Data.Prop, select = c("Age","Year","Sex","Country","cause1","cause2",
                                          "cause3","cause4"))

Data.Prop$Total <- Data.Prop$cause1+Data.Prop$cause2+Data.Prop$cause3+Data.Prop$cause4
Data.Prop <- as.data.frame(Data.Prop)
row.names(Data.Prop) <- 1:nrow(Data.Prop)
#Ready to calculate proportions by age, the stucture that will be applied to 5 year data

Data.Prop$P1 <- Data.Prop$cause1/Data.Prop$Total
Data.Prop$P2 <- Data.Prop$cause2/Data.Prop$Total
Data.Prop$P3 <- Data.Prop$cause3/Data.Prop$Total
Data.Prop$P4 <- Data.Prop$cause4/Data.Prop$Total
Data.Prop$Pt <- Data.Prop$P1+Data.Prop$P2+Data.Prop$P3+Data.Prop$P4
sum(Data.Prop$Pt)

Data.Prop$P1[Data.Prop$Age==85] <- 0
Data.Prop$P2[Data.Prop$Age==85] <- 0
Data.Prop$P3[Data.Prop$Age==85] <- 0
Data.Prop$P4[Data.Prop$Age==85] <- 0
Data.Prop$P5 <-0
Data.Prop$P5[Data.Prop$Age==85] <- 1

COD.structure <- subset(Data.Prop, select=c("Age","Year","Sex","Country","P1","P2","P3","P4","P5"))
#chech consistency
gdata::keep(COD.structure,sure=T)


save(COD.structure,file="Data/CoD_Structure_Grigorev.RData")




###### I need data in 5 year age groups 

################ Ready to calculate age-specific mortality rates
### Polonia information is until 2009
load("Data/CoD_Structure_Grigorev.RData")
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
      M1 <- as.matrix(D[,5:9])
      v1 <- matrix(c(rep(0,20),rep(1,5)),nrow = 5,ncol = 5)
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



save(Mx.Cause,file="Data/mx_causes_Grigorev.RData")











######################### Decompososition of LE
load("Data/mx_causes_Grigorev.RData")
source("R/edag_decomp_func.R")

D1 <- subset(Mx.Cause, Year==2003 & Sex==1 & Country=="RUS")
m1 <- as.matrix(D1[,6:10])
D2 <- subset(Mx.Cause, Year==2010 & Sex==1 & Country=="RUS")
m2 <- as.matrix(D2[,6:10])


le5frommxc(m2,sex="m")-le5frommxc(m1,sex="m")


############ Lets decompose by causes of death
rates1 <- m1
rates2 <- m2

CoDecomp             <- DecompContinuous_le_5(rates1 = c(rates1),rates2 = c(rates2), N=50, sex="m")
dim(CoDecomp)        <- dim(rates1)
dimnames(CoDecomp)   <- dimnames(rates1)
row.names(CoDecomp)  <- NULL
sum(CoDecomp)

CoD.Results   <- melt(CoDecomp)
CoD.Results   <- melt(CoDecomp,varnames = c("Row","Cause"))



####### Decompose everything
XYZ <- unique(Mx.Cause$Country)

CoD.Results.le <- NULL
for (k in XYZ){
  l <- 2009
  if (k=="POL") l <- 2008
  for ( i in 1:2){
    for (j in 1994:l){
      D1 <- subset(Mx.Cause, Year==j & Sex==i & Country==k)
      m1 <- as.matrix(D1[,6:10])
      D2 <- subset(Mx.Cause, Year==j+1 & Sex==i & Country==k)
      m2 <- as.matrix(D2[,6:10])
      
      if (i == 1) Sx <- "m"
      if (i == 2) Sx <- "f"
      
      CoDecomp             <- DecompContinuous_le_5(rates1 = c(m1),rates2 = c(m2), N=50, sex=Sx)
      dim(CoDecomp)        <- dim(m1)
      dimnames(CoDecomp)   <- dimnames(m1)
      row.names(CoDecomp)  <- NULL
      
      R   <- melt(CoDecomp,varnames = c("Country","Cause"))
      R$Country <- k
      R$Sex     <- Sx
      R$Age     <- rep(c(0,1,seq(5,110,5)),5)
      R$Year    <- j
      
      CoD.Results.le <- rbind(CoD.Results.le,R)
    }
  }
}
save(CoD.Results.le,file="Data/Decomp_Causes_EE_LE.RData")



###################

setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")
load("Data/Decomp_Causes_EE_LE.RData")

### Some data manipulation
Data                     <- CoD.Results.le
Data$Sex1[Data$Sex=="f"] <- "Females"
Data$Sex1[Data$Sex=="m"] <- "Males"

################# Sensitivity Analysis, compare to Grigorev et al 2014 PDR for Russia
### Periods
#1994-1998
#1998-2003
#2003-2011


Period.Label      <- c("1994-1998","1998-2003","2003-2010")
Data$Period      <- (cut(Data$Year+1, breaks=c(1994,1998,2003,Inf),labels=Period.Label))
Data.Period       <- aggregate(Data$value, by = list(Age=Data$Age,Cause=Data$Cause,Period=Data$Period,
                                                     Sex=Data$Sex1,Country=Data$Country),FUN=sum)

#########################

Data.Period$Country1[Data.Period$Country=="BLR"] <- "Belarus"
Data.Period$Country1[Data.Period$Country=="BGR"] <- "Bulgaria"
Data.Period$Country1[Data.Period$Country=="CZE"] <- "Czech Republic"
Data.Period$Country1[Data.Period$Country=="HUN"] <- "Hungary"
Data.Period$Country1[Data.Period$Country=="POL"] <- "Poland"
Data.Period$Country1[Data.Period$Country=="RUS"] <- "Russia"
Data.Period$Country1[Data.Period$Country=="SVK"] <- "Slovakia"
Data.Period$Country1[Data.Period$Country=="UKR"] <- "Ukraine"
Data.Period$Country1[Data.Period$Country=="SVN"] <- "Slovenia"
Data.Period$Country1[Data.Period$Country=="EST"] <- "Estonia"
Data.Period$Country1[Data.Period$Country=="LVA"] <- "Latvia"
Data.Period$Country1[Data.Period$Country=="LTU"] <- "Lithuania"


Labels.age      <- c("0",'1-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                     '40-44','45-49','50-54','55-59','60-64','65-69',
                     "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105-109","110+")

Data.Period$Age <- as.factor(Data.Period$Age)
levels(Data.Period$Age)<- Labels.age

levels(Data.Period$Cause)
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
#11) Above age 85

######### Sort according to Grigorev et al 2014 PDR
# HEART DISEASES
# CEREBROVASCULAR
# INJURY AND POISONING
# OTHER CAUSES

#1) Heart diseases
#2) Cerebrovascular diseases and other cardiovascular diseases
#3) Injury and poisoning
#5) Above age 85

fig.labels <- c("Heart diseases", "Cerebrovascular", "Injury and poisoning", "Other causes")

#recode causes
Data.Period$C <- 4
Data.Period$C[Data.Period$Cause=="P1"] <- 1
Data.Period$C[Data.Period$Cause=="P2"] <- 2
Data.Period$C[Data.Period$Cause=="P3"] <- 3
Data.Period$C[Data.Period$Cause=="P4"] <- 4
Data.Period$C[Data.Period$Cause=="P5"] <- 4


myColours1 <- c("blue","green","orange","yellow")

my.settings1 <- list(
  superpose.polygon=list(col=myColours1[], border="transparent"),
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)



#Results for males

#1994-1998
Graphic.1 <-barchart(Age ~ x |Country1, data=subset(Data.Period,Sex=="Males"&Period=="1994-1998"&Country1=="Russia"),
                      groups=C,  ylab= "Age group",xlab="Contribution (years)",
                      stack=TRUE,
                      main="Males (1994-1998)",
                      between = list(x = .5),
                      #layout = c(3, 3),
                      xlim=c(-.6,.6),
                      scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                      par.settings=my.settings1,
                      key = list(x=.1,y=.93, title="Cause of death",background="white",
                                 #key = list(space="bottom", title="Cause of death",background="white", 
                                 text=list(fig.labels)
                                 ,cex=.8,
                                 points=list(pch=19,col=myColours1[-11])),
                      panel=function(x,y,...){                      
                        panel.abline(v=seq(-.6,.6,.1),lwd=1,lty=3,col="darkgrey",...)
                        panel.abline(h=seq(1,23,2),lwd=1,lty=3,col="darkgrey",...)
                        panel.barchart(...,border="transparent",x,y)
                        panel.abline(v=0,lwd=1,lty=1,col="black",...)
                      })
Graphic.1

#pdf(file="Latex/LE_MalesDecomp_Causes_1994.pdf",width=11,height=9,pointsize=12)
#print(Graphic.Cm)
#dev.off()


#1998-2003
Graphic.2 <-barchart(Age ~ x |Country1, data=subset(Data.Period,Sex=="Males"&Period=="1998-2003"&Country1=="Russia"),
                      groups=C,  ylab= "Age group",xlab="Contribution (years)",
                      stack=TRUE,
                      main="Males (1998-2003)",
                      between = list(x = .5),
                      #layout = c(3, 3),
                     xlim=c(-.6,.6),
                      scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                      par.settings=my.settings1,
                      #key = list(x=.75,y=.93, title="Cause of death",background="white",
                                 #key = list(space="bottom", title="Cause of death",background="white", 
                       #          text=list(fig.labels)
                        #         ,cex=1,
                         #        points=list(pch=19,col=myColours1[-11])),
                      panel=function(x,y,...){                      
                        panel.abline(v=seq(-.6,.6,.1),lwd=1,lty=3,col="darkgrey",...)
                        panel.abline(h=seq(1,23,2),lwd=1,lty=3,col="darkgrey",...)
                        panel.barchart(...,border="transparent",x,y)
                        panel.abline(v=0,lwd=1,lty=1,col="black",...)
                      })
Graphic.2

#pdf(file="Latex/LE_MalesDecomp_Causes_1994.pdf",width=11,height=9,pointsize=12)
#print(Graphic.Cm)
#dev.off()

#2003-2010
Graphic.3 <-barchart(Age ~ x |Country1, data=subset(Data.Period,Sex=="Males"&Period=="2003-2010"&Country1=="Russia"),
                      groups=C,  ylab= "Age group",xlab="Contribution (years)",
                      stack=TRUE,
                      main="Males (2003-2010)",
                      between = list(x = .5),
                      #layout = c(3, 3),
                      xlim=c(-.6,.6),
                      scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                      par.settings=my.settings1,
                      #key = list(x=.75,y=.93, title="Cause of death",background="white",
                       #          #key = list(space="bottom", title="Cause of death",background="white", 
                        #         text=list(fig.labels)
                         #        ,cex=1,
                          #       points=list(pch=19,col=myColours1[-11])),
                      panel=function(x,y,...){                      
                        panel.abline(v=seq(-.6,.6,.1),lwd=1,lty=3,col="darkgrey",...)
                        panel.abline(h=seq(1,23,2),lwd=1,lty=3,col="darkgrey",...)
                        panel.barchart(...,border="transparent",x,y)
                        panel.abline(v=0,lwd=1,lty=1,col="black",...)
                      })
Graphic.3

library(gridExtra)



pdf(file="Latex/Russia_LE.pdf",width=12,height=10,pointsize=12)
grid.arrange(Graphic.1,Graphic.2,Graphic.3,ncol=2,nrow=2)
#print(Graphic.Cm)
dev.off()












################# Sensitivity Analysis, compare to Grigorev et al 2014 PDR for Estonia
setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")
load("Data/Decomp_Causes_EE_LE.RData")

### Some data manipulation
Data                     <- CoD.Results.le
Data$Sex1[Data$Sex=="f"] <- "Females"
Data$Sex1[Data$Sex=="m"] <- "Males"
### Periods
#1994-1998
#1998-2003
#2003-2011


Period.Label      <- c("1994-1995","1995-2003","2003-2010")
Data$Period      <- (cut(Data$Year+1, breaks=c(1994,1995,2003,Inf),labels=Period.Label))
Data.Period       <- aggregate(Data$value, by = list(Age=Data$Age,Cause=Data$Cause,Period=Data$Period,
                                                     Sex=Data$Sex1,Country=Data$Country),FUN=sum)

#########################

Data.Period$Country1[Data.Period$Country=="BLR"] <- "Belarus"
Data.Period$Country1[Data.Period$Country=="BGR"] <- "Bulgaria"
Data.Period$Country1[Data.Period$Country=="CZE"] <- "Czech Republic"
Data.Period$Country1[Data.Period$Country=="HUN"] <- "Hungary"
Data.Period$Country1[Data.Period$Country=="POL"] <- "Poland"
Data.Period$Country1[Data.Period$Country=="RUS"] <- "Russia"
Data.Period$Country1[Data.Period$Country=="SVK"] <- "Slovakia"
Data.Period$Country1[Data.Period$Country=="UKR"] <- "Ukraine"
Data.Period$Country1[Data.Period$Country=="SVN"] <- "Slovenia"
Data.Period$Country1[Data.Period$Country=="EST"] <- "Estonia"
Data.Period$Country1[Data.Period$Country=="LVA"] <- "Latvia"
Data.Period$Country1[Data.Period$Country=="LTU"] <- "Lithuania"


Labels.age      <- c("0",'1-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                     '40-44','45-49','50-54','55-59','60-64','65-69',
                     "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105-109","110+")

Data.Period$Age <- as.factor(Data.Period$Age)
levels(Data.Period$Age)<- Labels.age

levels(Data.Period$Cause)
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
#11) Above age 85

######### Sort according to Grigorev et al 2014 PDR
# HEART DISEASES
# CEREBROVASCULAR
# INJURY AND POISONING
# OTHER CAUSES

#1) Heart diseases
#2) Cerebrovascular diseases and other cardiovascular diseases
#3) Injury and poisoning
#5) Above age 85

fig.labels <- c("Heart diseases", "Cerebrovascular", "Injury and poisoning", "Other causes")

#recode causes
Data.Period$C <- 4
Data.Period$C[Data.Period$Cause=="P1"] <- 1
Data.Period$C[Data.Period$Cause=="P2"] <- 2
Data.Period$C[Data.Period$Cause=="P3"] <- 3
Data.Period$C[Data.Period$Cause=="P4"] <- 4
Data.Period$C[Data.Period$Cause=="P5"] <- 4


myColours1 <- c("blue","green","orange","yellow")

my.settings1 <- list(
  superpose.polygon=list(col=myColours1[], border="transparent"),
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)



#Results for males

#1995-2003
Graphic.1E <-barchart(Age ~ x |Country1, data=subset(Data.Period,Sex=="Males"&Period=="1995-2003"&Country1=="Estonia"),
                     groups=C,  ylab= "Age group",xlab="Contribution (years)",
                     stack=TRUE,
                     main="Males (1995-2003)",
                     between = list(x = .5),
                     #layout = c(3, 3),
                     xlim=c(-.6,.6),
                     scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                     par.settings=my.settings1,
                     key = list(x=.1,y=.93, title="Cause of death",background="white",
                                #key = list(space="bottom", title="Cause of death",background="white", 
                                text=list(fig.labels)
                                ,cex=.8,
                                points=list(pch=19,col=myColours1[-11])),
                     panel=function(x,y,...){                      
                       panel.abline(v=seq(-.6,.6,.1),lwd=1,lty=3,col="darkgrey",...)
                       panel.abline(h=seq(1,23,2),lwd=1,lty=3,col="darkgrey",...)
                       panel.barchart(...,border="transparent",x,y)
                       panel.abline(v=0,lwd=1,lty=1,col="black",...)
                     })
Graphic.1E

pdf(file="Latex/LE_Estonia.pdf",width=6,height=6,pointsize=12)
print(Graphic.1E)
dev.off()
