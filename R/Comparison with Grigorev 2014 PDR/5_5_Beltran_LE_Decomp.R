############ Comparison using Beltras-Sanchez et al 2008
library(reshape2)
library(latticeExtra)
library(cause.decomp)

setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")
#source("R/edag_decomp_func.R")
######################### Decompososition of LE
load("Data/mx_causes_Grigorev.RData")

##########some functions to decompose
BS.Decomp <- function (nMx1, nMx2, Rdx, max.age, sex, length.const.mort) 
{
  t1.lt <- lifetab.grad.nax.nLxappr(nMx = rowSums(nMx1), Rdx, 
                                    max.age, sex, length.const.mort)
  t2.lt <- lifetab.grad.nax.nLxappr(nMx = rowSums(nMx2), Rdx, 
                                    max.age, sex, length.const.mort)
  x <- t1.lt$x
  row <- length(x)
  n <- c(1, 4, rep(5, row - 3), length.const.mort)
  age.grp <- x
  nms <- colnames(nMx1)
  dime <- dim(nMx1)
  t1.alt <- assoc.lt(Data = nMx1, Rdx, max.age, sex, length.const.mort)
  t2.alt <- assoc.lt(Data = nMx2, Rdx, max.age, sex, length.const.mort)
  t1.cdlt <- matrix(0, nrow = dime[1], ncol = dime[2])
  t2.cdlt <- matrix(0, nrow = dime[1], ncol = dime[2])
  for (j in 1:dime[2]) {
    for (i in 1:dime[1]) {
      t1.cdlt[i, j] = (t1.lt$nLx[i]/t1.alt[i, j]) * n[i]
      t2.cdlt[i, j] = (t2.lt$nLx[i]/t2.alt[i, j]) * n[i]
    }
  }
  term1 <- matrix(0, dime[1], dime[2])
  for (j in 1:dime[2]) {
    for (i in 1:dime[1]) {
      term1[i, j] = (t2.alt[i, j] - t1.alt[i, j]) * ((t1.cdlt[i, 
                                                              j] + t2.cdlt[i, j])/(2 * n[i]))
    }
  }
  term1
}

D1 <- subset(Mx.Cause, Year==1998 & Sex==1 & Country=="RUS")
m1 <- as.matrix(D1[,c(6:10)])
D2 <- subset(Mx.Cause, Year==2003 & Sex==1 & Country=="RUS")
m2 <- as.matrix(D2[,c(6:10)])

Decomp.R <- BS.Decomp(nMx1=m2,nMx2=m1,Rdx=1,max.age=110,sex="male",length.const.mort=5)
sum(Decomp.R)

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
      
      if (i == 1) Sx <- "male"
      if (i == 2) Sx <- "female"
      
      CoDecomp             <- BS.Decomp(nMx1=m1,nMx2=m2,Rdx=1,max.age=110,sex=Sx,length.const.mort=5)
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


Data                     <- CoD.Results.le
Data$Sex1[Data$Sex=="female"] <- "Females"
Data$Sex1[Data$Sex=="male"] <- "Males"

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



pdf(file="Latex/Russia_LE_BS.pdf",width=12,height=10,pointsize=12)
grid.arrange(Graphic.1,Graphic.2,Graphic.3,ncol=2,nrow=2)
#print(Graphic.Cm)
dev.off()



################# Sensitivity Analysis, compare to Grigorev et al 2014 PDR for Estonia

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

pdf(file="Latex/LE_Estonia_BS.pdf",width=6,height=6,pointsize=12)
print(Graphic.1E)
dev.off()
