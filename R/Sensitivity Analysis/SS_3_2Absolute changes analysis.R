################# Relative changes analysis
library(data.table)
library(reshape)
library(latticeExtra)
library(HMDHFDplus)
library(ggplot2)
library(mgcv)
library(xtable)
library(grid)
library(RColorBrewer)
setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")

load("Data/HMD_Data.RData")
source("R/func.R")

unique(HMDL$PopName)

#### Select countries from eastern Europe
Eastern_HMDL   <- subset(HMDL,(PopName=="BLR" | PopName=="BGR" | PopName=="CZE" | 
                                 PopName=="HUN"| PopName=="POL"| PopName=="RUS" | 
                                 PopName=="SVK" | PopName=="UKR" | PopName=="SVN" 
                               | PopName=="EST" | PopName=="LVA" | PopName=="LTU")&Year>=1960)

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



################ Calculate Gini
source("R/Sensitivity Analysis/gini_decomp_func.R")

Data               <- data.table(Eastern_HMDL)
Gini            <- Data[, gini.func(mx=mx,sex=Sex[1]) , by = list(PopName,Sex,Year)]
Gini            <- as.data.frame(Gini)
Gini            <- rename(Gini,c(V1="Gini"))

Gini$Country[Gini$PopName=="BLR"] <- "Belarus"
Gini$Country[Gini$PopName=="BGR"] <- "Bulgaria"
Gini$Country[Gini$PopName=="CZE"] <- "Czech Republic"
Gini$Country[Gini$PopName=="HUN"] <- "Hungary"
Gini$Country[Gini$PopName=="POL"] <- "Poland"
Gini$Country[Gini$PopName=="RUS"] <- "Russia"
Gini$Country[Gini$PopName=="SVK"] <- "Slovakia"
Gini$Country[Gini$PopName=="UKR"] <- "Ukraine"
Gini$Country[Gini$PopName=="SVN"] <- "Slovenia"
Gini$Country[Gini$PopName=="EST"] <- "Estonia"
Gini$Country[Gini$PopName=="LVA"] <- "Latvia"
Gini$Country[Gini$PopName=="LTU"] <- "Lithuania"

Gini$Sex1[Gini$Sex=="f"] <- "Females"
Gini$Sex1[Gini$Sex=="m"] <- "Males"


######################### Running analysis with first absolute differences
Data1              <- data.table(subset(Eastern_HMDL, Age==0))
Data2              <- data.table(Gini)

Years              <-    Data1[, Y(Year = Year,lag.2 = 1), by = list(PopName,Sex)]
dif.e0             <-    Data1[, absolute.change(x = ex), by = list(PopName,Sex)]
dif.ed             <-    Data2[, absolute.change(x = Gini), by = list(PopName,Sex)]

Years             <- as.data.frame(Years)
Years             <- rename(Years,c(V1="Year"))
dif.e0            <- as.data.frame(dif.e0)
dif.e0            <- rename(dif.e0,c(V1="rel.change"))
dif.ed            <- as.data.frame(dif.ed)
dif.ed            <- rename(dif.ed,c(V1="rel.change"))

dif.e0$Year      <- Years$Year
dif.ed$Year      <- Years$Year

All.changes <- rbind(dif.e0,dif.ed)

dif.e0            <- rename(dif.e0,c(rel.change="rel.e0"))
dif.ed            <- rename(dif.ed,c(rel.change="rel.ed"))

Data.changes        <-dif.e0
Data.changes$rel.ed <- dif.ed$rel.ed

gdata::keep(dif.e0,dif.ed,Eastern_HMDL,Data.changes,sure=T) 
source("R/Func.R")

###################### Plot with 1st differences
###################### First e dagger
Data.changes$Period <- 0
Data.changes$Period[Data.changes$Year>=1960 & Data.changes$Year<=1987] <- 1
Data.changes$Period[Data.changes$Year>=1988 & Data.changes$Year<=1994] <- 2
Data.changes$Period[Data.changes$Year>=1995] <- 3
Data.changes$Period <- factor(Data.changes$Period,levels=c(1:3),labels=c("1960-1987","1988-1995","1996 onwards"))
Data.changes$Sex1[Data.changes$Sex=="f"] <- "Females"
Data.changes$Sex1[Data.changes$Sex=="m"] <- "Males"

F1.rel <- xyplot(rel.e0 ~ rel.ed|Sex1,data=Data.changes,groups=Period,
                main=list("a) Life disparity vs life expectancy (relative changes)",cex=1.5),
                pch=19,col=mycol.1,cex=1,   
                #xlim=c(-.1,.1),
                ylab=list("Relative change in life expectancy",cex=1.6),       
                #ylim=c(-.1,.1),
                xlab=list("Relative change in life disparity",cex=1.6),
                par.settings=my.settings1.1,
                scales=list(tck=c(1,0),cex=1.5),
                key = list(x=.75,y=.95, title="Period",background="white", 
                           text=list(c("1960-1987","1988-1995","1996 onwards"))
                         ,cex=1.5,
                        points=list(pch=19,col=c("red","lightgreen","blue"))),                           
                panel = function(x, y, ...){   
                  panel.abline(v=c(0),col='black',lty=2)         
                  panel.abline(h=c(0),col='black',lty=2)           
                  lims <- current.panel.limits()
                  #panel.text(-.048,-5,paste("Correlation Coefficient",as.character(round(mx.CC,1)),sep=" "),cex=1.5)
                  panel.xyplot(x, y, ...)
                  panel.abline(h=lims$ylim[1],v=lims$xlim[1],col="black")                  
                })
F1.rel

#pdf(file="Latex/F4.pdf",width=15,height=8,pointsize=6)
#print(F1.rel)
#dev.off()


############################################################### Proportions For males
DataChanges <- Data.changes

Data.changes <- subset(DataChanges,Sex=="m")
Data.changes$Dif.2         <- 1
Data.changes$Dif.2[Data.changes$rel.e0 <0 & Data.changes$rel.ed <0 ] <- 2
Data.changes$Dif.2[Data.changes$rel.e0 >0 & Data.changes$rel.ed >0 ] <- 2

Data.changes$Dif         <- 1
Data.changes$Dif[Data.changes$rel.e0 <0 & Data.changes$rel.ed <0 ] <- 2

Prop.1 <- subset(Data.changes, Period=="1960-1987")
P1 <- round(ftable(Prop.1$Dif)[2]/sum(ftable(Prop.1$Dif))*100,2)
SE.P1 <- sqrt(P1/100*(1-P1/100)/sum(ftable(Prop.1$Dif)))
P1.CI <-  paste("CI:(",round((P1/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P1-.5/sum(ftable(Prop.1$Dif)))*100,2),",",
                round((P1/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P1+.5/sum(ftable(Prop.1$Dif)))*100,2),")",sep="")

Prop.2 <- subset(Data.changes, Period=="1988-1995")
P2 <- round(ftable(Prop.2$Dif)[2]/sum(ftable(Prop.2$Dif))*100,2)
SE.P2 <- sqrt(P2/100*(1-P2/100)/sum(ftable(Prop.2$Dif)))
P2.CI <-  paste("CI:(",round((P2/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P2-.5/sum(ftable(Prop.2$Dif)))*100,2),",",
                round((P2/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P2+.5/sum(ftable(Prop.2$Dif)))*100,2),")",sep="")

Prop.3 <- subset(Data.changes, Period=="1996 onwards")
P3 <- round(ftable(Prop.3$Dif)[2]/sum(ftable(Prop.3$Dif))*100,2)
SE.P3 <- sqrt(P3/100*(1-P3/100)/sum(ftable(Prop.3$Dif)))
P3.CI <-  paste("CI:(",round((P3/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P3-.5/sum(ftable(Prop.3$Dif)))*100,2),",",
                round((P3/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P3+.5/sum(ftable(Prop.3$Dif)))*100,2),")",sep="")


Data.changes$Dif.1         <- 1
Data.changes$Dif.1[Data.changes$rel.e0 >0 & Data.changes$rel.ed >0 ] <- 2


Prop.1.1 <- subset(Data.changes, Period=="1960-1987")
P1.1 <- round(ftable(Prop.1.1$Dif.1)[2]/sum(ftable(Prop.1.1$Dif.1))*100,2)
SE.P1.1 <- sqrt(P1/100*(1-P1.1/100)/sum(ftable(Prop.1.1$Dif.1)))
P1.1.CI <-  paste("CI:(",round((P1.1/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P1.1-.5/sum(ftable(Prop.1.1$Dif.1)))*100,2),",",
                round((P1.1/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P1.1+.5/sum(ftable(Prop.1.1$Dif.1)))*100,2),")",sep="")

Prop.2.1 <- subset(Data.changes, Period=="1988-1995")
P2.1 <- round(ftable(Prop.2.1$Dif.1)[2]/sum(ftable(Prop.2.1$Dif.1))*100,2)
SE.P2.1 <- sqrt(P2.1/100*(1-P2.1/100)/sum(ftable(Prop.2.1$Dif.1)))
P2.1.CI <-  paste("CI:(",round((P2.1/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P2.1-.5/sum(ftable(Prop.2.1$Dif.1)))*100,2),",",
                round((P2.1/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P2.1+.5/sum(ftable(Prop.2.1$Dif.1)))*100,2),")",sep="")

Prop.3.1 <- subset(Data.changes, Period=="1996 onwards")
P3.1 <- round(ftable(Prop.3.1$Dif.1)[2]/sum(ftable(Prop.3.1$Dif.1))*100,2)
SE.P3.1 <- sqrt(P3.1/100*(1-P3.1/100)/sum(ftable(Prop.3.1$Dif.1)))
P3.1.CI <-  paste("CI:(",round((P3.1/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P3.1-.5/sum(ftable(Prop.3.1$Dif.1)))*100,2),",",
                round((P3.1/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P3.1+.5/sum(ftable(Prop.3.1$Dif.1)))*100,2),")",sep="")


#Store male's proportions
Prop.fig    <- c(paste(P1,"% ",sep=""),paste(P2,"% ",sep=""),paste(P3,"% ",sep=""))
Prop.fig.CI <- c(P1.CI,P2.CI,P3.CI)

Prop.fig.1    <- c(paste(P1.1,"% ",sep=""),paste(P2.1,"% ",sep=""),paste(P3.1,"% ",sep=""))
Prop.fig.1.CI <- c(P1.1.CI,P2.1.CI,P3.1.CI)
###########################



############################################################### Proportions For Females
Data.changes <- subset(DataChanges,Sex=="f")
Data.changes$Dif.2         <- 1
Data.changes$Dif.2[Data.changes$rel.e0 <0 & Data.changes$rel.ed <0 ] <- 2
Data.changes$Dif.2[Data.changes$rel.e0 >0 & Data.changes$rel.ed >0 ] <- 2

Data.changes$Dif         <- 1
Data.changes$Dif[Data.changes$rel.e0 <0 & Data.changes$rel.ed <0 ] <- 2

Prop.1 <- subset(Data.changes, Period=="1960-1987")
P1 <- round(ftable(Prop.1$Dif)[2]/sum(ftable(Prop.1$Dif))*100,2)
SE.P1 <- sqrt(P1/100*(1-P1/100)/sum(ftable(Prop.1$Dif)))
P1.CI <-  paste("CI:(",round((P1/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P1-.5/sum(ftable(Prop.1$Dif)))*100,2),",",
                round((P1/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P1+.5/sum(ftable(Prop.1$Dif)))*100,2),")",sep="")

Prop.2 <- subset(Data.changes, Period=="1988-1995")
P2 <- round(ftable(Prop.2$Dif)[2]/sum(ftable(Prop.2$Dif))*100,2)
SE.P2 <- sqrt(P2/100*(1-P2/100)/sum(ftable(Prop.2$Dif)))
P2.CI <-  paste("CI:(",round((P2/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P2-.5/sum(ftable(Prop.2$Dif)))*100,2),",",
                round((P2/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P2+.5/sum(ftable(Prop.2$Dif)))*100,2),")",sep="")

Prop.3 <- subset(Data.changes, Period=="1996 onwards")
P3 <- round(ftable(Prop.3$Dif)[2]/sum(ftable(Prop.3$Dif))*100,2)
SE.P3 <- sqrt(P3/100*(1-P3/100)/sum(ftable(Prop.3$Dif)))
P3.CI <-  paste("CI:(",round((P3/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P3-.5/sum(ftable(Prop.3$Dif)))*100,2),",",
                round((P3/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P3+.5/sum(ftable(Prop.3$Dif)))*100,2),")",sep="")


Data.changes$Dif.1         <- 1
Data.changes$Dif.1[Data.changes$rel.e0 >0 & Data.changes$rel.ed >0 ] <- 2


Prop.1.1 <- subset(Data.changes, Period=="1960-1987")
P1.1 <- round(ftable(Prop.1.1$Dif.1)[2]/sum(ftable(Prop.1.1$Dif.1))*100,2)
SE.P1.1 <- sqrt(P1/100*(1-P1.1/100)/sum(ftable(Prop.1.1$Dif.1)))
P1.1.CI <-  paste("CI:(",round((P1.1/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P1.1-.5/sum(ftable(Prop.1.1$Dif.1)))*100,2),",",
                  round((P1.1/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P1.1+.5/sum(ftable(Prop.1.1$Dif.1)))*100,2),")",sep="")

Prop.2.1 <- subset(Data.changes, Period=="1988-1995")
P2.1 <- round(ftable(Prop.2.1$Dif.1)[2]/sum(ftable(Prop.2.1$Dif.1))*100,2)
SE.P2.1 <- sqrt(P2.1/100*(1-P2.1/100)/sum(ftable(Prop.2.1$Dif.1)))
P2.1.CI <-  paste("CI:(",round((P2.1/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P2.1-.5/sum(ftable(Prop.2.1$Dif.1)))*100,2),",",
                  round((P2.1/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P2.1+.5/sum(ftable(Prop.2.1$Dif.1)))*100,2),")",sep="")

Prop.3.1 <- subset(Data.changes, Period=="1996 onwards")
P3.1 <- round(ftable(Prop.3.1$Dif.1)[2]/sum(ftable(Prop.3.1$Dif.1))*100,2)
SE.P3.1 <- sqrt(P3.1/100*(1-P3.1/100)/sum(ftable(Prop.3.1$Dif.1)))
P3.1.CI <-  paste("CI:(",round((P3.1/100 - qnorm(p = .975,mean = 0,sd = 1)*SE.P3.1-.5/sum(ftable(Prop.3.1$Dif.1)))*100,2),",",
                  round((P3.1/100 + qnorm(p = .975,mean = 0,sd = 1)*SE.P3.1+.5/sum(ftable(Prop.3.1$Dif.1)))*100,2),")",sep="")


#Store female's proportions
Prop.figf    <- c(paste(P1,"% ",sep=""),paste(P2,"% ",sep=""),paste(P3,"% ",sep=""))
Prop.fig.CIf <- c(P1.CI,P2.CI,P3.CI)

Prop.fig.1f    <- c(paste(P1.1,"% ",sep=""),paste(P2.1,"% ",sep=""),paste(P3.1,"% ",sep=""))
Prop.fig.1.CIf <- c(P1.1.CI,P2.1.CI,P3.1.CI)
###########################

###############################################################
##############Create a graph
DataChanges$Dif.2         <- 1
DataChanges$Dif.2[DataChanges$rel.e0 <0 & DataChanges$rel.ed <0 ] <- 2
DataChanges$Dif.2[DataChanges$rel.e0 >0 & DataChanges$rel.ed >0 ] <- 2
DataChanges$Sex1 <- as.factor(DataChanges$Sex1)

#Store female's proportions
Prop.figf
Prop.fig.CIf 

Prop.fig.1f  
Prop.fig.1.CIf 
###########################

#Store male's proportions
Prop.fig    
Prop.fig.CI 

Prop.fig.1  
Prop.fig.1.CI 
###########################
Prop.fig.T <- c(Prop.figf,Prop.fig)
Prop.fig.TCI <- c(Prop.fig.CIf,Prop.fig.CI)

Prop.fig.1T <- c(Prop.fig.1f,Prop.fig.1)
Prop.fig.1TCI <- c(Prop.fig.1.CIf,Prop.fig.1.CI)


Fig.stag <-     useOuterStrips(xyplot(rel.e0 ~ rel.ed|Period+Sex1,
                         data=DataChanges,groups=Dif.2,
                         #main=list("b) Life disparity (e-dagger)",cex=1.5),
                         strip=T,cex=1,
                         par.strip.text=list(cex=1.6),
                         #layout=c(3,1),
                         #between=list(x=1.5),
                         key = list(x=.1,y=.88, title=NULL,background="transparent", 
                                    text=list(c("Russia"))
                                    ,cex=1,
                                    points=list(pch=19,col="transparent")),
                         pch=19,col=c(makeTransparent("gray74",90),makeTransparent("tomato1",90)),
                         ylab=list("Absolute change in life expectancy",cex=1.6),
                         xlim=c(-.02,.02),
                         ylim=c(-3.5,3.5),
                         xlab=list("Absolute change in Gini coefficient",cex=1.6),
                         par.settings=my.settings2,
                         scales=list(x=list(at=seq(-.02,.02,.01),cex=1.5),
                                     y=list(at=seq(-3,3,1),cex=1.5)),
                         panel = function(x, y, ...){   
                           panel.abline(v=c(0),col='black',lty=2)         
                           panel.abline(h=c(0),col='black',lty=2)         
                           panel.text(-.01,-2,labels=Prop.fig.T[panel.number()],cex=1.2)
                           #panel.text(-.01,-2.5,labels=Prop.fig.TCI[panel.number()],cex=1.2)
                           panel.text(.01,2.5,labels=Prop.fig.1T[panel.number()],cex=1.2)
                           #panel.text(.01,2,labels=Prop.fig.1TCI[panel.number()],cex=1.2)
                           panel.xyplot(x, y, ...)                     
                         }),strip.left = T)
Fig.stag


Fig.stag1 <-     useOuterStrips(xyplot(rel.e0 ~ rel.ed|Period+Sex1,
                                      data=subset(DataChanges,PopName=="RUS"),groups=Dif.2,
                                      #main=list("b) Life disparity (e-dagger)",cex=1.5),
                                      strip=T,cex=1,
                                      par.strip.text=list(cex=1.6),
                                      #layout=c(3,1),
                                      #between=list(x=1.5),
                                      pch=19,col="black",
                                      #ylab=list("Absolute change in life expectancy",cex=1.6),
                                      #xlim=c(-3.5,3.5),
                                      #ylim=c(-3.5,3.5),
                                      #xlab=list("Absolute change in Life disparity",cex=1.6),
                                      par.settings=my.settings2,
                                      scales=list(x=list(at=seq(-3,3,1),cex=1.5),
                                                  y=list(at=seq(-3,3,1),cex=1.5)),
                                      ),strip.left = T)
Fig.stag2 <- Fig.stag+Fig.stag1
Fig.stag2

pdf(file="R/Sensitivity Analysis/F2_SS.pdf",width=13,height=9,pointsize=6)
print(Fig.stag2)
dev.off()

