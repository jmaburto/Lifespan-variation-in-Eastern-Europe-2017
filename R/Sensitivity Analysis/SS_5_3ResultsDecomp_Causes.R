library(latticeExtra)
library(reshape2)
library(plyr)
library(grid)
###########################################################################
####  Load decomposition by single year and single age and causes of death
###########################################################################

### Causes of death that I am interested, following Grigorev & Andreev 2015.
#1)Mortality amenable to alcohol consumption
#2)Infectious & respiratory diseases
#3) Cancers
#4)Circulatory diseases
#5)Diabetes
#6)Birth conditions (including maternal deaths
#7) Rest of causes
#8) mortality avobe 85
### reshape in a more managable way
setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")
load("Data/Decomp_Causes_EE.Rdata")

### Some data manipulation
Data                     <- CoD.Results
Data$Sex1[Data$Sex=="f"] <- "Females"
Data$Sex1[Data$Sex=="m"] <- "Males"


############### Create variabel with breaks in periods
Period.Label      <- c("1994-2000","2000-2010")
Data$Period      <- (cut(Data$Year+1, breaks=c(1994,2000,Inf),labels=Period.Label))
Data.Period       <- aggregate(Data$value, by = list(Age=Data$Age,Cause=Data$Cause,Period=Data$Period,
                                                  Sex=Data$Sex1,Country=Data$Country),FUN=sum)

#########################
#1)Mortality amenable to alcohol consumption
#2)Infectious & respiratory diseases
#3) Cancers
#4)Circulatory diseases
#5)Diabetes
#6)Birth conditions (including maternal deaths
#7) Rest of causes
#8) mortality avobe 85
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



myColours1 <- c("red", "blue", "orange", "green", "magenta","yellow","grey","grey")

my.settings1 <- list(
  superpose.polygon=list(col=myColours1[], border="transparent"),
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)

fig.labels <- c("Amenable to alcohol consumption","Infectious & respiratory diseases",
                "Cancers","Circulatory diseases","Diabetes","Birth conditions",
                "Rest of causes and above 85")

Graphic.Cm <-barchart(Age ~ x |Country1, data=subset(Data.Period,Sex=="Males"&Period=="1994-2000"),
                     groups=Cause,  ylab= "Age group",xlab="Contribution (years)",
                     stack=TRUE,
                     main="Males (1994-2000)",
                     between = list(x = .5),
                     layout = c(3, 3),
                     xlim=c(-.45,.3),
                     scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                     par.settings=my.settings1,
                     key = list(x=.75,y=.93, title="Cause of death",background="white",
                     #key = list(space="bottom", title="Cause of death",background="white", 
                                text=list(fig.labels)
                                ,cex=1,
                                points=list(pch=19,col=myColours1[-8])),
                     panel=function(x,y,...){                      
                       panel.abline(v=seq(-.6,.6,.1),lwd=1,lty=3,col="darkgrey",...)
                       panel.abline(h=seq(1,23,2),lwd=1,lty=3,col="darkgrey",...)
                       panel.barchart(...,border="transparent",x,y)
                       panel.abline(v=0,lwd=1,lty=1,col="black",...)
                     })
Graphic.Cm

pdf(file="Latex/MalesDecomp_Causes_1994.pdf",width=11,height=9,pointsize=12)
print(Graphic.Cm)
dev.off()



Graphic.Cm2 <-barchart(Age ~ x |Country1, data=subset(Data.Period,Sex=="Males"&Period=="2000-2010"),
                      groups=Cause,  ylab= "Age group",xlab="Contribution (years)",
                      stack=TRUE,
                      main="Males (2000-2010)",
                      between = list(x = .5),
                      layout = c(3, 3),
                      xlim=c(-.45,.3),
                      scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                      par.settings=my.settings1,
                      key = list(x=.75,y=.93, title="Cause of death",background="white",
                                 #key = list(space="bottom", title="Cause of death",background="white", 
                                 text=list(fig.labels)
                                 ,cex=1,
                                 points=list(pch=19,col=myColours1[-8])),
                      panel=function(x,y,...){                      
                        panel.abline(v=seq(-.6,.6,.1),lwd=1,lty=3,col="darkgrey",...)
                        panel.abline(h=seq(1,23,2),lwd=1,lty=3,col="darkgrey",...)
                        panel.barchart(...,border="transparent",x,y)
                        panel.abline(v=0,lwd=1,lty=1,col="black",...)
                      })
Graphic.Cm2

pdf(file="Latex/MalesDecomp_Causes_2000.pdf",width=11,height=9,pointsize=12)
print(Graphic.Cm2)
dev.off()

########### now for females
Graphic.Cf <-barchart(Age ~ x |Country1, data=subset(Data.Period,Sex=="Females"&Period=="1994-2000"),
                      groups=Cause,  ylab= "Age group",xlab="Contribution (years)",
                      stack=TRUE,
                      main="Females (1994-2000)",
                      between = list(x = .5),
                      layout = c(3, 3),
                      xlim=c(-.45,.3),
                      scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                      par.settings=my.settings1,
                      key = list(x=.75,y=.93, title="Cause of death",background="white",
                                 #key = list(space="bottom", title="Cause of death",background="white", 
                                 text=list(fig.labels)
                                 ,cex=1,
                                 points=list(pch=19,col=myColours1[-8])),
                      panel=function(x,y,...){                      
                        panel.abline(v=seq(-.6,.6,.1),lwd=1,lty=3,col="darkgrey",...)
                        panel.abline(h=seq(1,23,2),lwd=1,lty=3,col="darkgrey",...)
                        panel.barchart(...,border="transparent",x,y)
                        panel.abline(v=0,lwd=1,lty=1,col="black",...)
                      })
Graphic.Cf

pdf(file="Latex/FemalesDecomp_Causes_1994.pdf",width=11,height=9,pointsize=12)
print(Graphic.Cf)
dev.off()



Graphic.Cf2 <-barchart(Age ~ x |Country1, data=subset(Data.Period,Sex=="Females"&Period=="2000-2010"),
                       groups=Cause,  ylab= "Age group",xlab="Contribution (years)",
                       stack=TRUE,
                       main="Females (2000-2010)",
                       between = list(x = .5),
                       layout = c(3, 3),
                       xlim=c(-.45,.3),
                       scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                       par.settings=my.settings1,
                       key = list(x=.75,y=.93, title="Cause of death",background="white",
                                  #key = list(space="bottom", title="Cause of death",background="white", 
                                  text=list(fig.labels)
                                  ,cex=1,
                                  points=list(pch=19,col=myColours1[-8])),
                       panel=function(x,y,...){                      
                         panel.abline(v=seq(-.6,.6,.1),lwd=1,lty=3,col="darkgrey",...)
                         panel.abline(h=seq(1,23,2),lwd=1,lty=3,col="darkgrey",...)
                         panel.barchart(...,border="transparent",x,y)
                         panel.abline(v=0,lwd=1,lty=1,col="black",...)
                       })
Graphic.Cf2

pdf(file="Latex/FemalesDecomp_Causes_2000.pdf",width=11,height=9,pointsize=12)
print(Graphic.Cf2)
dev.off()