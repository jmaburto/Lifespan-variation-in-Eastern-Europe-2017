library(data.table)
library(reshape)
library(latticeExtra)
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

####Quick plot
F1b <- xyplot(ex ~ Year | Sex1, data=subset(Eastern_HMDL,Age==0 & Sex=="m"), type="o",
              main="Life expectancy",
             group=Country,pch=pch.fig.F1b,col=makeTransparent(col.fig.F1b,160),xlab=list("Year",cex=1.5),
             xlim=c(1959,2016),ylim=c(50,85),ylab=list("Years",cex=1.5),between=list(x=1.5),
             key = list(x=.05,y=.95, title="Country",background="white", 
                        text=list(c("Belarus","Bulgaria","Czech Republic","Estonia","Hungary","Latvia",
                                    "Lithuania","Poland","Russia","Slovakia","Slovenia","Ukraine"))
                        ,cex=1,
                        points=list(pch=pch.fig.F1b,col=col.fig.F1b)),
             par.settings=my.settings,strip=F,
             scales=list(alternating=1,x=list(cex=1.5,at=c(seq(1950,2010,10))),
                         y=list(cex=1.5,at=c(seq(50,85,5)),alternating=1)),
             panel = function(x, y, ...){            
               panel.abline(v=c(seq(1950,2010,10)),col='dark grey',lty=3)
               panel.abline(h=c(seq(50,85,5)),col='dark grey',lty=3)                  
               panel.abline(v=c(1991),col='blue',lty=1)
               panel.arrows(1980,55,1986,55,length=.1,col="black")  
               panel.text(1968,55,"Gorbachev's anti-alcohol campaign",cex=1.1)
               panel.arrows(1997,55,1991,55,length=.1,col="black")  
               panel.text(2005,55,"Soviet Union break up",cex=1.1)
               panel.rect(xleft=1985, xright=1987,ybottom=50, ytop=85,col=makeTransparent("red",90))
               panel.xyplot(x, y, ...)         
             })
F1b

F1c <- xyplot(ex ~ Year | Sex1, data=subset(Eastern_HMDL,Age==0), type="o",
             group=Country,pch=pch.fig.F1b,col=makeTransparent(col.fig.F1b,160),xlab=list("Year",cex=1.5),
             xlim=c(1959,2016),ylim=c(50,85),ylab=list("Years",cex=1.5),between=list(x=1.5),
             key = list(x=.55,y=.95, title="Country",background="white", 
                        text=list(c("Belarus","Bulgaria","Czech Republic","Estonia","Hungary","Latvia",
                                    "Lithuania","Poland","Russia","Slovakia","Slovenia","Ukraine"))
                        ,cex=1,
                        points=list(pch=pch.fig.F1b,col=col.fig.F1b)),
             par.settings=my.settings,
             scales=list(alternating=1,x=list(cex=1.5,at=c(seq(1950,2010,10))),
                         y=list(cex=1.5,at=c(seq(50,85,5)),alternating=1)),
             panel = function(x, y, ...){            
               panel.abline(v=c(seq(1950,2010,10)),col='dark grey',lty=3)
               panel.abline(h=c(seq(50,85,5)),col='dark grey',lty=3)                  
               panel.abline(v=c(1991),col='blue',lty=1)
               panel.arrows(1980,55,1986,55,length=.1,col="black")  
               panel.text(1968,55,"Gorbachev's anti-alcohol campaign",cex=.8)
               panel.arrows(1997,55,1991,55,length=.1,col="black")  
               panel.text(2005,55,"Soviet Union break up",cex=.8)
               panel.rect(xleft=1985, xright=1987,ybottom=50, ytop=85,col=makeTransparent("red",90))
               panel.xyplot(x, y, ...)         
             })
F1c

pdf(file="R/Sensitivity Analysis/F1_SS.pdf",width=17,height=8,pointsize=4)
print(F1c)
dev.off()

################ Calculate gini coefficient
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

####Quick plot


F2c <- xyplot(Gini ~ Year | Sex1, data=Gini, type="o",
             group=Country,pch=pch.fig.F1b,col=makeTransparent(col.fig.F1b,160),xlab=list("Year",cex=1.5),
             xlim=c(1959,2016),
             ylim=c(0,.3),ylab=list("Gini coefficient",cex=1.5),between=list(x=1.5),
             #key = list(x=.51,y=.95, title="Country",background="white", 
                      #  text=list(c("Belarus","Bulgaria","Czech Republic","Estonia","Hungary","Latvia",
                         #           "Lithuania","Poland","Russia","Slovakia","Slovenia","Ukraine"))
                     #   ,cex=1,
                   #     points=list(pch=pch.fig.F1b,col=col.fig.F1b)),
             par.settings=my.settings,
             scales=list(alternating=1,x=list(cex=1.5,at=c(seq(1950,2010,10))),
                         y=list(cex=1.5,at=c(seq(0,.3,.1)),alternating=1)),
             ,panel = function(x, y, ...){            
               panel.abline(v=c(seq(1950,2010,10)),col='dark grey',lty=3)
               panel.abline(h=c(seq(0,1,.1)),col='dark grey',lty=3)                  
               panel.abline(v=c(1991),col='blue',lty=1)
               #panel.arrows(1980,20,1986,20,length=.1,col="black")  
               #panel.text(1968,20,"Gorbachev's anti-alcohol campaign",cex=.95)
               #panel.arrows(1997,20,1991,20,length=.1,col="black")  
               #panel.text(2005,20,"Soviet Union break up",cex=.95)
               panel.rect(xleft=1985, xright=1987,ybottom=0, ytop=1,col=makeTransparent("red",90))
               panel.xyplot(x, y, ...)         
             })

F2c

require(gridExtra)
pdf(file="R/Sensitivity Analysis/F1_SS.pdf",width=14,height=14,pointsize=4)
grid.arrange(F1c,F2c, nrow=2)
dev.off()





