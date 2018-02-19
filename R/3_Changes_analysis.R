################# Relative changes analysis
library(data.table)
library(reshape2)
library(latticeExtra)
library(ggplot2)
library(ggthemes)
library(ecp)

setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe-2017")

load("Data/HMD_Data.RData")
source("R/Functions.R")
unique(HMDL$PopName)


Eastern_HMDL             <- HMDL[HMDL$PopName %in% Country.HMD.vec & HMDL$Year >= 1960,]
Eastern_HMDL$Country     <- Country.name.vec[Eastern_HMDL$PopName]
Eastern_HMDL$Sex         <- as.factor(Eastern_HMDL$Sex)
levels(Eastern_HMDL$Sex) <- Sexes
Eastern_HMDL             <- Eastern_HMDL[with(Eastern_HMDL,order(Country,Sex,Year,Age)),]

#get edagger and life expectancy
Data     <- Eastern_HMDL[,list(ed = e.dagger.LT(fx=dx/100000,ex=ex,ax=ax),ex=ex[1]), by = list(Year,Sex,PopName,Country)]
Data.dif <- Data[,list(dif.ed = get.dif.fun(ed,relative = 1),
                       dif.ex = get.dif.fun(ex,relative = 1),
                       dif.ed.rel = get.dif.fun(ed,relative = 2)*100,
                       dif.ex.rel = get.dif.fun(ex,relative = 2)*100,
                       year = Year[-1L]),by = list(PopName,Country,Sex)]

unique(Data.dif$year)

Data.dif$Category2 <-  4
Data.dif[Data.dif$dif.ed >= 0 & Data.dif$dif.ex >= 0, ]$Category2  <-3
Data.dif[Data.dif$dif.ed < 0 & Data.dif$dif.ex < 0, ]$Category2 <- 1
Data.dif[Data.dif$dif.ed >= 0 & Data.dif$dif.ex < 0, ]$Category2 <- 2


Data.dif$Category <- 2  
Data.dif[Data.dif$dif.ed >= 0 & Data.dif$dif.ex >= 0, ]$Category <- 1
Data.dif[Data.dif$dif.ed < 0 & Data.dif$dif.ex < 0, ]$Category <-   1
Data.dif[Data.dif$Country=='Russia', ]$Category <- 3
Data.dif$Category   <- factor(Data.dif$Category,levels=c(1:3),labels=c('Negative','Positive','Russia'))

save(Data.dif, file = 'R/Sensitivity Analysis/CEE_App/Association.RData')

Data.dif$Period     <- (cut(Data.dif$year+1, breaks=c(1960,1981,1989,1995,2000,Inf),labels=Period.labels))
### Plots of first differences versus first differences


abs.dif.male <- ggplot(Data.dif[Data.dif$Sex == 'Male',], aes(dif.ed, dif.ex,colour=Category,group=Period)) +
  ggtitle( expression(paste('Association between changes in ',e[0],' and ',e^"\u2020",', males.')) , subtitle = 'Absolute changes (years)')+
  geom_vline(aes(xintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
  geom_hline(aes(yintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
  geom_point(show.legend = F,size =2.5)+
  scale_colour_manual('Country', values = c(makeTransparent("#e7657d",150),makeTransparent('gray71',100), "deepskyblue4"))+
  facet_wrap(~Period,nrow = 1)+
  #theme_minimal(base_size = 18)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                    linetype="solid"),
        axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA))+
  scale_y_continuous( expression(paste('Changes in ',e[0])),limits = c(-3.2,3.2)) +
  scale_x_continuous( expression(paste('Changes in ',e^"\u2020")),limits = c(-3.2,3.2))
  
abs.dif.male


abs.dif.female <- ggplot(Data.dif[Data.dif$Sex == 'Female',], aes(dif.ed, dif.ex,colour=Category,group=Period)) +
  ggtitle( expression(paste('Association between changes in ',e[0],' and ',e^"\u2020",'. females.')) , subtitle = 'Absolute changes (years)')+
  geom_vline(aes(xintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
  geom_hline(aes(yintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
  geom_point(show.legend = F,size =2.5)+
  scale_colour_manual('Country', values = c(makeTransparent("#e7657d",150),makeTransparent('gray71',100), "deepskyblue4"))+
  facet_wrap(~Period,nrow = 1)+
  #theme_minimal(base_size = 18)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                    linetype="solid"),
        axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA))+
  scale_y_continuous( expression(paste('Changes in ',e[0])),limits = c(-3.2,3.2)) +
  scale_x_continuous( expression(paste('Changes in ',e^"\u2020")),limits = c(-3.2,3.2))


abs.dif.female


### now for relative changes

#range(Data.dif$dif.ed.rel)

rel.dif.male <- ggplot(Data.dif[Data.dif$Sex == 'Male',], aes(dif.ed.rel, dif.ex.rel,colour=Category,group=Period)) +
  ggtitle( ' ' , subtitle = 'Relative changes (%)')+
  geom_vline(aes(xintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
  geom_hline(aes(yintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
  geom_point(show.legend = F,size =2.5)+
  scale_colour_manual('Country', values = c(makeTransparent("#e7657d",150),makeTransparent('gray71',100), "deepskyblue4"))+
  facet_wrap(~Period,nrow = 1)+
  #theme_minimal(base_size = 18)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                    linetype="solid"),
        axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA))+
  scale_y_continuous( expression(paste('Changes in ',e[0])),limits = c(-8.25,8.25)) +
  scale_x_continuous( expression(paste('Changes in ',e^"\u2020")),limits = c(-8.25,8.25))

rel.dif.male


rel.dif.female <- ggplot(Data.dif[Data.dif$Sex == 'Female',], aes(dif.ed.rel, dif.ex.rel,colour=Category,group=Period)) +
  ggtitle( ' ' , subtitle = 'Females, relative changes (%)')+
  geom_vline(aes(xintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
  geom_hline(aes(yintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
  geom_point(show.legend = F,size =2.5)+
  scale_colour_manual('Country', values = c(makeTransparent("#e7657d",150),makeTransparent('gray71',100), "deepskyblue4"))+
  facet_wrap(~Period,nrow = 1)+
  #theme_minimal(base_size = 18)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                    linetype="solid"),
        axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA))+
  scale_y_continuous( expression(paste('Changes in ',e[0])),limits = c(-8.25,8.25)) +
  scale_x_continuous( expression(paste('Changes in ',e^"\u2020")),limits = c(-8.25,8.25))

rel.dif.female


#### now print them

require(gridExtra)
pdf(file="Outcomes/changes_males.pdf",width=15,height=10,pointsize=6,useDingbats = F)
grid.arrange(abs.dif.male,rel.dif.male,nrow = 2)
dev.off()

pdf(file="Outcomes/changes_females.pdf",width=15,height=10,pointsize=6,useDingbats = F)
grid.arrange(abs.dif.female,rel.dif.female,nrow = 2)
dev.off()



### get proportions

# Data <- Data.dif[Data.dif$PopName == 'BLR' & Data.dif$Sex == 'Male' & Data.dif$Period == '1960-1987',] 

Proportions <- Data.dif[, get.prop.fun(Data = .SD), by = list(Sex,Period)]
Proportions <- Proportions[order(Period),]
Proportions[Proportions$Sex=='']

Proportions[Proportions$Sex =='Male']



#### find changes in slope

cv.countries <- Data[, list(cv=sd(x = ex)/mean(ex)),by=list(Year,Sex)]
cv.countriesed <- Data[, list(cv=sd(x = ed)/mean(ed)),by=list(Year,Sex)]

xyplot(cv ~ Year,groups = Sex,data = cv.countriesed, type='l',ylim=c(0,.11)) + xyplot(cv ~ Year,groups = Sex,data = cv.countries, type='l')

m   <- cv.countries[cv.countries$Sex=='Male',]
m1   <- matrix(m$cv)
m1.r <- e.divisive(m1,sig.lvl = 0.05,R = 199,k =4 ,min.size = 2,alpha = 1)
male.years <- m[m1.r$estimates[-length(m1.r$estimates)],]
male.years

f   <- cv.countries[cv.countries$Sex=='Female',]
f1   <- matrix(f$cv)
f1.r <- e.divisive(f1,sig.lvl = 0.05,R = 199,k = 4,min.size = 5,alpha = 1)
female.years <- f[f1.r$estimates[-length(f1.r$estimates)],]
female.years


