library(data.table)
library(reshape2)
library(latticeExtra)
library(ggplot2)
library(ggthemes)

setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe-2017")

load("Data/HMD_Data.RData")
source("R/Functions.R")

review.BLR <- HMDL[HMDL$PopName == 'BLR' & HMDL$Sex == 'm' & HMDL$Year %in% c(1960,1994,2005,2014),]
review.BLR$lx <-review.BLR$lx/100000

Eastern_HMDL             <- HMDL[HMDL$PopName %in% Country.HMD.vec & HMDL$Year >= 1960,]
Eastern_HMDL$Country     <- Country.name.vec[Eastern_HMDL$PopName]
Eastern_HMDL$Sex         <- as.factor(Eastern_HMDL$Sex)
levels(Eastern_HMDL$Sex) <- Sexes

unique(Eastern_HMDL$Year)

# Life expectancy at age 5 trends --------------------------------------------------
e5 <- Eastern_HMDL[Eastern_HMDL$Age == 5,]
e0 <- Eastern_HMDL[Eastern_HMDL$Age == 0,]


e5.m <- ggplot(e5[e5$Sex=='Male'], aes(Year, ex,colour=Country)) +
  ggtitle( expression(paste('Life expectancy (',e[5],')')) , subtitle = 'Males')+
  annotate('rect', xmin=1987, xmax=1989, ymin=-Inf, ymax=Inf,
           alpha=0.2,fill="red")+
  geom_line(aes(group = Country,size= Country),show.legend = F) +
  scale_size_manual(values =lwd.fig.F1b,guide = FALSE)+
  scale_x_continuous('Year', expand = c(0, 0)) +
  #scale_y_continuous('Years', limits = c(50,85)) +
  theme_fivethirtyeight(base_size = 18)+ 
  geom_vline(aes(xintercept=1991.8),show.legend = F,linetype=2,lwd=1.5, colour="blue")+
  scale_colour_manual('Country', values = col.fig.F1b) +
  theme(text = element_text(size=18),legend.key.size = unit(.4, "in"),axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA),
        legend.position="bottom")+
  geom_line(aes(group = Country,size= Country),show.legend = T)
e5.m

e0.m <- ggplot(e0[e0$Sex=='Male'], aes(Year, ex,colour=Country)) +
  ggtitle(expression(paste('Life expectancy (',e[0],')')), subtitle = 'Males')+
  annotate('rect', xmin=1987, xmax=1989, ymin=-Inf, ymax=Inf,
           alpha=0.2,fill="red")+
  geom_line(aes(group = Country,size= Country),show.legend = F) +
  scale_size_manual(values =lwd.fig.F1b,guide = FALSE)+
  scale_x_continuous('Year', expand = c(0, 0)) +
  #scale_y_continuous('Years', limits = c(68,85)) +
  theme_fivethirtyeight(base_size = 18)+ 
  geom_vline(aes(xintercept=1991.8),show.legend = F,linetype=2,lwd=1.5, colour="blue")+
  scale_colour_manual('Country', values = col.fig.F1b) +
  theme(text = element_text(size=18),legend.key.size = unit(.4, "in"),axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA),
        legend.position="bottom")+
  geom_line(aes(group = Country,size= Country),show.legend = T)
e0.m

require(gridExtra)
pdf(file="Outcomes/For the review/e5_0_males.pdf",width=20,height=10,pointsize=2,useDingbats = F)
grid.arrange(e5.m,e0.m,ncol=2)
dev.off()





# Life disparity trends at age 5---------------------------------------------------

Eastern_HMDL$dx   <-Eastern_HMDL$dx/100000
Eastern_HMDL$lx   <-Eastern_HMDL$lx/100000

################ Calculate edagger

edagger5 <- Eastern_HMDL[,list(ed = e.dagger.LT.5X(fx=dx,ex=ex,ax=ax,lx = lx)), by = list(Year,Sex,PopName,Country)]

edagger0 <- Eastern_HMDL[,list(ed = e.dagger.LT(fx=dx,ex=ex,ax=ax)), by = list(Year,Sex,PopName,Country)]


ed5.m <- ggplot(edagger5[edagger5$Sex=='Male'], aes(Year, ed,colour=Country)) +
  ggtitle(expression(paste('Life disparity (',e[5]^"\u2020",')')), subtitle = 'Males')+
  annotate('rect', xmin=1987, xmax=1989, ymin=-Inf, ymax=Inf,
           alpha=0.2,fill="red")+
  geom_line(aes(group = Country,size= Country),show.legend = F) +
  scale_size_manual(values =lwd.fig.F1b,guide = FALSE)+
  scale_x_continuous('Year', expand = c(0, 0)) +
  scale_y_continuous('Years', limits = c(11,19)) +
  theme_fivethirtyeight(base_size = 18)+ 
  geom_vline(aes(xintercept=1991.8),show.legend = F,linetype=2,lwd=1.5, colour="blue")+
  scale_colour_manual('Country', values = col.fig.F1b) +
  theme(text = element_text(size=18),legend.key.size = unit(.4, "in"),axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA),
        legend.position="bottom")+
  geom_line(aes(group = Country,size= Country),show.legend = T)
ed5.m

ed0.m <- ggplot(edagger0[edagger0$Sex=='Male'], aes(Year, ed,colour=Country)) +
  ggtitle(expression(paste('Life disparity (',e[0]^"\u2020",')')), subtitle = 'Males')+
  annotate('rect', xmin=1987, xmax=1989, ymin=-Inf, ymax=Inf,
           alpha=0.2,fill="red")+
  geom_line(aes(group = Country,size= Country),show.legend = F) +
  scale_size_manual(values =lwd.fig.F1b,guide = FALSE)+
  scale_x_continuous('Year', expand = c(0, 0)) +
  scale_y_continuous('Years', limits = c(11,19)) +
  theme_fivethirtyeight(base_size = 18)+ 
  geom_vline(aes(xintercept=1991.8),show.legend = F,linetype=2,lwd=1.5, colour="blue")+
  scale_colour_manual('Country', values = col.fig.F1b) +
  theme(text = element_text(size=18),legend.key.size = unit(.4, "in"),axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA),
        legend.position="bottom")+
  geom_line(aes(group = Country,size= Country),show.legend = T)
ed0.m



require(gridExtra)
pdf(file="Outcomes/For the review/ed5_males-females.pdf",width=20,height=10,pointsize=2,useDingbats = F)
grid.arrange(ed5.m,ed0.m,ncol=2)
dev.off()





#get edagger and life expectancy

Data     <- Eastern_HMDL[,list(ed = e.dagger.LT.5X(fx=dx,ex=ex,ax=ax,lx),ex=ex[6]), by = list(Year,Sex,PopName,Country)]
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


Data.dif$Period     <- (cut(Data.dif$year+1, breaks=c(1960,1981,1989,1995,2000,Inf),labels=Period.labels))
### Plots of first differences versus first differences


abs.dif.male <- ggplot(Data.dif[Data.dif$Sex == 'Male',], aes(dif.ed, dif.ex,colour=Category,group=Period)) +
  ggtitle( expression(paste('Association between changes in ',e[5],' and ',e[5]^"\u2020",', males.')) , subtitle = 'Absolute changes (years)')+
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
  scale_y_continuous( expression(paste('Changes in ',e[5])),limits = c(-3.2,3.2)) +
  scale_x_continuous( expression(paste('Changes in ',e[5]^"\u2020")),limits = c(-3.2,3.2))

abs.dif.male


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
  scale_y_continuous( expression(paste('Changes in ',e[5])),limits = c(-8.25,8.25)) +
  scale_x_continuous( expression(paste('Changes in ',e[5]^"\u2020")),limits = c(-8.25,8.25))

rel.dif.male



#### now print them

require(gridExtra)
pdf(file="Outcomes/For the review/Changes_age5.pdf",width=15,height=10,pointsize=6,useDingbats = F)
grid.arrange(abs.dif.male,rel.dif.male,nrow = 2)
dev.off()


### get proportions

# Data <- Data.dif[Data.dif$PopName == 'BLR' & Data.dif$Sex == 'Male' & Data.dif$Period == '1960-1987',] 


Proportions <- Data.dif[, get.prop.fun(Data = .SD), by = list(Sex,Period)]
Proportions <- Proportions[order(Period),]
Proportions[Proportions$Sex=='']

males <- Data.dif[Data.dif$Sex=='Male']


T1 <- t(table(males$Category2,males$Period))/colSums(table(males$Category2,males$Period))
T2 <- T1[,1]+T1[,3]
T3 <- T1[,2]+T1[,4]

T4 <- round(rbind(T2,T3)*100,2)
row.names(T4) <- rev(c('same direction (%)', 'opposite direction (%)'))

datatable(T4, options = list(paging=FALSE,ordering=T,searching=F),rownames = T)


