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


e5.f <- ggplot(e5[e5$Sex=='Female'], aes(Year, ex,colour=Country)) +
  ggtitle(expression(paste('Life expectancy (',e[5],')')), subtitle = 'Females')+
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
e5.f

require(gridExtra)
pdf(file="Outcomes/For the review/e5_males-females.pdf",width=20,height=10,pointsize=2,useDingbats = F)
grid.arrange(e5.m,e5.f,ncol=2)
dev.off()





# Life disparity trends at age 5---------------------------------------------------

Eastern_HMDL$dx   <-Eastern_HMDL$dx/100000
Eastern_HMDL$lx   <-Eastern_HMDL$lx/100000

################ Calculate edagger

edagger5 <- Eastern_HMDL[,list(ed = e.dagger.LT.5X(fx=dx,ex=ex,ax=ax,lx = lx)), by = list(Year,Sex,PopName,Country)]


ed.m <- ggplot(edagger5[edagger5$Sex=='Male'], aes(Year, ed,colour=Country)) +
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
ed.m


ed.f <- ggplot(edagger5[edagger5$Sex=='Female'], aes(Year, ed,colour=Country)) +
  ggtitle(expression(paste('Life disparity (',e[5]^"\u2020",')')), subtitle = 'Females')+
  annotate('rect', xmin=1987, xmax=1989, ymin=-Inf, ymax=Inf,
           alpha=0.2,fill="red")+
  geom_line(aes(group = Country,size= Country),show.legend = F) +
  scale_size_manual(values =lwd.fig.F1b,guide = FALSE)+
  scale_x_continuous('Year', expand = c(0, 0)) +
  scale_y_continuous('Years', expand = c(0, 0)) +
  theme_fivethirtyeight(base_size = 18)+ 
  geom_vline(aes(xintercept=1991.8),show.legend = F,linetype=2,lwd=1.5, colour="blue")+
  scale_colour_manual('Country', values = col.fig.F1b) +
  theme(text = element_text(size=18),legend.key.size = unit(.4, "in"),axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA),
        legend.position="bottom")+
  geom_line(aes(group = Country,size= Country),show.legend = T)
ed.f


require(gridExtra)
pdf(file="Outcomes/For the review/ed5_males-females.pdf",width=20,height=10,pointsize=2,useDingbats = F)
grid.arrange(ed.m,ed.f,ncol=2)
dev.off()

