library(data.table)
library(reshape2)
library(latticeExtra)
library(ggplot2)
library(ggthemes)

setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe")

load("Data/HMD_Data.RData")
source("R/Functions.R")


Eastern_HMDL             <- HMDL[HMDL$PopName %in% Country.HMD.vec & HMDL$Year >= 1960,]
Eastern_HMDL$Country     <- Country.name.vec[Eastern_HMDL$PopName]
Eastern_HMDL$Sex         <- as.factor(Eastern_HMDL$Sex)
levels(Eastern_HMDL$Sex) <- Sexes



# Life expectancy trends --------------------------------------------------



ex.m <- ggplot(Eastern_HMDL[Eastern_HMDL$Age == 0 & Eastern_HMDL$Sex=='Male'], aes(Year, ex,colour=Country)) +
  ggtitle( expression(paste('Life expectancy (',e[0],')')) , subtitle = 'Males')+
  annotate('rect', xmin=1987, xmax=1989, ymin=-Inf, ymax=Inf,
           alpha=0.2,fill="red")+
  geom_line(aes(group = Country,size= Country),show.legend = F) +
  scale_size_manual(values =lwd.fig.F1b,guide = FALSE)+
  scale_x_continuous('Year', expand = c(0, 0)) +
  scale_y_continuous('Years', limits = c(55,80)) +
  theme_fivethirtyeight(base_size = 18)+ 
  geom_vline(aes(xintercept=1991.8),show.legend = F,linetype=2,lwd=1.5, colour="blue")+
  scale_colour_manual('Country', values = col.fig.F1b) +
  theme(text = element_text(size=18),legend.key.size = unit(.4, "in"),axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA),
        legend.position="bottom")
ex.m

pdf(file="Outcomes/ex_males.pdf",width=12,height=9,pointsize=6,useDingbats = F)
print(ex.m)
dev.off()

pdf(file="Outcomes/ex_males_labels.pdf",width=12,height=9,pointsize=6,useDingbats = F)
print(ex.m + geom_line(aes(group = Country,size= Country),show.legend = T))
dev.off()


ex.f <- ggplot(Eastern_HMDL[Eastern_HMDL$Age == 0 & Eastern_HMDL$Sex=='Female'], aes(Year, ex,colour=Country)) +
  ggtitle(expression(paste('Life expectancy (',e[0],')')), subtitle = 'Females')+
  annotate('rect', xmin=1987, xmax=1989, ymin=-Inf, ymax=Inf,
           alpha=0.2,fill="red")+
  geom_line(aes(group = Country,size= Country),show.legend = F) +
  scale_size_manual(values =lwd.fig.F1b,guide = FALSE)+
  scale_x_continuous('Year', expand = c(0, 0)) +
  scale_y_continuous('Years', limits = c(68,85)) +
  theme_fivethirtyeight(base_size = 18)+ 
  geom_vline(aes(xintercept=1991.8),show.legend = F,linetype=2,lwd=1.5, colour="blue")+
  scale_colour_manual('Country', values = col.fig.F1b) +
  theme(text = element_text(size=18),legend.key.size = unit(.4, "in"),axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA),
        legend.position="bottom")
ex.f

pdf(file="Outcomes/ex_females.pdf",width=12,height=9,pointsize=6,useDingbats = F)
print(ex.f)
dev.off()

pdf(file="Outcomes/ex_females_labels.pdf",width=12,height=9,pointsize=6,useDingbats = F)
print(ex.f + geom_line(aes(group = Country,size= Country),show.legend = T))
dev.off()




# Life disparity trends ---------------------------------------------------

Eastern_HMDL$dx   <-Eastern_HMDL$dx/100000

################ Calculate edagger
edagger <- Eastern_HMDL[,list(ed = e.dagger.LT(fx=dx,ex=ex,ax=ax)), by = list(Year,Sex,PopName,Country)]


ed.m <- ggplot(edagger[edagger$Sex=='Male'], aes(Year, ed,colour=Country)) +
  ggtitle(expression(paste('Life disparity (',e^"\u2020",')')), subtitle = 'Males')+
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
        legend.position="bottom")
ed.m

pdf(file="Outcomes/ed_males.pdf",width=12,height=9,pointsize=6,useDingbats = F)
print(ed.m)
dev.off()

pdf(file="Outcomes/ed_males_labels.pdf",width=12,height=9,pointsize=6,useDingbats = F)
print(ed.m + geom_line(aes(group = Country,size= Country),show.legend = T))
dev.off()


ed.f <- ggplot(edagger[edagger$Sex=='Female'], aes(Year, ed,colour=Country)) +
  ggtitle(expression(paste('Life disparity (',e^"\u2020",')')), subtitle = 'Females')+
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
        legend.position="bottom")

pdf(file="Outcomes/ed_females.pdf",width=12,height=9,pointsize=6,useDingbats = F)
print(ed.f)
dev.off()

pdf(file="Outcomes/ed_females_labels.pdf",width=12,height=9,pointsize=6,useDingbats = F)
print(ed.f + geom_line(aes(group = Country,size= Country),show.legend = T))
dev.off()
