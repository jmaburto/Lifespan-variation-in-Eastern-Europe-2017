library(ggthemes)
library(reshape2)
library(ggplot2)
library(data.table)
library(RColorBrewer)
###########################################################################
####  Load decomposition by single year and single age
###########################################################################

setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe-2017")
load("Data/Age_Decomp_results.Rdata")
source("R/Functions.R")
################# To group by 5 years age-intervals
#set ages as 0,5,10,...85
Data            <- data.table(Decomp.results)
Data$Age5       <- (cut(Data$age+1, breaks=c(seq(0,110,5),Inf),labels=Labels.age))
Data            <- Data[,list(Contribution = sum(Contribution)), by = list(country,sex,year,Ind,Age5)]
Data$Period     <- (cut(Data$year+1, breaks=c(1960,1981,1989,1995,2001,Inf),labels=Period.labels))
Data            <- Data[,list(Contribution = sum(Contribution)), by = list(country,sex,Period,Ind,Age5)]

unique(Data$country)
CEE <- sort(c('Bulgaria','Czhech Republic','Hungary','Poland','Slovakia','Slovenia'))
BC  <- sort(c('Estonia','Latvia','Lithuania'))
FSU <- sort(c('Belarus','Russia','Ukraine'))
  
Data$Region <- 1                                                                        
Data[Data$country %in% BC,]$Region <-2
Data[Data$country %in% FSU,]$Region <-3
Data$Region <- as.factor(Data$Region)
levels(Data$Region) <- c('CE','BC','FSU')
  
### Add labels to periods
base2 <- toupper(c("#7265e7", "#e265e7", "#e7657d", "#e7bc65", "#a2e765", "#65e797", "#65c7e7"))
#plot(1:7,1:7,col=toupper(base2),pch=16,cex=5)
# determine order by eyeballing colors to causes (HT J. Schoeley)
myColours1 <- base2[c(7,5,3,1,4)]
myColours2 <- c('gray','royalblue3','orangered')

mysubset <- unique(Data[,c('country','Region')])
mysubset$color <- myColours2[1]
mysubset[mysubset$Region == 'FSU']$color <- myColours2[2]
mysubset[mysubset$Region == 'BC']$color <- myColours2[3]

Data$color <- myColours1[1]
Data[Data$Period == unique(Data$Period)[2]]$color <- myColours1[2]
Data[Data$Period == unique(Data$Period)[3]]$color <- myColours1[3]
Data[Data$Period == unique(Data$Period)[4]]$color <- myColours1[4]
Data[Data$Period == unique(Data$Period)[5]]$color <- myColours1[5]


mydata <- Data[Data$sex == 'Male' & Data$Ind == 'e0' & Data$Age5 != '0-4' &
                 Data$Age5 != '105-109' & Data$Age5 != '110+',]

Age.males.e0 <- ggplot() +
  ggtitle( 'Age-contribution to changes in life expectancy by period' , subtitle = 'Males')+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + country,nrow =4,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = Age5, y = Contribution, fill = color,group = Period), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$Period), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),legend.position="bottom")+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()

Age.males.e0

pdf(file="Outcomes//Age_e0_decomp_Males.pdf",width=13,height=10,pointsize=6,useDingbats = F)
print(Age.males.e0)
dev.off()


mydata <- Data[Data$sex == 'Female' & Data$Ind == 'e0' & Data$Age5 != '0-4' &
                 Data$Age5 != '105-109' & Data$Age5 != '110+',]

Age.females.e0 <-  ggplot() +
  ggtitle( 'Age-contribution to changes in life expectancy by period' , subtitle = 'Females')+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + country,nrow =4,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = Age5, y = Contribution, fill = color,group = Period), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$Period), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),legend.position="bottom")+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()
Age.females.e0

pdf(file="Outcomes//Age_e0_decomp_Females.pdf",width=13,height=10,pointsize=6,useDingbats = F)
print(Age.females.e0)
dev.off()



mydata <- Data[Data$sex == 'Male' & Data$Ind == 'ed' & Data$Age5 != '0-4' &
                 Data$Age5 != '105-109' & Data$Age5 != '110+',]

Age.males.ed <- ggplot() +
  ggtitle( 'Age-contribution to changes in life disparity by period' , subtitle = expression(paste('Males, negative values decrease ',e^"\u2020", 'and positive values increase ',e^"\u2020")))+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + country,nrow =4,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = Age5, y = Contribution, fill = color,group = Period), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$Period), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),legend.position="bottom")+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()

Age.males.ed

pdf(file="Outcomes//Age_ed_decomp_Males.pdf",width=8,height=10,pointsize=6,useDingbats = F)
print(Age.males.ed)
dev.off()

mydata <- Data[Data$sex == 'Female' & Data$Ind == 'ed' & Data$Age5 != '0-4' &
                 Data$Age5 != '105-109' & Data$Age5 != '110+',]

Age.females.ed <-ggplot() +
  ggtitle( 'Age-contribution to changes in life disparity by period' , subtitle = expression(paste('Females, negative values decrease ',e^"\u2020", 'and positive values increase ',e^"\u2020")))+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + country,nrow =4,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = Age5, y = Contribution, fill = color,group = Period), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$Period), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),legend.position="bottom")+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()
Age.females.ed

pdf(file="Outcomes//Age_ed_decomp_Females.pdf",width=13,height=10,pointsize=6,useDingbats = F)
print(Age.females.ed)
dev.off()

#### Infant contributions


Infant.e0 <- ggplot(Data[Data$Ind == 'e0' & Data$Age5 == '0-4',], 
                         aes(x = country, y = Contribution, fill = Period, width=.7)) +
  ggtitle( 'Infant contribution to changes in life expectancy by period')+
  scale_fill_manual('Period', values = myColours1) + 
  geom_bar(aes(group = Period), stat = "identity",position = "stack")+
  facet_wrap(~sex)+
  labs(x = "Country", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                    linetype="solid"),
        axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA))+
  theme(axis.title.y = element_text(size = 12, angle = 90))+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 14))+
  geom_hline(yintercept = 0)+
  coord_flip()
Infant.e0

pdf(file="Outcomes//Infant_ex_decomp.pdf",width=12,height=7,pointsize=6,useDingbats = F)
print(Infant.e0)
dev.off()

Infant.ed <- ggplot(Data[Data$Ind == 'ed' & Data$Age5 == '0-4',], 
                    aes(x = country, y = Contribution, fill = Period, width=.7)) +
  ggtitle( 'Infant contribution to changes in life disparity by period')+
  scale_fill_manual('Period', values = myColours1) + 
  geom_bar(aes(group = Period), stat = "identity",position = "stack")+
  facet_wrap(~sex)+
  labs(x = "Country", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                    linetype="solid"),
        axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA))+
  theme(axis.title.y = element_text(size = 12, angle = 90))+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 14))+
  geom_hline(yintercept = 0)+
  coord_flip()
Infant.e0

pdf(file="Outcomes//Infant_ed_decomp.pdf",width=12,height=7,pointsize=6,useDingbats = F)
print(Infant.ed)
dev.off()


