library(ggthemes)
library(reshape2)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(plotly)
library(grid)
###########################################################################

setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe-2017")
load("Data/Decomp_Causes_EE.RData")
source("R/Functions.R")
################# To group by 5 years age-intervals
#set ages as 0,5,10,...85
Data              <- data.table(CoD.Results)
Data$contribution <- Data$value
Data              <- Data[,c('Country','sex','year','cause','age','ind','contribution')]
unique(Data$year)
#Group into 2 periods
Data$period      <- 1
Data[Data$year >= 2000,]$period      <- 2
Data$period      <- as.factor(Data$period)
levels(Data$period) <- c('1994-2000', '2000-2010')

Data$cause2 <- as.character(Data$cause)
levels(Data$cause)
Data[Data$cause2 == "Stroke" | Data$cause2 == "IHD" | Data$cause2 == "Other Circulatory",]$cause2 <- 'Circulatory'
Data[Data$cause2 == "Birth conditions" ,]$cause2 <- 'Rest'
unique(Data$cause2)

Data <- Data[, list(contribution = sum(contribution)), by = list(Country,sex,cause2,age,ind,period)]
fake <- Data[Data$Country == 'Poland',]
fake$Country <- 'Zfake'
fake$contribution <- 0

Data <- rbind(Data,fake)

  
#1) Wholy attributable to alcohol
#2) Circulatory
#3) Transportation accidents
#4) Other external causes
#5) Infectious and respiratory diseases
#6) Cancers
#7) Rest of causes
  
### Add labels to periods
base2 <- toupper(c("#7265e7", "#e265e7", "#e7657d", "#e7bc65", "#a2e765", "#65e797", "#65c7e7"))
#plot(1:7,1:7,col=toupper(base2),pch=16,cex=5)
# determine order by eyeballing colors to causes (HT J. Schoeley)
myColours1 <- c(base2[c(3,4,5,1,7,2)],'lightgrey')

Data$cause <- as.factor(Data$cause2)
levels(Data$cause)

Data$cause <- factor(Data$cause, levels(Data$cause)[rev(c(1,3,5,7,4,2,6))])
levels(Data$cause)

CEE <- sort(c('Bulgaria','Czhech Republic','Hungary','Poland','Slovakia','Slovenia', 'fake'))
BC  <- sort(c('Estonia','Latvia','Lithuania'))
FSU <- sort(c('Belarus','Russia','Ukraine'))

Data$Region <- 1                                                                        
Data[Data$Country %in% BC,]$Region <-2
Data[Data$Country %in% FSU,]$Region <-3
Data$Region <- as.factor(Data$Region)
levels(Data$Region) <- c('CE','BC','FSU')

myColours2 <- c('gray','royalblue3','orangered')

mysubset <- unique(Data[,c('Country','Region')])
mysubset$color <- myColours2[1]
mysubset[mysubset$Region == 'FSU']$color <- myColours2[2]
mysubset[mysubset$Region == 'BC']$color <- myColours2[3]

Data$color <- myColours1[1]
Data[Data$cause == unique(Data$cause)[2]]$color <- myColours1[2]
Data[Data$cause == unique(Data$cause)[3]]$color <- myColours1[3]
Data[Data$cause == unique(Data$cause)[4]]$color <- myColours1[4]
Data[Data$cause == unique(Data$cause)[5]]$color <- myColours1[5]
Data[Data$cause == unique(Data$cause)[6]]$color <- myColours1[6]
Data[Data$cause == unique(Data$cause)[7]]$color <- myColours1[7]


mydata <-Data[Data$sex == 'Male' & Data$ind == 'ex' & Data$age != '1-4' & Data$age != '0' &
                Data$age != '105-109' & Data$age != '110+' & Data$period == '1994-2000',]


Age.males.e01 <- ggplot() +
  ggtitle( 'Age-contribution to changes in life expectancy by cause of death' , subtitle = 'Males, 1994-2000')+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + Country,nrow =3,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = age, y = contribution, fill = color,group = cause), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$cause), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),
        legend.position = c(0.84, 0.85))+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()

g = ggplotGrob(Age.males.e01)

g$layout

pos <- grepl(pattern = "panel-1-3", g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
grid.newpage()
grid.draw(g)



pdf(file="Outcomes//Cause_e0_decomp_Males_1.pdf",width=10,height=10,pointsize=6,useDingbats = F)
grid.draw(g)
dev.off()

mydata <- Data[Data$sex == 'Male' & Data$ind == 'ex' & Data$age != '1-4' & Data$age != '0' &
               Data$age != '105-109' & Data$age != '110+' & Data$period == '2000-2010',]


cause.males.e02 <- ggplot() +
  ggtitle( 'Age-contribution to changes in life expectancy by cause of death' , subtitle = 'Males, 2000-2010')+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + Country,nrow =3,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = age, y = contribution, fill = color,group = cause), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$cause), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),
        legend.position = c(0.84, 0.85))+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()

g = ggplotGrob(cause.males.e02)

g$layout

pos <- grepl(pattern = "panel-1-3", g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
grid.newpage()
grid.draw(g)

pdf(file="Outcomes//Cause_e0_decomp_Males_2.pdf",width=10,height=10,pointsize=6,useDingbats = F)
grid.draw(g)
dev.off()


mydata <-Data[Data$sex == 'Female' & Data$ind == 'ex' & Data$age != '1-4' & Data$age != '0' &
                Data$age != '105-109' & Data$age != '110+' & Data$period == '1994-2000',]


cause.females.e01 <- ggplot() +
  ggtitle( 'Age-contribution to changes in life expectancy by cause of death' , subtitle = 'Females, 1994-2000')+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + Country,nrow =3,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = age, y = contribution, fill = color,group = cause), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$cause), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),
        legend.position = c(0.84, 0.85))+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()

g = ggplotGrob(cause.females.e01)

g$layout

pos <- grepl(pattern = "panel-1-3", g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
grid.newpage()
grid.draw(g)


pdf(file="Outcomes//Cause_e0_decomp_Females_1.pdf",width=10,height=10,pointsize=6,useDingbats = F)
grid.draw(g)
dev.off()

mydata <- Data[Data$sex == 'Female' & Data$ind == 'ex' & Data$age != '1-4' & Data$age != '0' &
                 Data$age != '105-109' & Data$age != '110+' & Data$period == '2000-2010',]

cause.females.e02 <- ggplot() +
  ggtitle( 'Age-contribution to changes in life expectancy by cause of death' , subtitle = 'Females, 2000-2010')+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + Country,nrow =3,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = age, y = contribution, fill = color,group = cause), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$cause), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),
        legend.position = c(0.84, 0.85))+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()

g = ggplotGrob(cause.females.e02)

g$layout

pos <- grepl(pattern = "panel-1-3", g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
grid.newpage()
grid.draw(g)


pdf(file="Outcomes//Cause_e0_decomp_Females_2.pdf",width=10,height=10,pointsize=6,useDingbats = F)
grid.draw(g)
dev.off()

############################################################################

mydata <- Data[Data$sex == 'Male' & Data$ind == 'ed' & Data$age != '1-4' & Data$age != '0' &
                 Data$age != '105-109' & Data$age != '110+' & Data$period == '1994-2000',]


cause.males.ed1 <- ggplot() +
  ggtitle( 'Age-contribution to changes in life disparity by cause of death' , subtitle = 'Males, 1994-2000')+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + Country,nrow =3,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = age, y = contribution, fill = color,group = cause), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$cause), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),
        legend.position = c(0.84, 0.85))+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()

g = ggplotGrob(cause.males.ed1)

g$layout

pos <- grepl(pattern = "panel-1-3", g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
grid.newpage()
grid.draw(g)


pdf(file="Outcomes//Cause_ed_decomp_Males_1.pdf",width=10,height=10,pointsize=6,useDingbats = F)
grid.draw(g)
dev.off()

mydata <- Data[Data$sex == 'Male' & Data$ind == 'ed' & Data$age != '1-4' & Data$age != '0' &
                 Data$age != '105-109' & Data$age != '110+' & Data$period == '2000-2010',]

cause.males.ed2 <- ggplot() +
  ggtitle( 'Age-contribution to changes in life disparity by cause of death' , subtitle = 'Males, 2000-2010')+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + Country,nrow =3,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = age, y = contribution, fill = color,group = cause), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$cause), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),
        legend.position = c(0.84, 0.85))+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()

g = ggplotGrob(cause.males.ed2)

g$layout

pos <- grepl(pattern = "panel-1-3", g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
grid.newpage()
grid.draw(g)


pdf(file="Outcomes//Cause_ed_decomp_Males_2.pdf",width=10,height=10,pointsize=6,useDingbats = F)
grid.draw(g)
dev.off()


mydata <-Data[Data$sex == 'Female' & Data$ind == 'ed' & Data$age != '1-4' & Data$age != '0' &
                Data$age != '105-109' & Data$age != '110+' & Data$period == '1994-2000',]



cause.females.ed1 <- ggplot() +
  ggtitle( 'Age-contribution to changes in life disparity by cause of death' , subtitle = 'Females, 1994-2000')+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + Country,nrow =3,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = age, y = contribution, fill = color,group = cause), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$cause), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),
        legend.position = c(0.84, 0.85))+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()

g = ggplotGrob(cause.females.ed1)

g$layout

pos <- grepl(pattern = "panel-1-3", g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
grid.newpage()
grid.draw(g)


pdf(file="Outcomes//Cause_ed_decomp_Females_1.pdf",width=10,height=10,pointsize=6,useDingbats = F)
grid.draw(g)
dev.off()



mydata <-Data[Data$sex == 'Female' & Data$ind == 'ed' & Data$age != '1-4' & Data$age != '0' &
                Data$age != '105-109' & Data$age != '110+' & Data$period == '2000-2010',]



cause.females.ed2 <- ggplot() +
  ggtitle( 'Age-contribution to changes in life disparity by cause of death' , subtitle = 'Females, 2000-2010')+
  geom_rect(data = mysubset, aes(fill=color),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.1)+
  facet_wrap(~Region + Country,nrow =3,labeller = label_wrap_gen(multi_line=FALSE))+
  geom_bar(data = mydata, aes(x = age, y = contribution, fill = color,group = cause), stat = "identity",position = "stack")+
  scale_fill_identity('Period',guide = 'legend', labels = unique(mydata$cause), breaks = myColours1)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.title = element_text(),rect = element_rect(fill = 'white', linetype = 0, colour = NA),
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 00),
        title =element_text(size=10, face='bold'),
        text = element_text(size=14),strip.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 8),
        legend.position = c(0.84, 0.85))+
  guides(fill=guide_legend(ncol=1),size = guide_legend(title.position="top", title.hjust = 0.5))+
  geom_hline(yintercept = 0)+
  coord_flip()

g = ggplotGrob(cause.females.ed2)

g$layout

pos <- grepl(pattern = "panel-1-3", g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
grid.newpage()
grid.draw(g)

pdf(file="Outcomes//Cause_ed_decomp_Females_2.pdf",width=10,height=10,pointsize=6,useDingbats = F)
grid.draw(g)
dev.off()



#### summary table for edagger
table.data <- Data[,round(sum(contribution),2), by = list(Country,sex,cause,ind,period)]

males1 <- table.data[table.data$sex=='Male' & table.data$period =='1994-2000' & table.data$ind == 'ed',]
table1 <- dcast(males1, Country ~ cause+ind,value.var = 'V1')
males2 <- table.data[table.data$sex=='Male' & table.data$period =='2000-2010' & table.data$ind == 'ed',]
t2     <- dcast(males2, Country ~ cause+ind+period,value.var = 'V1')
table1 <- cbind(table1,t2[,-1])
table1$sex <- 'Males'




females1 <- table.data[table.data$sex=='Female' & table.data$period =='1994-2000' & table.data$ind == 'ed',]
table12  <- dcast(females1, Country ~ cause+ind,value.var = 'V1')
females2 <- table.data[table.data$sex=='Female' & table.data$period =='2000-2010' & table.data$ind == 'ed',]
t22      <- dcast(females2, Country ~ cause+ind+period,value.var = 'V1')
table12 <- cbind(table12,t22[,-1])
table12$sex <- 'Females'

table1.ed <- rbind(table1,table12)


write.csv(table1.ed,file = 'Outcomes/Table_ed.csv',quote = F)

library(xtable)
xtable1 <- table1[,c(1,rev(2:8))]

xtable2 <- table1[,c(1,rev(9:15))]
names(xtable2) <- names(xtable1)

c1 <- c('1994-2000', rep('',7),'2000-2010', rep('',7))
latex.table <- rbind(xtable1,xtable2)
latex.table<- cbind(Period=c1,latex.table)


'Cause-specific contributions to the change in $e^\dagger$ for males, 1994-2000 \& 2000-2010'
rownames(latex.table) <- NULL
latex.table$Total <- rowSums(latex.table[,3:9])
print(xtable(latex.table),include.rownames=F)

#### summary table for ex
table.data <- Data[,round(sum(contribution),2), by = list(Country,sex,cause,ind,period)]

males1 <- table.data[table.data$sex=='Male' & table.data$period =='1994-2000' & table.data$ind == 'ex',]
table1 <- dcast(males1, Country ~ cause+ind,value.var = 'V1')
males2 <- table.data[table.data$sex=='Male' & table.data$period =='2000-2010' & table.data$ind == 'ex',]
t2     <- dcast(males2, Country ~ cause+ind+period,value.var = 'V1')
table1 <- cbind(table1,t2[,-1])
table1$sex <- 'Males'

females1 <- table.data[table.data$sex=='Female' & table.data$period =='1994-2000' & table.data$ind == 'ex',]
table12  <- dcast(females1, Country ~ cause+ind,value.var = 'V1')
females2 <- table.data[table.data$sex=='Female' & table.data$period =='2000-2010' & table.data$ind == 'ex',]
t22      <- dcast(females2, Country ~ cause+ind+period,value.var = 'V1')
table12 <- cbind(table12,t22[,-1])
table12$sex <- 'Females'

table1.ex <- rbind(table1,table12)

write.csv(table1.ex,file = 'Outcomes/Table_ex.csv',quote = F)
