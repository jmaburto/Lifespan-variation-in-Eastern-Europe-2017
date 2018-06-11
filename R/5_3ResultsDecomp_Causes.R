library(ggthemes)
library(reshape2)
library(ggplot2)
library(data.table)
library(RColorBrewer)
###########################################################################

setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe")
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

Data <- Data[, list(contribution = sum(contribution)), by = list(Country,sex,cause,age,ind,period)]
  
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
  
### Add labels to periods
base2 <- toupper(c("#7265e7", "#e265e7", "#e7657d", "#e7bc65", "#a2e765", "#65e797", "#65c7e7"))
#plot(1:7,1:7,col=toupper(base2),pch=16,cex=5)
# determine order by eyeballing colors to causes (HT J. Schoeley)
myColours1 <- c(base2[c(3,4,5,1,7,2,6)], 'lightskyblue4','pink','lightgrey')

Data$cause <- factor(Data$cause, levels=rev(levels(Data$cause)))
levels(Data$cause)

cause.males.e01 <- ggplot(Data[Data$sex == 'Male' & Data$ind == 'ex' & Data$age != '1-4' & Data$age != '0' &
                              Data$age != '105-109' & Data$age != '110+' & Data$period == '1994-2000',], 
                       aes(x = age, y = contribution, fill = cause, width=.7)) +
  ggtitle( 'Age-contribution to changes in life expectancy by cause of death' , subtitle = 'Males, 1994-2000')+
  scale_fill_manual('Cause', values = rev(myColours1)) + 
  guides(fill = guide_legend(reverse = TRUE))+
  geom_bar(aes(group = cause), stat = "identity",position = "stack")+
  facet_wrap(~Country)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
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
        axis.text.y = element_text(size = 8))+
  geom_hline(yintercept = 0)+
  coord_flip()
cause.males.e01

pdf(file="Outcomes//Cause_e0_decomp_Males_1.pdf",width=11,height=10,pointsize=6,useDingbats = F)
print(cause.males.e01)
dev.off()


cause.males.e02 <- ggplot(Data[Data$sex == 'Male' & Data$ind == 'ex' & Data$age != '1-4' & Data$age != '0' &
                                 Data$age != '105-109' & Data$age != '110+' & Data$period == '2000-2010',], 
                          aes(x = age, y = contribution, fill = cause, width=.7)) +
  ggtitle( 'Age-contribution to changes in life expectancy by cause of death' , subtitle = 'Males, 2000-2010')+
  scale_fill_manual('Cause', values = rev(myColours1)) + 
  guides(fill = guide_legend(reverse = TRUE))+
  geom_bar(aes(group = cause), stat = "identity",position = "stack")+
  facet_wrap(~Country)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
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
        axis.text.y = element_text(size = 8))+
  geom_hline(yintercept = 0)+
  coord_flip()
cause.males.e02

pdf(file="Outcomes//Cause_e0_decomp_Males_2.pdf",width=11,height=10,pointsize=6,useDingbats = F)
print(cause.males.e02)
dev.off()




cause.females.e01 <- ggplot(Data[Data$sex == 'Female' & Data$ind == 'ex' & Data$age != '1-4' & Data$age != '0' &
                                 Data$age != '105-109' & Data$age != '110+' & Data$period == '1994-2000',], 
                          aes(x = age, y = contribution, fill = cause, width=.7)) +
  ggtitle( 'Age-contribution to changes in life expectancy by cause of death' , subtitle = 'Females, 1994-2000')+
  scale_fill_manual('Cause', values = rev(myColours1)) + 
  guides(fill = guide_legend(reverse = TRUE))+
  geom_bar(aes(group = cause), stat = "identity",position = "stack")+
  facet_wrap(~Country)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
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
        axis.text.y = element_text(size = 8))+
  geom_hline(yintercept = 0)+
  coord_flip()
cause.females.e01

pdf(file="Outcomes//Cause_e0_decomp_Females_1.pdf",width=11,height=10,pointsize=6,useDingbats = F)
print(cause.females.e01)
dev.off()


cause.females.e02 <- ggplot(Data[Data$sex == 'Female' & Data$ind == 'ex' & Data$age != '1-4' & Data$age != '0' &
                                 Data$age != '105-109' & Data$age != '110+' & Data$period == '2000-2010',], 
                          aes(x = age, y = contribution, fill = cause, width=.7)) +
  ggtitle( 'Age-contribution to changes in life expectancy by cause of death' , subtitle = 'Females, 2000-2010')+
  scale_fill_manual('Cause', values = rev(myColours1)) + 
  guides(fill = guide_legend(reverse = TRUE))+
  geom_bar(aes(group = cause), stat = "identity",position = "stack")+
  facet_wrap(~Country)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
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
        axis.text.y = element_text(size = 8))+
  geom_hline(yintercept = 0)+
  coord_flip()
cause.females.e02

pdf(file="Outcomes//Cause_e0_decomp_Females_2.pdf",width=11,height=10,pointsize=6,useDingbats = F)
print(cause.females.e02)
dev.off()

############################################################################


cause.males.ed1 <- ggplot(Data[Data$sex == 'Male' & Data$ind == 'ed' & Data$age != '1-4' & Data$age != '0' &
                                 Data$age != '105-109' & Data$age != '110+' & Data$period == '1994-2000',], 
                          aes(x = age, y = contribution, fill = cause, width=.7)) +
  ggtitle( 'Age-contribution to changes in life disparity by cause of death' , subtitle = 'Males, 1994-2000')+
  scale_fill_manual('Cause of death', values = rev(myColours1)) + 
  geom_bar(aes(group = cause), stat = "identity",position = "stack")+
  facet_wrap(~Country)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
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
        axis.text.y = element_text(size = 8))+
  geom_hline(yintercept = 0)+
  theme(legend.position = c(0.85, 0.15))+
  guides(fill=guide_legend(ncol=1,reverse = TRUE,title.position = "top" ))+
  coord_flip()
cause.males.ed1

pdf(file="Outcomes//Cause_ed_decomp_Males_1.pdf",width=11,height=10,pointsize=6,useDingbats = F)
print(cause.males.ed1)
dev.off()


cause.males.ed2 <- ggplot(Data[Data$sex == 'Male' & Data$ind == 'ed' & Data$age != '1-4' & Data$age != '0' &
                                 Data$age != '105-109' & Data$age != '110+' & Data$period == '2000-2010',], 
                          aes(x = age, y = contribution, fill = cause, width=.7)) +
  ggtitle( 'Age-contribution to changes in life disparity by cause of death' , subtitle = 'Males, 2000-2010')+
  scale_fill_manual('Cause of death', values = rev(myColours1)) + 
  geom_bar(aes(group = cause), stat = "identity",position = "stack")+
  facet_wrap(~Country)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
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
        axis.text.y = element_text(size = 8))+
  geom_hline(yintercept = 0)+
  theme(legend.position = c(0.85, 0.15))+
  guides(fill=guide_legend(ncol=1,reverse = TRUE,title.position = "top" ))+
  coord_flip()
cause.males.ed2

pdf(file="Outcomes//Cause_ed_decomp_Males_2.pdf",width=11,height=10,pointsize=6,useDingbats = F)
print(cause.males.ed2)
dev.off()




cause.females.ed1 <- ggplot(Data[Data$sex == 'Female' & Data$ind == 'ed' & Data$age != '1-4' & Data$age != '0' &
                                   Data$age != '105-109' & Data$age != '110+' & Data$period == '1994-2000',], 
                            aes(x = age, y = contribution, fill = cause, width=.7)) +
  ggtitle( 'Age-contribution to changes in life disparity by cause of death' , subtitle = 'Females, 1994-2000')+
  scale_fill_manual('Cause of death', values = rev(myColours1)) + 
  geom_bar(aes(group = cause), stat = "identity",position = "stack")+
  facet_wrap(~Country)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
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
        axis.text.y = element_text(size = 8))+
  geom_hline(yintercept = 0)+
  theme(legend.position = c(0.85, 0.15))+
  guides(fill=guide_legend(ncol=1,reverse = TRUE,title.position = "top" ))+
  coord_flip()
cause.females.ed1

pdf(file="Outcomes//Cause_ed_decomp_Females_1.pdf",width=11,height=10,pointsize=6,useDingbats = F)
print(cause.females.ed1)
dev.off()


cause.females.ed2 <- ggplot(Data[Data$sex == 'Female' & Data$ind == 'ed' & Data$age != '1-4' & Data$age != '0' &
                                   Data$age != '105-109' & Data$age != '110+' & Data$period == '2000-2010',], 
                            aes(x = age, y = contribution, fill = cause, width=.7)) +
  ggtitle( 'Age-contribution to changes in life disparity by cause of death' , subtitle = 'Females, 2000-2010')+
  scale_fill_manual('Cause of death', values = rev(myColours1)) + 
  geom_bar(aes(group = cause), stat = "identity",position = "stack")+
  facet_wrap(~Country)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
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
        axis.text.y = element_text(size = 8))+
  geom_hline(yintercept = 0)+
  theme(legend.position = c(0.85, 0.15))+
  guides(fill=guide_legend(ncol=1,reverse = TRUE,title.position = "top" ))+
  coord_flip()
cause.females.ed2

pdf(file="Outcomes//Cause_ed_decomp_Females_2.pdf",width=11,height=10,pointsize=6,useDingbats = F)
print(cause.females.e02)
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
xtable1 <- table1[,1:11]
xtable1 <- xtable1[,c(1,11,8,9,10,7,4,3,5,6,2)]

xtable2 <- table1[,c(1,12:21)]
xtable2 <- xtable2[,c(1,11,8,9,10,7,4,3,5,6,2)]
names(xtable2) <- names(xtable1)
latex.table <- rbind(xtable1,xtable2)

'Cause-specific contributions to the change in $e^\dagger$ for males, 1994-2000 \& 2000-2010'
rownames(latex.table) <- NULL
latex.table$Total <- rowSums(latex.table[,2:10])
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
