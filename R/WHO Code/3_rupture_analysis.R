###############################################################################
library(data.table)
library(reshape2)
library(ggplot2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe-2017/")

# Run this line to update the COD RData file
 #source(file = 'R/WHO Code/2_Classification_COD')

# Load data from 2_Get_WHO_Data.R
get(load('Data/CEE_WHO_COD_DATA.RData'))
DT_COD.melt <- DT_COD.melt[DT_COD.melt$Year <= 2010,]

# to classify causes of death following the Centers for disease Control and Prevention, Rehm etal papers, CoD
# and the WHO documentation.

#Classification used in the paper

# 1 Attributable to alcohol
# 2 IHD
# 3 Stroke
# 4 Transportation Accidents
# 5 Other external causes
# 6 Infectious & respiratory diseases
# 7 Cancers
# 8 Other circulatory
# 9 Rest

levels(DT_COD.melt$Sex) <- c('Males', 'Females')

COD.labels <- c('Attributable to alcohol',
                'IHD',
                'Stroke',
                'Transportation Accidents',
                'Other external causes',
                'Infectious & respiratory diseases',
                'Cancers',
                'Other circulatory',
                'Rest')

DT_COD.melt$Cat <- as.factor(DT_COD.melt$Cat)
levels(DT_COD.melt$Cat) <- COD.labels

change.ICD <- DT_COD.melt[, list(Year.change=max(Year)), by = list(Country.name,ICD)]
change.ICD

Total.cause <- DT_COD.melt[,list(Total=sum(Dx)), by = list(Country.name,Year,Sex,Cat)]

#####
unique(Total.cause$Country.name)



f1 <- ggplot(Total.cause[Total.cause$Country.name==Country], aes(Year,Total))+
  ggtitle(Country,subtitle = paste0('Years of change in ICD: ', change.ICD[change.ICD$ICD==9 & change.ICD$Country.name==Country,]$Year.change+1))+
  geom_line(aes(colour = Sex), lwd=1,show.legend =T)+theme_light()+
  geom_point(aes(colour = Sex), lwd=1,show.legend =F)+
  theme(text = element_text(size=20))+
  facet_wrap(~Cat,scales = "free",ncol = 3)+ xlim(c(1994, 2010))+
  geom_vline(data=change.ICD[change.ICD$ICD==9 & change.ICD$Country.name==Country,], 
             aes(xintercept=Year.change+1,color=as.factor(ICD)),
             show.legend = F)+
  theme(legend.title=element_blank())+
  coord_fixed(1.5)

f1


