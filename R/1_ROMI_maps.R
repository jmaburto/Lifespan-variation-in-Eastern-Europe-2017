########################### Code for heatmaps
library(data.table)
library(reshape2)
library(latticeExtra)
library(HMDHFDplus)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(MortalitySmooth)


# 1. Code to update data from HMD -----------------------------------------



# #get data from HMD
# XYZ <- getHMDcountries()
# us <- "jmaburto@colmex.mx"
# pw <- "kolmogorov"
# 
# setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe")
# # now grab all the lifetables and mesh together..
# # grab them all
# HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
#   cat(x,"\n")
#   Males        <- readHMDweb(x,"mltper_1x1",username=us,password=pw)
#   Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
#   Males$Sex    <- "m"
#   Females$Sex  <- "f"
#   CTRY         <- rbind(Females, Males)
#   CTRY$PopName <- x
#   CTRY
# }, us = us, pw = pw))
# 
# HMDL <- data.table(HMDL)
# save(HMDL,file="Data/HMD_Data.RData")
# 
# HMD_Counts <- do.call(rbind,lapply(XYZ, function(x, us, pw){
#   cat(x,"\n")
#   Deaths          <- readHMDweb(x,"Deaths_1x1",username=us,password=pw)
#   Exposures       <- readHMDweb(x,"Exposures_1x1",username=us,password=pw)
#   Deaths$Type     <- 'Deaths'
#   Exposures$Type  <- 'Exposures'
#   CTRY         <- rbind(Deaths, Exposures)
#   CTRY$PopName <- x
#   CTRY
# }, us = us, pw = pw))
# 
# HMD_Counts <- data.table(HMD_Counts)
# save(HMD_Counts,file="Data/HMD_Counts.RData")
# 



# 2. Code to do the ROMI plot ---------------------------------------------


setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe-2017")
load("Data/HMD_Counts.RData")
source('R/Functions.R')
### Subset countries in the study
HMD_Counts   <- HMD_Counts[HMD_Counts$PopName %in% Country.HMD.vec & HMD_Counts$Year >= 1960,]

### Reshape to get the ROMI plots
Eastern_Data        <- lapply(Sexes, FUN = reshape.data.1,
                       Data = HMD_Counts, Country.name.vec = Country.name.vec)
names(Eastern_Data) <- Sexes

gdata::keep(Eastern_Data,sure=T)
source("R/Functions.R")

### get a data.table to apply function faster
Eastern.DT <- do.call(rbind,Eastern_Data)
Eastern.DT <- Eastern.DT[,c('PopName','country','Year','sex','Age','Deaths','Exposures','mx')]
Eastern.DT <- Eastern.DT[with(Eastern.DT,order(PopName,country,Year,sex,Age)),]
Eastern.DT <- data.table(Eastern.DT)
#Data <- Eastern.DT[Eastern.DT$country == 'Bulgaria' & Eastern.DT$sex == 'Female',]

Ro.Data   <- Eastern.DT[, Ro.function(Data = .SD), by = list (PopName,country,sex)]

CEE <- sort(c('Bulgaria','Czhech Republic','Hungary','Poland','Slovakia','Slovenia'))
BC  <- sort(c('Estonia','Latvia','Lithuania'))
FSU <- sort(c('Belarus','Russia','Ukraine'))

Ro.Data$Region <- 1                                                                        
Ro.Data[Ro.Data$country %in% BC,]$Region <-2
Ro.Data[Ro.Data$country %in% FSU,]$Region <-3
Ro.Data$Region <- as.factor(Ro.Data$Region)
levels(Ro.Data$Region) <- c('CE','BC','FSU')

# A bounding to get better visualization
Ro.Data$Ro2 <- Ro.Data$Ro
Ro.Data[Ro.Data$Ro < -10,]$Ro2 <- -10
Ro.Data[Ro.Data$Ro > 10,]$Ro2  <-  10


romi.m <- ggplot(Ro.Data[Ro.Data$sex == 'Male' & Ro.Data$Age < 101,], aes(Year, Age, z = -Ro2 )) +
    ggtitle('Rates of mortality improvements', subtitle = 'Males')+
    facet_wrap(~Region + country,nrow =4,labeller = label_wrap_gen(multi_line=FALSE))+
    scale_x_continuous('Year', expand = c(0, 0)) +
    scale_y_continuous('Age', expand = c(0, 0)) +
    geom_tile(aes(fill = -Ro2))+
    theme_fivethirtyeight(base_size = 18)+ 
    #theme_hc(base_size = 18)+
    theme(text = element_text(size=18),legend.key.size = unit(.4, "in"),axis.title = element_text(),
          rect = element_rect(fill = 'white', 
                              linetype = 0, colour = NA),
          panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                      linetype="solid"))+
    scale_fill_gradientn(colours = colorspace::diverge_hcl(5),name= 'Change (%)')
    
  romi.m
  pdf(file="Outcomes/Romi_males.pdf",width=9,height=11,pointsize=6,useDingbats = F)
  print(romi.m)
  dev.off()
  
  
  romi.f <- ggplot(Ro.Data[Ro.Data$sex == 'Female' & Ro.Data$Age < 101,], aes(Year, Age, z = -Ro2 )) +
    ggtitle('Rates of mortality improvements', subtitle = 'Females')+
    scale_x_continuous('Year', expand = c(0, 0)) +
    scale_y_continuous('Age', expand = c(0, 0)) +
    facet_wrap(~Region + country,nrow =4,labeller = label_wrap_gen(multi_line=FALSE))+
    geom_tile(aes(fill = -Ro2))+
    theme_fivethirtyeight(base_size = 18)+ 
    #theme_hc(base_size = 18)+
    theme(text = element_text(size=18),legend.key.size = unit(.4, "in"),axis.title = element_text(),
          rect = element_rect(fill = 'white', 
                              linetype = 0, colour = NA),
          panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                      linetype="solid"))+
    scale_fill_gradientn(colours = colorspace::diverge_hcl(5),name= 'Change (%)')
  
  romi.f
  pdf(file="Outcomes/Romi_females.pdf",width=9,height=11,pointsize=6,useDingbats = F)
  print(romi.f)
  dev.off()
  
  



