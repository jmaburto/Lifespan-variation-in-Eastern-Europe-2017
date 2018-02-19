###############################################################################
###############################################################################

# Classification of causes of death (see paper)

library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe-2017/")
# Create a vector with the countries we are interested in
Country.name.vec <- c('Bulgaria','Hungary','Slovakia','Slovenia')
datadir          <- "C:/Users/jmaburto/Documents/WHO Data (big files)/Aburto,Wensink/WHO Data"

# Create a vector with the countries' codes according to WHO
Country.code.vec <- c(4030,4150,4274,4276)

# Information for ICD9
ICD9    <- data.table(read.table(  file = paste0(datadir,'/MortIcd9.txt'),header = T,sep = ',',stringsAsFactors = F))
ICD9    <- ICD9[ICD9$Country %in% Country.code.vec,]
ICD9$ICD<- 9
#unique(ICD9$Cause)
# Information for ICD10
ICD10_1 <- data.table(read.table(  file = paste0(datadir,'/MortIcd10_part1.txt'),header = T,sep = ',',stringsAsFactors = F))
ICD10_2 <- data.table(read.table(  file = paste0(datadir,'/MortIcd10_part2.txt'),header = T,sep = ',',stringsAsFactors = F))
ICD10   <- rbind(ICD10_1,ICD10_2)
ICD10   <- ICD10[ICD10$Country %in% Country.code.vec,]
ICD10$ICD<- 10
# No we have information on cause of death for Sweden, Norway and Denmark. Just anticipating in case we do any,
# comparison. But now just focusing on Denmark
WHO_Data <- rbind(ICD9,ICD10)
WHO_Data$Country.name <- as.factor(WHO_Data$Country)
levels(WHO_Data$Country.name) <- Country.name.vec

WHO_Data <- WHO_Data[WHO_Data$Year >= 1994,]

#COD_Data <- COD_Data[COD_Data$Country == 4050,]
# sex 1 is male, 2 is female, 9 is unspecified
WHO_Data <- WHO_Data[WHO_Data$Sex != 9,]
WHO_Data$Sex <- as.factor(WHO_Data$Sex)
levels(WHO_Data$Sex) <- c('m', 'f')

WHO_Data <- WHO_Data[,c(1,41,40,4:39)]
save(WHO_Data, file = 'Data/CEE_WHO_Data.RData')

gdata::keep(WHO_Data, sure = T)

