###############################################################################
library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe-2017/")

# Run this line to update the COD RData file
 #source(file = 'R/WHO Code/2_Get_WHO_Data.R')

# Load data from 2_Get_WHO_Data.R
Country.name.vec <- c('Bulgaria','Hungary','Slovakia','Slovenia')
get(load('Data/CEE_WHO_Data.RData'))

unique(WHO_Data$List)

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




# ICD 9 classification ----------------------------------------------------
ICD9.data    <- WHO_Data[WHO_Data$ICD==9,]
sort(unique(ICD9.data$Cause))

# Drop those code that do not have an A to avoid duplicates
#ICD9.data <-   ICD9.data[ICD9.data$Cause %in% good.codes9,]

########## Rest of bad codes
ICD9.data$Cat <- 10

########## category 1
ICD9.data[ICD9.data$Cause %in% c('B215','B347'),]$Cat <- 1

########## category 2
ICD9.data[ICD9.data$Cause %in% c('B27'),]$Cat <- 2

########## category 3
ICD9.data[ICD9.data$Cause %in% c('B290','B291','B292','B294'),]$Cat <- 3

########## category 4
ICD9.data[ICD9.data$Cause %in% c('B47'),]$Cat <- 4

########## category 5
ICD9.data[ICD9.data$Cause %in% c('B48','B54','B50','B51','B52','B55'),]$Cat <- 5

########## category 6
ICD9.data[ICD9.data$Cause %in% c('B01','B02','B03','B04','B05','B06','B07','B31',"B32" ),]$Cat <- 6

########## category 7
ICD9.data[ICD9.data$Cause %in% c(paste0('B0',8:9),paste0('B1',0:7)),]$Cat <- 7

########## category 8
ICD9.data[ICD9.data$Cause %in% c('B25','B26','B28','B293','B299','B30'),]$Cat <- 8

########## category 9
ICD9.data[ICD9.data$Cause %in% c(paste0('B',18:20),
                                 paste0('B21',0:4),
                                 paste0('B21',6:9),
                                 paste0('B',22:24),
                                 paste0('B',31:33),
                                 paste0('B',35:39),
                                 paste0('B',40:46),
                                 paste0('B',49),
                                 paste0('B',53),
                                 paste0('B',56),
                                 paste0('B34',0:6),
                                 paste0('B34',8:9)),]$Cat <- 9


ICD9.data <- ICD9.data[ICD9.data$Cat != 10,]
sort(unique(ICD9.data$Cause))



#bad codes 
#m <- ICD9.data[ICD9.data$Cat == 10,]
 #unique(m$Cause)

 #Hungary <- ICD9.data[ICD9.data$Country.name == 'Hungary',] 
 
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

# ICD 10 classification ----------------------------------------------------

ICD10.data      <- WHO_Data[WHO_Data$ICD==10,]
ICD10.data      <- ICD10.data[nchar(ICD10.data$Cause) <= 3,]
sort(unique(ICD10.data$Cause))
#get only the first 3 digits of the cause of death

# Exclude the cause A000, is all causes in documentation
ICD10.data <- ICD10.data[ICD10.data$Cause != 'AAA',]

# bad codes
ICD10.data$Cat <- 10


########## category 1
ICD10.data[ICD10.data$Cause %in% c('F10','K70','K74','X45'),]$Cat <- 1

########## category 2
ICD10.data[ICD10.data$Cause %in% c(paste0('I',20:25)),]$Cat <- 2

########## category 3
ICD10.data[ICD10.data$Cause %in% c(paste0('I',60:67),
                                   paste0('G',45)),]$Cat <- 3

########## category 4
ICD10.data[ICD10.data$Cause %in% c(paste0('V0',1:9),
                                   paste0('V',10:99)),]$Cat <- 4

########## category 5
ICD10.data[ICD10.data$Cause %in% c(paste0('X0',0:9),
                                   paste0('X',40:44),
                                   paste0('X',46:49),
                                   paste0('X',60:84),
                                   paste0('X',85:99),
                                   paste0('Y0',0:9),
                                   paste0('X',10:39),
                                   paste0('X',50:59),
                                   paste0('Y',35:36),
                                   paste0('Y',40:84),
                                   paste0('Y',85:91),
                                   paste0('Y',95:98),
                                   paste0('W0',0:9),
                                   paste0('W',10:19),
                                   paste0('W',65:74),
                                   paste0('W',75:84),
                                   paste0('W',20:64),
                                   paste0('W',85:99)
                                   ),]$Cat <- 5


########## category 6
ICD10.data[ICD10.data$Cause %in% c(paste0('A0',0:9),
                                   paste0('B0',0:9),
                                   paste0('A',10:99),
                                   paste0('B',10:99),
                                   paste0('J0',0:9),
                                   paste0('J',10:99)),]$Cat <- 6

########## category 7
ICD10.data[ICD10.data$Cause %in% c(paste0('C0',0:9),
                                   paste0('D0',0:9),
                                   paste0('C',10:99),
                                   paste0('D',10:48)),]$Cat <- 7



########## category 8
ICD10.data[ICD10.data$Cause %in% c(paste0('I0',0:9),
                                   paste0('I',10:15),
                                   paste0('I',26:28),
                                   paste0('I',34:38),
                                   paste0('I',40:46),
                                   paste0('I',30:33),
                                   paste0('I',50:51),
                                   paste0('I',47:49),
                                   paste0('I',69:78),
                                   paste0('I',79:99),
                                   paste0('F0',0:9),
                                   paste0('I0',0:9)),]$Cat <- 8

########## category 9
ICD10.data[ICD10.data$Cause %in% c(paste0('D',50:89),
                                   paste0('E0',0:9),
                                   paste0('E',10:88),
                                   paste0('F',0:9),
                                   paste0('F',11:99),
                                   paste0('G0',0:9),
                                   paste0('G',10:98),
                                   paste0('H0',0:9),
                                   paste0('H',10:99),
                                   paste0('I',15:19),
                                   paste0('I',29),
                                   paste0('I',39),
                                   paste0('I',52:60),
                                   paste0('I',68),
                                   paste0('K0',0:9),
                                   paste0('K',10:69),
                                   paste0('K',71:73),
                                   paste0('K',75:92),
                                   paste0('L0',0:9),
                                   paste0('L',10:98),
                                   paste0('M0',0:9),
                                   paste0('M',10:99),
                                   paste0('N0',0:9),
                                   paste0('N',10:98),
                                   paste0('O0',0:9),
                                   paste0('O',10:99),
                                   paste0('P0',0:9),
                                   paste0('P',10:96),
                                   paste0('Q0',0:9),
                                   paste0('Q',10:99),
                                   paste0('R0',0:9),
                                   paste0('R',10:99),
                                   paste0('Y',10:34),
                                   paste0('Y',37:39)),]$Cat <- 9

unique(ICD10.data[ICD10.data$Cat==10,]$Cause)





# Get all ICDs together ---------------------------------------------------
DT_COD <- rbind(ICD9.data,ICD10.data)

gdata::keep(DT_COD, sure = T)


##### Now play with ages
unique(DT_COD$Frmat)

# groups ages 1:4 for format 0
DT_COD.0       <- DT_COD[DT_COD$Frmat == 0,]
DT_COD.0$A_1_4 <- DT_COD.0$Deaths3 + DT_COD.0$Deaths4 + DT_COD.0$Deaths5 + DT_COD.0$Deaths6

# groups ages 1:4 for format 1
DT_COD.1       <- DT_COD[DT_COD$Frmat == 1,]
DT_COD.1$A_1_4 <- DT_COD.1$Deaths3 + DT_COD.1$Deaths4 + DT_COD.1$Deaths5 + DT_COD.1$Deaths6

# groups ages 1:4 for format 2
DT_COD.2       <- DT_COD[DT_COD$Frmat == 2,]
DT_COD.2$A_1_4 <- DT_COD.2$Deaths3

# rbind the 2 datasets
DT_COD <- rbind(DT_COD.0,DT_COD.1,DT_COD.2)

#reduce to variables needed (age < 85), until Deaths22 
DT_COD           <- DT_COD[,c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat', 'Deaths1', 'Deaths2', 'A_1_4', paste0('Deaths',7:22))]
colnames(DT_COD) <- c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat','Total',as.character(c(0,1,seq(5,80,5))))
DT_COD           <- DT_COD[with(DT_COD,order(Country,Sex,Year,Cat))]

DT_COD.melt2      <- melt(DT_COD, id.vars = c('Country','Country.name', 'ICD','Year','Sex','Cat'), variable.name = 'Age',value.name = 'Dx')

s <- dcast.data.table(DT_COD.melt2,Country+Country.name+ICD+Year+Sex+Cat~Age, value.var = 'Dx',fun.aggregate = sum,drop = F, fill = 0)

DT_COD.melt2 <- melt(s, id.vars = c('Country','Country.name', 'ICD','Year','Sex','Cat'), variable.name = 'Age',value.name = 'Dx')


DT_COD.melt      <- melt(DT_COD, id.vars = c('Country','Country.name', 'ICD','Year','Sex','Cat'), variable.name = 'Age',value.name = 'Dx')

### Get total deaths by age, sex, category, year.
DT_COD.melt      <- DT_COD.melt[, list(Dx=sum(Dx)), by =  list(Country,Country.name,ICD,Year,Sex,Age,Cat)]

DT_COD.melt2      <- DT_COD.melt2[, list(Dx=sum(Dx)), by =  list(Country,Country.name,ICD,Year,Sex,Age,Cat)]



### get proportions of causes of death by age
DT_COD.melt      <- DT_COD.melt[DT_COD.melt$Age != 'Total',]
DT_COD.melt2      <- DT_COD.melt2[DT_COD.melt2$Age != 'Total',]


DT_COD.melt      <- DT_COD.melt[, Dx.p := Dx/sum(Dx), by = list(Country,Country.name,ICD,Year,Sex,Age)]
DT_COD.melt      <- DT_COD.melt[with(DT_COD.melt,order(Country,Year,Sex,Cat,Age)),]

#DT_COD.melt <- DT_COD.melt2

#DT_COD.melt2[DT_COD.melt2$Country.name == 'Slovakia',]

save(DT_COD.melt,file= 'Data/CEE_WHO_COD_DATA.RData')
save(DT_COD.melt,file= 'R/Sensitivity Analysis/CEE_App/COD_Rupture.RData')

