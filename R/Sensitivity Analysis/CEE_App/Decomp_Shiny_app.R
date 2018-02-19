library(shiny)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(dplyr)

rsconnect::setAccountInfo(name='demographs',
                          token='85D46E99E52F997A5B7724A2CB92970D',
                          secret='nSFJ33exT9xU84CNmqwmlIRlXOAfma3HuwEg5owg')

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe-2017/R/Sensitivity Analysis/CEE_App/")

load('Age_Decomp_results.Rdata')
load('COD_Rupture.RData')
load('Association.RData')
load('mx_CEE.RData')


unique(Decomp.results$year)
runApp()

b0 <- 1960
b1 <- 1981
b2 <- 1989
b3 <- 1995
b4 <- 2000
b5 <- Inf

Period.labels <- c(paste0(b0,'-',b1),paste0(b1,'-',b2),paste0(b2,'-',b3),paste0(b3,'-',b4),paste0(b4,'- 2014'))
Data.dif$Period     <- (cut(Data.dif$year+1, breaks=c(b0,b1,b2,b3,b4,b5),labels=Period.labels))
### Plots of first differences versus first differences

T1 <- t(table(Data.dif$Category2,Data.dif$Period))/colSums(table(Data.dif$Category2,Data.dif$Period))
T2 <- T1[,1]+T1[,3]
T3 <- T1[,2]+T1[,4]


### get proportions

# Data <- Data.dif[Data.dif$PopName == 'BLR' & Data.dif$Sex == 'Male' & Data.dif$Period == '1960-1987',] 

Proportions <- Data.dif[, get.prop.fun(Data = .SD), by = list(Sex,Period)]
Proportions <- Proportions[order(Period),]
Proportions[Proportions$Sex=='']

Proportions[Proportions$Sex =='Male']