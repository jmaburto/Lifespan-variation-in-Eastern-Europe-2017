library(shiny)
library(ggplot2)
library(data.table)
library(RColorBrewer)

rsconnect::setAccountInfo(name='demographs',
                          token='85D46E99E52F997A5B7724A2CB92970D',
                          secret='nSFJ33exT9xU84CNmqwmlIRlXOAfma3HuwEg5owg')

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe-2017/R/Sensitivity Analysis/CEE_App/")

load('Age_Decomp_results.Rdata')
unique(Decomp.results$year)
runApp()
