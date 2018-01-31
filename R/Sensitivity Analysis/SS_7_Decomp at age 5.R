###################### Life expectancy decomposition and e dagger
### Author JM (Horiuchi etal 2008)
###############################################################################
library(reshape2)
library(data.table)
library(foreach)
library(doParallel)
registerDoParallel(cores=4)

setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe-2017")

load("Data/HMD_Data.RData")
source("R/Functions.R")
Eastern_HMDL             <- HMDL[HMDL$PopName %in% Country.HMD.vec & HMDL$Year >= 1960,]
Eastern_HMDL$Country     <- Country.name.vec[Eastern_HMDL$PopName]
Eastern_HMDL$Sex         <- as.factor(Eastern_HMDL$Sex)
levels(Eastern_HMDL$Sex) <- Sexes
Eastern_HMDL             <- Eastern_HMDL[with(Eastern_HMDL,order(Country,Sex,Year,Age)),]
gdata::keep(Eastern_HMDL,sure=T)
source("R/Functions.R")


#k <- Country.name.vec[1]
#j <- Sexes[2]
#mx <- Eastern_HMDL[Eastern_HMDL$Year == 1960 & Eastern_HMDL$Sex=='Male' & Eastern_HMDL$PopName=='BLR',]$mx
Decomp.results <- NULL
for (k in sort(Country.name.vec)){
  for(j in Sexes){
    D1         <- Eastern_HMDL[Eastern_HMDL$Country==k & Eastern_HMDL$Sex == j,]
    years      <- unique(sort(D1$Year))[-1]
    mat        <- acast(D1, Age~Year, value.var="mx")  
    
    dime       <- dim(mat)[2]
    #e0.decomp  <- foreach(i=1:(dime-1)) %dopar% {Decomp(func=life.expectancy.frommx.5, rates1=mat[,i] , rates2=mat[,i+1], N=100,sex=j)}
    ed.decomp  <- foreach(i=1:(dime-1)) %dopar% {Decomp(func=edag.function.frommx.5, rates1=mat[,i] , rates2=mat[,i+1], N=50,sex=j)}
    
    #D.e0        <- do.call(cbind, lapply(e0.decomp, data.frame, stringsAsFactors=FALSE))
    #names(D.e0) <- years
    #rownames(D.e0) <- 0:110
    D.ed        <- do.call(cbind, lapply(ed.decomp, data.frame, stringsAsFactors=FALSE))
    names(D.ed) <- years
    rownames(D.ed) <- 0:110
    
    #DT.e0      <- melt(as.matrix(D.e0), varnames = c('age','year'),value.name = 'Contribution')
    #DT.e0$Ind  <- 'e0'
    DT.ed      <- melt(as.matrix(D.ed), varnames = c('age','year'),value.name = 'Contribution')
    DT.ed$Ind  <- 'ed'
    #DT.decomp  <- rbind(DT.e0,DT.ed)
    DT.decomp  <- DT.ed
    DT.decomp$country <- unique(D1$Country)
    DT.decomp$PopName <- unique(D1$PopName)
    DT.decomp$sex <- unique(D1$Sex)
    
    Decomp.results     <- rbind(Decomp.results,DT.decomp)
    print(k)
  }
}



save(Decomp.results, file = "Data/Age_Decomp_results_age5.Rdata")


#### Do some graphs
load('Data/Age_Decomp_results_age5.Rdata')
source("R/Functions.R")
library(ggplot2)
library(ggthemes)

Data            <- data.table(Decomp.results)
Data$Age5       <- (cut(Data$age+1, breaks=c(seq(0,110,5),Inf),labels=Labels.age))
Data            <- Data[,list(Contribution = sum(Contribution)), by = list(country,sex,year,Ind,Age5)]
Data$Period     <- (cut(Data$year+1, breaks=c(1960,1981,1989,1995,2000,Inf),labels=Period.labels))
Data            <- Data[,list(Contribution = sum(Contribution)), by = list(country,sex,Period,Ind,Age5)]


### Add labels to periods
base2 <- toupper(c("#7265e7", "#e265e7", "#e7657d", "#e7bc65", "#a2e765", "#65e797", "#65c7e7"))
#plot(1:7,1:7,col=toupper(base2),pch=16,cex=5)
# determine order by eyeballing colors to causes (HT J. Schoeley)
myColours1 <- base2[c(7,5,3,1,4)]

# 
# Age.males.e0 <- ggplot(Data[Data$sex == 'Male' & Data$Ind == 'e0' & Data$Age5 != '0-4' &
#                               Data$Age5 != '105-109' & Data$Age5 != '110+',], 
#                        aes(x = Age5, y = Contribution, fill = Period, width=.7)) +
#   ggtitle( 'Age-contribution to changes in life expectancy by period' , subtitle = 'Males')+
#   scale_fill_manual('Period', values = myColours1) + 
#   geom_bar(aes(group = Period), stat = "identity",position = "stack")+
#   facet_wrap(~country)+
#   labs(x = "Age group", y = "Contribution (years)",size=12)+
#   theme_fivethirtyeight(base_size = 18)+
#   theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
#                                     linetype="solid"),
#         axis.title = element_text(),
#         rect = element_rect(fill = 'white', 
#                             linetype = 0, colour = NA))+
#   theme(axis.title.y = element_text(size = 12, angle = 90))+
#   theme(axis.title.x = element_text(size = 12, angle = 00))+
#   theme(text = element_text(size=14),
#         strip.text.x = element_text(size = 16, colour = "black"),
#         axis.text.y = element_text(size = 8))+
#   geom_hline(yintercept = 0)+
#   coord_flip()
# Age.males.e0
# 
# Age.females.e0 <- ggplot(Data[Data$sex == 'Female' & Data$Ind == 'e0' & Data$Age5 != '0-4' &
#                                 Data$Age5 != '105-109' & Data$Age5 != '110+',], 
#                          aes(x = Age5, y = Contribution, fill = Period, width=.7)) +
#   ggtitle( 'Age-contribution to changes in life expectancy by period' , subtitle = 'Females')+
#   scale_fill_manual('Period', values = myColours1) + 
#   geom_bar(aes(group = Period), stat = "identity",position = "stack")+
#   facet_wrap(~country)+
#   labs(x = "Age group", y = "Contribution (years)",size=12)+
#   theme_fivethirtyeight(base_size = 18)+
#   theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
#                                     linetype="solid"),
#         axis.title = element_text(),
#         rect = element_rect(fill = 'white', 
#                             linetype = 0, colour = NA))+
#   theme(axis.title.y = element_text(size = 12, angle = 90))+
#   theme(axis.title.x = element_text(size = 12, angle = 00))+
#   theme(text = element_text(size=14),
#         strip.text.x = element_text(size = 16, colour = "black"),
#         axis.text.y = element_text(size = 8))+
#   geom_hline(yintercept = 0)+
#   coord_flip()
# Age.females.e0


Age.males.ed5 <- ggplot(Data[Data$sex == 'Male' & Data$Ind == 'ed' & Data$Age5 != '0-4' &
                              Data$Age5 != '105-109' & Data$Age5 != '110+',], 
                       aes(x = Age5, y = Contribution, fill = Period, width=.7)) +
  ggtitle( 'Age-contribution to changes in life disparity at age 5 by period' , subtitle = expression(paste('Males, negative values decrease ',e^"\u2020", 'and positive values increase ',e^"\u2020")))+
  scale_fill_manual('Period', values = myColours1) + 
  geom_bar(aes(group = Period), stat = "identity",position = "stack")+
  facet_wrap(~country)+
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
Age.males.ed5


Age.females.ed <- ggplot(Data[Data$sex == 'Female' & Data$Ind == 'ed' & Data$Age5 != '0-4' &
                                Data$Age5 != '105-109' & Data$Age5 != '110+',], 
                         aes(x = Age5, y = Contribution, fill = Period, width=.7)) +
  ggtitle( 'Age-contribution to changes in life disparity at age 5 by period' , subtitle = expression(paste('Females, negative values decrease ',e^"\u2020", 'and positive values increase ',e^"\u2020")))+
  scale_fill_manual('Period', values = myColours1) + 
  geom_bar(aes(group = Period), stat = "identity",position = "stack")+
  facet_wrap(~country)+
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
Age.females.ed

require(gridExtra)
pdf(file="Outcomes/For the review/Decomp_ed_males.pdf",width=15,height=10,pointsize=2,useDingbats = F)
grid.arrange(Age.males.ed5,ncol=1)
dev.off()


