###################### Life expectancy decomposition and e dagger by causes of death
### Author JM (Horiuchi etal 2008)
###############################################################################

library(data.table)
library(reshape2)
library(DemoDecomp)
library(ggplot2)
#setwd(  "C:/Users/jmaburto/Documents/GitHub/Lifespan-variation-in-Eastern-Europe")


################ Ready to calculate age-specific mortality rates
### Polonia information is until 2009
load("Data/COD_LT_5Data.RData")
XYZ <- c("BLR","CZE","EST","LTU","LVA","POL","RUS","UKR")
#k <- XYZ[6]
#i <- 1
#j <- 1994

Mx.Cause <- NULL
for (k in XYZ){
  for (i in 1:2){
    for (j in 1994:2010){
      Sx <- ifelse(i==1,'m','f')
      
      D  <- DT.COD.data[DT.COD.data$country == k & DT.COD.data$sex == i & DT.COD.data$year == j,]
      D  <- as.matrix(acast(D,age ~ Class,value.var = 'prop'))
      v1 <- matrix(c(rep(0,54),rep(1,6)),nrow = 6,ncol = 10)
      D <- rbind(D,v1)
      rownames(D) <- unique(HMDL_5$Age)
      
      E  <- HMDL_5[HMDL_5$PopName == k & HMDL_5$Sex == Sx & HMDL_5$Year == j,]
      
      cMx <- D*E$mx
      cause.mx <- melt(cMx)
      names(cause.mx) <- c('age','cause','mxc')
      cause.mx$year <- j
      cause.mx$sex <- i
      cause.mx$country <- k
      
      Mx.Cause <- rbind(Mx.Cause,cause.mx)
    }
  }
}
Mx.Cause <- data.table(Mx.Cause)
gdata::keep(Mx.Cause, sure = T)
source("R/Functions.R")

## Choose just 3 points in time

Decomp.data <- Mx.Cause[ year %in% c(1994,2000,2010)]
Decomp.data <- Decomp.data[order(country,sex,year,cause,age)]

#DT <- Decomp.data[country == 'BLR' & year == 1994]

sex.decomp.fun <- function(DT = .SD, sex = 'f'){
  males.mx   <- DT[sex == 2]$mxc
  females.mx <- DT[sex == 1]$mxc
  causes.n   <- DT[sex == 1]$cause
  age.n      <- DT[sex == 1]$age
  
  decomp <- horiuchi(func = e0frommxc5,pars1 = males.mx, pars2 = females.mx,N = 50)
  
  results <- data.table(cbind(causes.n,age.n,decomp))
  results
}

sex.decomp <- Decomp.data[, sex.decomp.fun(.SD), by = list(country,year) ]

sex.decomp$cause  <- as.factor(sex.decomp$causes.n)
levels(sex.decomp$cause) <- labels.cause
sex.decomp$age  <- as.factor(sex.decomp$age.n)
levels(sex.decomp$age) <- Labels.age2


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

sex.decomp$cause <- factor(sex.decomp$cause, levels=rev(levels(sex.decomp$cause)))
levels(sex.decomp$cause)

sex.dif.plot <- ggplot(sex.decomp, 
                          aes(x = age, y = decomp, fill = cause, width=.7)) +
  ggtitle( 'Age-contribution to sex-difference in life expectancy by cause of death')+
  scale_fill_manual('Cause', values = rev(myColours1)) + 
  guides(fill = guide_legend(reverse = TRUE))+
  geom_bar(aes(group = cause), stat = "identity",position = "stack")+
  facet_grid(year~country)+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  #theme_fivethirtyeight(base_size = 18)+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                    linetype="solid"),
        axis.title = element_text(),
        rect = element_rect(fill = 'white', 
                            linetype = 0, colour = NA))+
  theme(axis.title.y = element_text(size = 10, angle = 90))+
  theme(axis.title.x = element_text(size = 10, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 8))+
  geom_hline(yintercept = 0)+
  coord_flip()
sex.dif.plot

pdf(file="R/Sex difference check/plot.pdf",width=15,height=10,pointsize=6,useDingbats = F)
print(sex.dif.plot)
dev.off()
























  