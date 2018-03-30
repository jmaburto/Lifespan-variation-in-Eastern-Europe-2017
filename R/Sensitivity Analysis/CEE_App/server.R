library(ggplot2)
library(RColorBrewer)
library(data.table)
library(ggthemes)
library(gridExtra)

load('Age_Decomp_results.Rdata')
load('COD_Rupture.RData')
load('Association.RData')
load('mx_CEE.RData')

shinyServer(
  function(input, output) {
    makeTransparent<-function(someColor, alpha=100){
      newColor<-col2rgb(someColor)
      apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                  blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
    }
    
    AKm02a0 <- function(m0, sex = "m"){
      sex <- rep(sex, length(m0))
      ifelse(sex == "m", 
             ifelse(m0 < .0230, {0.14929 - 1.99545 * m0},
                    ifelse(m0 < 0.08307, {0.02832 + 3.26201 * m0},.29915)),
             # f
             ifelse(m0 < 0.01724, {0.14903 - 2.05527 * m0},
                    ifelse(m0 < 0.06891, {0.04667 + 3.88089 * m0}, 0.31411))
      )
    }
    
    life.expectancy.fromSD <- compiler::cmpfun(function(DT){
      mx <- DT$mx
      sex <- DT$Sex2[1]
      i.openage <- length(mx)
      OPENAGE   <- i.openage - 1
      RADIX     <- 1
      ax        <- mx * 0 + .5
      ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
      qx        <- mx / (1 + (1 - ax) * mx)
      qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
      ax[i.openage]       <- 1 / mx[i.openage]                   
      px 				    <- 1 - qx
      px[is.nan(px)]      <- 0
      lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
      dx 				    <- lx * qx
      Lx 				    <- lx - (1 - ax) * dx
      Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
      Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
      ex 				    <- Tx / lx
      ex[1]
    })
    
    edag.function.fromSD <- compiler::cmpfun(function(DT){
      mx <- DT$mx
      sex <- DT$Sex2[1]
      i.openage <- length(mx)
      OPENAGE   <- i.openage - 1
      RADIX     <- 1
      ax        <- mx * 0 + .5
      ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
      qx        <- mx / (1 + (1 - ax) * mx)
      qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
      ax[i.openage]       <- 1 / mx[i.openage]                   
      px 				    <- 1 - qx
      px[is.nan(px)]      <- 0
      lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
      dx 				    <- lx * qx
      Lx 				    <- lx - (1 - ax) * dx
      Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
      Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
      ex 				    <- Tx / lx
      l <- length(ex)
      ed <- (sum(dx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
      ed
    })
    
    
    output$mytable = renderDataTable({
      
      Data <- Data.dif
      
      b0 <- 1960
      b1 <- as.integer(input$breakk1)
      b2 <- as.integer(input$breakk2)
      b3 <- as.integer(input$breakk3)
      b4 <- as.integer(input$breakk4)
      b5 <- Inf
      
      # year1 <- 1960
      # year2 <- 1980
      # year3 <- 1988
      # year4 <- 1994
      # year5 <- 2000
      # year6 <- 2014
      # Sx  <- 'Male'
      
      Sx <- as.character(input$Sexx)
      
      Period.labels <- c(paste0(b0,'-',b1),paste0(b1,'-',b2),paste0(b2,'-',b3),paste0(b3,'-',b4),paste0(b4,'-2014'))
      Data$Period     <- (cut(Data$year+1, breaks=c(b0,b1,b2,b3,b4,b5),labels=Period.labels))
      
      Data <- Data[Data$Sex ==  Sx]
      
      T1 <- t(table(Data$Category2,Data$Period))/colSums(table(Data$Category2,Data$Period))
      T2 <- T1[,1]+T1[,3]
      T3 <- T1[,2]+T1[,4]
      
      T4 <- round(rbind(T2,T3)*100,2)
      row.names(T4) <- c('Unexpected direction (%)', 'Expected direction (%)')
      
      
      datatable(t(T4), options = list(paging=FALSE,ordering=T,searching=F),rownames = T)
    })
    
    output$association <- renderPlot({
     
      
       Data <- Data.dif
      
      b0 <- 1960
      b1 <- as.integer(input$breakk1)
      b2 <- as.integer(input$breakk2)
      b3 <- as.integer(input$breakk3)
      b4 <- as.integer(input$breakk4)
      b5 <- Inf
      
      # year1 <- 1960
      # year2 <- 1980
      # year3 <- 1988
      # year4 <- 1994
      # year5 <- 2000
      # year6 <- 2014
      # Sx  <- 'Male'
      
      Sx <- as.character(input$Sexx)
      
      Period.labels <- c(paste0(b0,'-',b1),paste0(b1,'-',b2),paste0(b2,'-',b3),paste0(b3,'-',b4),paste0(b4,'-2014'))
      Data$Period     <- (cut(Data$year+1, breaks=c(b0,b1,b2,b3,b4,b5),labels=Period.labels))
      ### Plots of first differences versus first differences
      
      
      abs.dif.male <- ggplot(Data[Data$Sex == Sx,], aes(dif.ed, dif.ex,colour=Category,group=Period)) +
        ggtitle(paste('Association between changes in life expectancy and life disparity,', Sx) , subtitle = 'Absolute changes (years)')+
        geom_vline(aes(xintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
        geom_hline(aes(yintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
        geom_point(show.legend = F,size =2.5)+
        scale_colour_manual('Country', values = c(makeTransparent("#e7657d",150),makeTransparent('gray71',100), "deepskyblue4"))+
        facet_wrap(~Period,nrow = 1)+
        theme_fivethirtyeight(base_size = 18)+
        theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                          linetype="solid"),
              axis.title = element_text(),
              rect = element_rect(fill = 'white', 
                                  linetype = 0, colour = NA))+
        scale_y_continuous( 'Changes in life expectancy',limits = c(-3.2,3.2)) +
        scale_x_continuous( 'Changes in life disparity',limits = c(-3.2,3.2))
      
      abs.dif.male
      
      
      rel.dif.male <- ggplot(Data[Data$Sex == Sx,], aes(dif.ed.rel, dif.ex.rel,colour=Category,group=Period)) +
        ggtitle( ' ' , subtitle = 'Relative changes (%)')+
        geom_vline(aes(xintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
        geom_hline(aes(yintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
        geom_point(show.legend = F,size =2.5)+
        scale_colour_manual('Country', values = c(makeTransparent("#e7657d",150),makeTransparent('gray71',100), "deepskyblue4"))+
        facet_wrap(~Period,nrow = 1)+
        #theme_minimal(base_size = 18)+
        theme_fivethirtyeight(base_size = 18)+
        theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                          linetype="solid"),
              axis.title = element_text(),
              rect = element_rect(fill = 'white', 
                                  linetype = 0, colour = NA))+
        scale_y_continuous( 'Changes in life expectancy (%)',limits = c(-8.25,8.25)) +
        scale_x_continuous( 'Changes in life disparity (%)',limits = c(-8.25,8.25))
      
      rel.dif.male
      
      
      #### now print them
      grid.arrange(abs.dif.male,rel.dif.male,nrow = 2)
      
    },width = 1600, height = 800)
    
    output$ed.decomp <- renderPlot({
      
      year1 <- 1960
      year2 <- as.integer(input$break1)
      year3 <- as.integer(input$break2)
      year4 <- as.integer(input$break3)
      year5 <- as.integer(input$break4)
      year6 <- 2014

      # year1 <- 1960
      # year2 <- 1980
      # year3 <- 1988
      # year4 <- 1994
      # year5 <- 2000
      # year6 <- 2014
      # Sx  <- 'Male'

      Sx <- as.character(input$Sex)

      Labels.age      <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                           '40-44','45-49','50-54','55-59','60-64','65-69',
                           "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105-109","110+")

      Period.labels <- c(paste0(as.character(year1),'-',as.character(year2)),
                         paste0(as.character(year2),'-',as.character(year3)),
                         paste0(as.character(year3),'-',as.character(year4)),
                         paste0(as.character(year4),'-',as.character(year5)),
                         paste0(as.character(year5),'-',as.character(year6)))

      base2 <- toupper(c("#7265e7", "#e265e7", "#e7657d", "#e7bc65", "#a2e765", "#65e797", "#65c7e7"))
      myColours1 <- base2[c(7,5,3,1,4)]

      Data            <- data.table(Decomp.results)
      Data$Age5       <- (cut(Data$age+1, breaks=c(seq(0,110,5),Inf),labels=Labels.age))
      Data            <- Data[,list(Contribution = sum(Contribution)), by = list(country,sex,year,Ind,Age5)]
      Data$Period     <- cut(Data$year+1, breaks=c(year1,year2+1,year3+1,year4+1,year5+1,Inf),labels=Period.labels)
      Data            <- Data[,list(Contribution = sum(Contribution)), by = list(country,sex,Period,Ind,Age5)]
      fig.data        <-  Data[Data$sex == Sx & Data$Ind == 'ed' & Data$Age5 != '0-4' &Data$Age5 != '105-109' & Data$Age5 != '110+',]
      
    
      
       q <- ggplot(data = fig.data,aes(x = Age5, y = Contribution, fill = Period, width=.7)) +
         ggtitle('Age-contribution to changes in life disparity by period')+
          #ggtitle( 'Age-contribution to changes in life disparity by period' , 
           #        subtitle = expression(paste('Males, negative values decrease ',e^"\u2020", 'and positive values increase ',e^"\u2020")))+ 
         scale_fill_manual('Period', values = myColours1) +
         geom_bar(aes(group = Period), stat = "identity",position = "stack")+
         facet_wrap(~country)+theme_light()+
         labs(x = "Age group", y = "Contribution (years)",size=12)+
         theme(text = element_text(size=10),
               axis.text.x = element_text(angle=45, hjust=1))+
         labs(x = " ", y = " ",size=10)+
         theme(text = element_text(size=10),
               strip.text.x = element_text(size = 12, colour = "black"))+
         geom_hline(yintercept = 0)

       q
      
    },width = 1600, height = 800)
    
    output$rupture <- renderPlot({
        #Country <- 'Slovenia'
        
        ifelse(input$Country == 'Slovakia', y<-2009, y<-2010)
        
        Data <- DT_COD.melt[DT_COD.melt$Year <= y,]

        levels(Data$Sex) <- c('Males', 'Females')
        
        COD.labels <- c('Attributable to alcohol',
                        'IHD',
                        'Stroke',
                        'Transportation Accidents',
                        'Other external causes',
                        'Infectious & respiratory diseases',
                        'Cancers',
                        'Other circulatory',
                        'Rest')
        
        
        change.ICD <- Data[, list(Year.change=max(Year)), by = list(Country.name,ICD)]
        
        Total.cause <- Data[,list(Total=sum(Dx)), by = list(Country.name,Year,Sex,Cat,ICD)]
        Total.cause$ICD <- as.factor(Total.cause$ICD)
        levels(Total.cause$ICD) <- c('ICD 9','ICD 10')
        Total.cause$Cat <- as.factor(Total.cause$Cat)
        levels(Total.cause$Cat) <- COD.labels
        
        
        ##### Bulgaria
        
        Country <- input$Country
        
    
        f1 <- ggplot(Total.cause[Total.cause$Country.name==Country], aes(Year,Total,colour=as.factor(ICD), group=interaction(Sex,ICD),linetype=Sex))+
          ggtitle(Country,subtitle = paste0('Years of change in ICD: ', change.ICD[change.ICD$ICD==9 & change.ICD$Country.name==Country,]$Year.change+1))+
          geom_line(lwd=1,show.legend =T)+theme_light()+
          theme(text = element_text(size=20))+
          facet_wrap(~Cat,scales = "free",ncol = 3)+
          geom_vline(data=change.ICD[change.ICD$ICD==9 & change.ICD$Country.name==Country,], 
                     aes(xintercept=Year.change+.5),
                     show.legend = F)+
          coord_cartesian(xlim=c(1993, 2011))+
          theme(legend.title=element_blank())
        
        f1
        
      },width = 1300,height = 800)
    
    output$association2 <- renderPlot({
      
      r1 <- input$r1
      r3 <- input$r2
      r2 <- seq(r1,r3,length.out = 10)
      
      Data <- Data_CEE_mx[Data_CEE_mx$Sex == 'Male',]
      
      Data[Data$Age == 0 & Data$Year < 1990]$mx <- Data[Data$Age == 0 & Data$Year < 1990]$mx*r1
      for (i in 1:10) {
        Data[Data$Age == 0 & Data$Year == (i+1989)]$mx <- Data[Data$Age == 0 & Data$Year == (i+1989)]$mx*r2[i]
      }
      Data[Data$Age == 0 & Data$Year >= 2000]$mx <- Data[Data$Age == 0 & Data$Year >= 2000]$mx*r3
      

      DT1 <- Data[,list(ex = life.expectancy.fromSD(.SD), ed = edag.function.fromSD(.SD)), by = list(Year,Sex,Country)]
      DT2 <- DT1[,list(year=Year[-1L],dif.ex = diff(ex),dif.ed = diff(ed)), by = list(Sex,Country)] 
      
      DT2$Category2 <-  4
      DT2[DT2$dif.ed >= 0 & DT2$dif.ex >= 0, ]$Category2  <-3
      DT2[DT2$dif.ed < 0 & DT2$dif.ex < 0, ]$Category2 <- 1
      DT2[DT2$dif.ed >= 0 & DT2$dif.ex < 0, ]$Category2 <- 2
      
      
      DT2$Category <- 2  
      DT2[DT2$dif.ed >= 0 & DT2$dif.ex >= 0, ]$Category <- 1
      DT2[DT2$dif.ed < 0 & DT2$dif.ex < 0, ]$Category <-   1
      DT2[DT2$Country=='Russia', ]$Category <- 3
      DT2$Category   <- factor(DT2$Category,levels=c(1:3),labels=c('Negative','Positive','Russia'))
      
      
      DT2$Period     <- (cut(DT2$year+1, breaks=c(1960,1981,1989,1995,2000,Inf),labels= c("Stagnation 1960-1980","Improvements 1980-1988","Deterioration 1988-1994",
                                                                                          "Divergence 1994-2000","Convergence 2000-present")))
      ### Plots of first differences versus first differences
      abs.dif.male <- ggplot(DT2[DT2$Sex == 'Male',], aes(dif.ed, dif.ex,colour=Category,group=Period)) +
        ggtitle('Association between changes in life expectancy and lifespan variation, males.', subtitle = 'Absolute changes (years)')+
        geom_vline(aes(xintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
        geom_hline(aes(yintercept=0),show.legend = F,linetype=2,size=.6, colour="gray48")+
        geom_point(show.legend = F,size =2.5)+
        scale_colour_manual('Country', values = c(makeTransparent("#e7657d",150),makeTransparent('gray71',100), "deepskyblue4"))+
        facet_wrap(~Period,nrow = 1)+
        #theme_minimal(base_size = 18)+
        theme_fivethirtyeight(base_size = 18)+
        theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                          linetype="solid"),
              axis.title = element_text(),
              rect = element_rect(fill = 'white', 
                                  linetype = 0, colour = NA))+
        scale_y_continuous( 'Changes in life expectancy ') +
        scale_x_continuous( 'Changes in lifespan variation')
      
      abs.dif.male
      
      
    },width = 1600, height = 500)
    
    
    output$mytable2 = renderDataTable({
      
      r1 <- input$r1
      r3 <- input$r2
      r2 <- seq(r1,r3,length.out = 10)
      
      Data <- Data_CEE_mx[Data_CEE_mx$Sex == 'Male',]
      
      Data[Data$Age == 0 & Data$Year < 1990]$mx <- Data[Data$Age == 0 & Data$Year < 1990]$mx*r1
      for (i in 1:10) {
        Data[Data$Age == 0 & Data$Year == (i+1989)]$mx <- Data[Data$Age == 0 & Data$Year == (i+1989)]$mx*r2[i]
      }
      Data[Data$Age == 0 & Data$Year >= 2000]$mx <- Data[Data$Age == 0 & Data$Year >= 2000]$mx*r3
      
      
      DT1 <- Data[,list(ex = life.expectancy.fromSD(.SD), ed = edag.function.fromSD(.SD)), by = list(Year,Sex,Country)]
      DT2 <- DT1[,list(year=Year[-1L],dif.ex = diff(ex),dif.ed = diff(ed)), by = list(Sex,Country)] 
      
      DT2$Category2 <-  4
      DT2[DT2$dif.ed >= 0 & DT2$dif.ex >= 0, ]$Category2  <-3
      DT2[DT2$dif.ed < 0 & DT2$dif.ex < 0, ]$Category2 <- 1
      DT2[DT2$dif.ed >= 0 & DT2$dif.ex < 0, ]$Category2 <- 2
      
      
      DT2$Category <- 2  
      DT2[DT2$dif.ed >= 0 & DT2$dif.ex >= 0, ]$Category <- 1
      DT2[DT2$dif.ed < 0 & DT2$dif.ex < 0, ]$Category <-   1
      DT2[DT2$Country=='Russia', ]$Category <- 3
      DT2$Category   <- factor(DT2$Category,levels=c(1:3),labels=c('Negative','Positive','Russia'))
      
      
      DT2$Period     <- (cut(DT2$year+1, breaks=c(1960,1981,1989,1995,2000,Inf),labels= c("Stagnation 1960-1980","Improvements 1980-1988","Deterioration 1988-1994",
                                                                                          "Divergence 1994-2000","Convergence 2000-present")))
      males <- DT2[DT2$Sex=='Male']
      
      
      T1 <- t(table(males$Category2,males$Period))/colSums(table(males$Category2,males$Period))
      T2 <- T1[,1]+T1[,3]
      T3 <- T1[,2]+T1[,4]
      
      T4 <- round(rbind(T2,T3)*100,2)
      row.names(T4) <- c('Unexpected direction (%)', 'Expected direction (%)')
      
      
      datatable(t(T4), options = list(paging=FALSE,ordering=T,searching=F),rownames = T)
    })
    
    
    
})

# 
# 

# 
# Data.fig   <- Data[Data$Year >= initial.ind & Data$Year < final.ind & Data$Country == state.ind, ]
# Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Country,Sex,Cause,Age)] 
# #Data.fig        <- Data.fig[Data.fig$Age!= '0-4',]
# 
# Total.Age <- Data.fig[,sum(Contribution), by = list(Age,Sex)]
# Total.Age$V1 <- Total.Age$V1
# 
# base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],'lightgrey','lightpink')
# Data.fig$Contribution <- Data.fig$Contribution
# q <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
#   ggtitle('Decomposition of lifespan inequality', subtitle = paste0(state.ind,', ', initial.ind,'-',final.ind))+
#   facet_wrap(~Sex)+
#   scale_fill_manual('Cause of death', values = base2) + 
#   geom_bar(stat = "identity",position = "stack")+
#   theme_light()+
#   theme(text = element_text(size=10),
#         axis.text.x = element_text(angle=45, hjust=1))+
#   labs(x = " ", y = " ",size=10)+
#   theme(text = element_text(size=10),
#         strip.text.x = element_text(size = 12, colour = "black"))+
#   geom_hline(yintercept = 0)
