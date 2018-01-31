library(ggplot2)
library(RColorBrewer)
library(data.table)
library(ggthemes)

load('Age_Decomp_results.Rdata')

shinyServer(
  function(input, output) {
    
      output$ed.decomp <- renderPlotly({
      
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
      #    ggtitle( 'Age-contribution to changes in life disparity by period' , 
      #             subtitle = expression(paste('Males, negative values decrease ',e^"\u2020", 'and positive values increase ',e^"\u2020")))+ 
         scale_fill_manual('Period', values = myColours1) +
         geom_bar(aes(group = Period), stat = "identity",position = "stack")+
         facet_wrap(~country)+
         labs(x = "Age group", y = "Contribution (years)",size=12)+
         theme(text = element_text(size=10),
               axis.text.x = element_text(angle=45, hjust=1))+
         labs(x = " ", y = " ",size=10)+
         theme(text = element_text(size=10),
               strip.text.x = element_text(size = 12, colour = "black"))+
         geom_hline(yintercept = 0)

       print(ggplotly(q,width = 1600, height = 800))
      
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
