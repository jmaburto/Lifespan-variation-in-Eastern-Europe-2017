
#user: jose...@hotmail.com
#ps: LifespanVariation
#user: DemoGraphs

library(shiny)
library(data.table)
library(DT)
library(plotly)

  Sexx <- c('Male','Female')
  break1.x       <- 1961:2010
  
  
  shinyUI(
    fluidPage(
      titlePanel('Lifespan dispersion in CEE'),
      navbarPage(
        '[Authors redacted for anonymity, app created for the paper Lifespan dispersion in times of life expectancy fluctuation: the case of Central and Eastern Europe]',
        position = c("fixed-bottom")),
      sidebarLayout(
        sidebarPanel(
          selectInput( 'Sex','Sex',Sexx, selected = 'Male'),
          br(),
          numericInput('break1', 'Break 1', 1980,
                       min = 1961, max = 2010),
          br(),
          numericInput('break2', 'Break 2', 1988,
                       min = 1962, max = 2011),
          br(),
          numericInput('break3', 'Break 3', 1994,
                       min = 1963, max = 2012),
          br(),
          numericInput('break4', 'Break 4', 2000,
                       min = 1964, max = 2013),
          p("The default years are the ones shown in the article. Here, the user can change the break-years and see how sensible the results are. For example, 
            the breaks determined by the divisive hierarchical estimation algorithm on the coefficient of variation betwen countries were: 1976,1986,1993 and 2001,"),
          br(),
          width = 2
        ),

        
        mainPanel(
        tabsetPanel(
         tabPanel("Decomposition results by period",
                  plotlyOutput('ed.decomp')
                 )
  
        ))
        )
      )
    )
  
  
#  devtools::install_github('hadley/ggplot2')
  