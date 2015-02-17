# ui.R

library(shiny)
library(data.table)
library(ggplot2)
fastrak  <- read.csv('fastrak.csv')
fastrak  <- as.data.table(fastrak)
stations <- unique(fastrak$station)

shinyUI(
  fluidPage(
    
    fluidRow(
      column(3, selectInput('station', label = h3('Station'), choices = stations, selected = 1)),
      
      column(3, dateRangeInput('dateRange', label = h3('Time Range'), 
                               start = '2007-03-23', 
                               end   = '2013-08-31',
                               min   = '2007-03-23',
                               max   = '2013-08-31')
      )
    ),
    
    mainPanel(
      plotOutput("mainPlot")  
    )
  )
)
