# server.R

library(shiny)
library(data.table)
library(ggplot2)
fastrak      <- read.csv('fastrak.csv')
fastrak$time <- gsub(' .+', '', fastrak$time)
fastrak      <- as.data.table(fastrak)

shinyServer(function(input, output) {
  
  output$mainPlot <- renderPlot({
    
    start_time <- as.character(input$dateRange[1])
    end_time   <- as.character(input$dateRange[2])
    station_id <- input$station
    
    DT <- fastrak[station == station_id & time >= start_time & time <= end_time, 
                  list(count = sum(count)), by = time]
    
    qplot(as.Date(time),
          count,
          data = DT,
          geom = 'line',
          xlab = 'time') + aes(group = 1) + scale_x_date()
  })
})