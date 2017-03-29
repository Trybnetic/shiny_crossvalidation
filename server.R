library(ggplot2)
library(shiny)
source("helpers.R")

shinyServer(function(input, output) {
  Data <- reactive({
    simulateData(input$Sample)
  })
  
  output$ModelPlot <- renderPlot({
    plotModels(Data(), input$max.poly)
  })
  # output$FitPlot <- renderPlot({
  #   
  # })

})
