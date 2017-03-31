library(ggplot2)
library(shiny)
library(shinyBS)
library(shinyjs)
library(plotly)
source("helpers.R")

shinyServer(function(input, output) {
  Data <- reactive({
    if(input$Simulate == 0){
      NULL
    }
    isolate(
      simulateData(input$Sample,  Model(), input$Noise)
    )
  })

  output$ModelPlot <- renderPlotly({
    plotModels(Data(), input$max.poly)
  })
  output$FitPlot <- renderPlot({
    plot_aic_bic(Data())
  })

  # plot the generative model
  output$GenerativeModel <- renderPlot({
    plotGenerativeModel(polynom = input$Polynom,
                        model = Model(),
                        noise=input$Noise)

  })

  ModelTable <- reactive({
    m <- matrix(0, nrow=input$Polynom+1, ncol=1)
    rownames(m) <- c("Intercept", paste("X", 1:input$Polynom, sep="^"))
    m
  })
  Mod <- reactiveValues()
  Mod$table <- matrix(c(5,2,1,2), nrow=4)
  observeEvent(input$Polynom,{
    Mod$table <- ModelTable()
  })

  Model <- reactive({
    Mod$table[input$SelectCoef,] <- input$Coefficient
    Mod$table
  })

  output$CoefSelect <- renderUI({
    selectInput("SelectCoef", "Select the coefficient", input$Polynom,
                choices = as.list(c("Intercept", paste("X", 1:input$Polynom, sep="^"))))
  })
})
