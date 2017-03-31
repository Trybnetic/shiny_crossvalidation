library(ggplot2)
library(shiny)
library(shinyBS)
library(shinyjs)
library(plotly)
library(polynom)
source("helpers.R")
source("varianceOfFunction.R")
shinyServer(function(input, output) {
  Data <- reactive({
    if(input$Simulate == 0){
      NULL
    }
    isolate(
      simulateData(input$Sample,  Model(), Noise())
    )
  })
  
  # Calculates the sd of the noise based on the variance of the selected function
  # and the proportion of noise in the total variance 
  Noise <- reactive({
    varFunction <- varf(polynomial(Model()), -20, 20)
    varNoise <- varFunction * input$Noise / (1-input$Noise)
    sdNoise <- sqrt(varNoise)
    
    ifelse(sdNoise == 0, input$Noise, sdNoise)
  })
  
  output$ModelPlot <- renderPlotly({
    pdf(NULL)
    plotModels(Data(), input$max.poly)
  })
  output$FitPlot <- renderPlot({
    plot_aic_bic(calc_aic_bic(max.poly = input$max.poly, data = Data()))
  })

  # plot the generative model
  output$GenerativeModel <- renderPlot({
    plotGenerativeModel(poly_vec = Model(),
                        #model = Model(),
                        noise=Noise())

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
