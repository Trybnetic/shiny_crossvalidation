library(ggplot2)
library(shiny)
library(shinyBS)
library(shinyjs)
library(plotly)
library(polynom)
library(gridExtra)
source("helpers.R")
source("varianceOfFunction.R")
source("plot_crossValidation.R")
source("aicbic.R")

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
    varFunction <- varf(polynomial(Model()), -5, 5)
    varNoise <- varFunction * input$Noise / (1-input$Noise)
    sdNoise <- sqrt(varNoise)
    
    ifelse(sdNoise == 0, input$Noise, sdNoise)
  })
  
  output$ModelPlot <- renderPlotly({
    pdf(NULL)
    plotModels(Data(), input$max.poly)
  })
  # remove the previous plots
  observeEvent(input$Simulate,{
    graphics.off()
    #print(Model())
    })
  
  # update the plots upon pressing button Crossvalidate
  FPlot <- eventReactive(input$Crossvalidate, {
      grid.arrange(
        plot_aic_bic(calc_aic_bic(max.poly = input$max.poly, data = Data())),
        plotCrossValidation(validation_se(Data(),
                                          input$n.bins,
                                          input$max.poly)),
        ncol=2
      )
  })
  # Render the BIC / crossvalidation plot
  output$FitPlot <- renderPlot({FPlot()})

  #observeEvent(input$Simulate, {print(Mod)})
  # plot the generative model
  output$GenerativeModel <- renderPlot({
    plotGenerativeModel(poly_vec = Model(),
                        noise=Noise())

  })

  # 
  # ModelTable <- reactive({
  #   isolate(
  #     m <- matrix(0, nrow=input$Polynom+1, ncol=1),
  #     rownames(m) <- c("Intercept", paste("X", 1:input$Polynom, sep="^")),
  #     m
  #   )
  #   
  # })
  
  Mod <- matrix(c(5,2,1,2,4,5), nrow=6) #reactiveValues()
  #Mod$table <- matrix(c(5,2,1,2), nrow=4)
  observeEvent(input$Polynom, {
      Mod <<- matrix(0, nrow=input$Polynom+1, ncol=1)#ModelTable(input$Polynom)
  })
  # Mod$table <- eventReactive(input$Polynom,{
  #   print(input$Polynom)
  #   ModelTable(input$Polynom)
  # })
  
  Model <- reactive({
    #Terrible, terrible workaround
      selCoef <- which(as.list(
        c("Intercept",paste("X", 1:input$Polynom, sep="^")))==input$SelectCoef)
      Mod[selCoef,] <<- input$Coefficient
      Mod
  })

  output$CoefSelect <- renderUI({
    selectInput("SelectCoef", "Select the coefficient", input$Polynom,
                choices = as.list(c("Intercept", paste("X", 1:input$Polynom, sep="^"))))
  })
})
