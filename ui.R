library(ggplot2)
library(shiny)
library(shinyBS)
library(shinyjs)
source("helpers.R")

shinyUI(fluidPage(

  # Application title
  titlePanel("AIC vs crossvalidation comparison"),
  
  bsModal(id = "DefineModel", title = "Define your generative model",
          size = "large", trigger = "ModelInit",
          sidebarPanel(  
            numericInput("Polynom",
                         "Select the highest polynom of the generative function",
                         min = 1,
                         max = 10,
                         value = 3),
            uiOutput("CoefSelect"),
            # textInput("Coefficients", "Input the coefficients for the polynomials (and intercept)",
            #          value="5,2,1,2"),
            numericInput("Coefficient", "Select the coefficient", value=0),
            sliderInput("Noise", "Define the amount of noise", min = 0, max = 1, step=0.01, value=0.3),
            #actionButton(inputId = "initiatePlotGenerative", label = "Plot the model"),
            actionButton(inputId = "saveGenerative", label="Save the model")
          ),
          mainPanel(plotOutput("GenerativeModel"))
          ),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = "ModelInit", label = "Define your own model"),
      numericInput("Sample",
                  "Sample Size:",
                  min = 20,
                  max = 1000,
                  value = 100),
      sliderInput("max.poly",
                  "Select the maximum degree of polynomial:",
                  min = 1,
                  max = 10,
                  value = 7),
      actionButton(inputId="Simulate", "Simulate new data")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("ModelPlot"),
      plotOutput("FitPlot")
    )
  )
))
