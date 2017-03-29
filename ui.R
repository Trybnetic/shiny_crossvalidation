library(ggplot2)
library(shiny)
source("helpers.R")

shinyUI(fluidPage(

  # Application title
  titlePanel("AIC vs crossvalidation comparison"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("Sample",
                  "Sample Size:",
                  min = 20,
                  max = 1000,
                  value = 100),
      sliderInput("max.poly",
                  "Select the maximum degree of polynomial:",
                  min = 1,
                  max = 10,
                  value = 7)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("ModelPlot"),
      plotOutput("FitPlot")
    )
  )
))
