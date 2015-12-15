
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#install.packages("tseries")
library(tseries)

shinyUI(fluidPage(

  # Application title
  titlePanel("Bootstrapping for the parameter alpha of an AR(1) process"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("a",
                  "Alpha:",
                  min = -1,
                  max = 1,
                  value = 0.5),
      sliderInput("n",
                  "Points for time series",
                  min = 1,
                  max = 5000,
                  value = 500),
      sliderInput("nb",
                  "Bootstrap samples:",
                  min = 1,
                  max = 25000,
                  value = 5000)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series Plot",plotOutput("TSPlot")),
        tabPanel("Bootstrap for Alpha",plotOutput("BSPlot"))
      )
    )
  )
))
