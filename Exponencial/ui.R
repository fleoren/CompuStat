
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Simulacion de Exponenciales"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda",
                  "Lambda",
                  min = 1,
                  max = 50,
                  value = 10),
      numericInput("nsim",
                  "Values",
                  min = 1,
                  max = 100000,
                  value = 100),
      tableOutput("table")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
    
  )
))
