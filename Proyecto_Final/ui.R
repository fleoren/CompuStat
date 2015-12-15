
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Déjanos Adivinar Cuántos Clusters Tienen tus Datos"),

  # Sidebar... here would go the "choose file" button"
  sidebarLayout(
    sidebarPanel(
      #sliderInput("bins",
       #           "Number of bins:",
       #           min = 1,
      #            max = 50,
        #          value = 30)
    ),

    # Show a plot of the generated clusters
    tabsetPanel(
      tabPanel("Clusters",plotOutput("clustPlot"))
      #tabPanel("Algunas Gráficas de Apoyo",plotOutput("bla"))
    )
  )
))
