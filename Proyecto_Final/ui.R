
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dejanos Adivinar Cuantos Clusters Tienen tus Datos"),
  
  # Sidebar... here would go the "choose file" button"
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Elige un archivo',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Tiene encabezado', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Encomillado',
                   c(None='',
                     'Doble'='"',
                     'Simple'="'"),
                   '"'),
      # otra
      
      sliderInput('angle', 'Angulo del grafico', 0, 360, value=45)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Clusters",plotOutput("clustPlot"))
      )
    )
    
    # Show a plot of the generated clusters
    #tabsetPanel(
    #  tabPanel("Clusters",plotOutput("clustPlot"))
    #tabPanel("Algunas Graficas de Apoyo",plotOutput("bla"))
    #)
  )
))