
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dejanos adivinar cuantos clusters tienen tus datos"),
  
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
                )),
      
      tags$hr(),
      checkboxInput('header', 'Tiene encabezado', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Encomillado',
                   c('Ninguno'='',
                     'Doble'='"',
                     'Simple'="'"),
                   '"'),
      
      sliderInput('angle', 'Angulo del grafico 3D', 0, 360, value=45),
      tags$hr(),
      
      checkboxInput('pca', 'PCA', FALSE),

      uiOutput('selectMultiple')
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("EM + BIC",
          uiOutput("numclusters"),
          uiOutput("datosclas"),
          plotOutput("clustPlot"),
          uiOutput("clasificacion"),
          plotOutput("classification"),
          uiOutput("densidad"),
          plotOutput("density"),
          plotOutput("bic")),
        tabPanel("kmeans + AIC",
                 uiOutput("numclusterskmeans"),
                 uiOutput("datosclaskmeans"),
                 plotOutput("datoskmeans"),
                 uiOutput("codo"),
                 plotOutput("codokmeans"))
      )
    )
    
    
    # Show a plot of the generated clusters
    #tabsetPanel(
    #  tabPanel("Clusters",plotOutput("clustPlot"))
    #tabPanel("Algunas Graficas de Apoyo",plotOutput("bla"))
    #)
    
  )
))