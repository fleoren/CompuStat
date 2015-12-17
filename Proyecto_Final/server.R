# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#install.packages('mclust')
library(mclust)      
#install.packages("devtools")
library("devtools")
#install_github(repo = "bryanhanson/ChemoSpec@master")
library("ChemoSpec")
#install.packages("car")
library(car)
#install.packages("scatterplot3d")
library("scatterplot3d")

shinyServer(function(input, output) {

  filedata <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
  
  output$numclusters <- renderUI({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    mix<-as.matrix(filedata()[input$columnas])
    mixclust = Mclust(mix)
    
    HTML(paste("<span style='font-size: 18px;'>", "Se encontraron ","<strong>",mixclust$G,"</strong>"," clusters.","</span>"))
  })
  
  output$datosclas <- renderUI({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    HTML(paste("<h4 style='text-align:center;'>","Como se ven tus datos","</h4>"))
  })
  
  output$clasificacion <- renderUI({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    HTML(paste("<h4 style='text-align:center;'>","Clasificacion","</h4>"))
  })
  
  output$densidad <- renderUI({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    HTML(paste("<h4 style='text-align:center;'>","Densidad","</h4>"))
  })
  
  output$clustPlot <- renderPlot({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    mix<-as.matrix(filedata()[input$columnas])
    # generate the clustering model
    mixclust = Mclust(mix)           # initialize EM with hierarchical clustering, execute BIC and EM
    #add classification as a variable, then use as data frame for easy plotting
    mix<-cbind(mix,mixclust$classification)
    colnames(mix)[length(colnames(mix))] <- "class"
    mix_df<-as.data.frame(mix)
    
    #partir colors en K (num clusters)
    a <- rainbow(201)
    b <- a[seq(1, length(a), 201/mixclust$G)]
    #colors <- sample(rainbow(201), size=mixclust$G, replace = FALSE) this made it random
    colors <- b[as.numeric(mix_df$class)] #for coloring clusters
    
    scatterplot3d(mix_df[input$columnas], pch = 16, color=colors,
                         #main="Clusters Encontrados",
                  col.axis="grey", angle=input$angle,
                  type="h", lty.hplot=2) #yay plot
    
  })
  
  output$density <- renderPlot({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    mix<-filedata()[input$columnas]
    
    dens = densityMclust(mix)
    
    a <- rainbow(201)
    b <- a[seq(1, length(a), 201/dens$G)]
    
    plot(dens, what = "density", data = mix, col=b, main = "")
  })
  
  output$classification <- renderPlot({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    mix<-filedata()[input$columnas]
    
    mixclust = Mclust(mix)
    
    plot(mixclust, what = "classification", main = "")
  })
  
  output$selectMultiple <- renderUI({
    if (is.null(filedata()))
      return(NULL)
      
    data <- as.matrix(filedata())
    selectizeInput('columnas', 'Que columnas quieres visualizar', 
                   choices = colnames(data), multiple = TRUE,
                   options = list(maxItems = 3))
  })
  
})
