# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(mclust)
library(scatterplot3d)
library(rgl)

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
  
  output$bic <- renderPlot({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    mix<-filedata()[input$columnas]
    BIC = mclustBIC(mix)
    plot(BIC)
  })
  
  
  
  ############ kmeans
  output$datoskmeans <- renderPlot({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    mix<-filedata()[input$columnas]
    
    minimizar <- seq(1,9)
    for(i in 1:9)
    {
      # K-Means Cluster Analysis
      fit <- kmeans(mix, i) # 5 cluster solution
      
      rss <- sum(fit$withinss)
      aic <- nrow(mix)/3*i + nrow(mix)*log(rss/nrow(mix))
      
      minimizar[i] <- aic
    }
    
    optimo <- which.min(minimizar)
    fit <- kmeans(mix, optimo)
    # get cluster means 
    aggregate(mix,by=list(fit$cluster),FUN=mean)
    # append cluster assignment
    mix <- data.frame(mix, fit$cluster)
    
    #partir colors en K (num clusters)
    a <- rainbow(201)
    b <- a[seq(1, length(a), 201/optimo)]
    #colors <- sample(rainbow(201), size=mixclust$G, replace = FALSE) this made it random
    colors <- b[as.numeric(mix$fit.cluster)] #for coloring clusters
    
    scatterplot3d(mix[input$columnas], pch = 16, color=colors,
                  #main="Clusters Encontrados",
                  col.axis="grey", angle=input$angle,
                  type="h", lty.hplot=2) #yay plot
  })
  
  output$numclusterskmeans <- renderUI({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    mix<-filedata()[input$columnas]
    
    minimizar <- seq(1,9)
    for(i in 1:9)
    {
      # K-Means Cluster Analysis
      fit <- kmeans(mix, i) # 5 cluster solution
      
      rss <- sum(fit$withinss)
      aic <- nrow(mix)/3*i + nrow(mix)*log(rss/nrow(mix))
      
      minimizar[i] <- aic
    }
    
    optimo <- which.min(minimizar)
    
    HTML(paste("<span style='font-size: 18px;'>", "Se encontraron ","<strong>", optimo,"</strong>"," clusters.","</span>"))
  })
  
  output$codokmeans <- renderPlot({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    mix<-filedata()[input$columnas]
    
    aic <- seq(1,9)
    for(i in 1:9)
    {
      # K-Means Cluster Analysis
      fit <- kmeans(mix, i) # 5 cluster solution
      
      rss <- sum(fit$withinss)
      temp <- nrow(mix)/3*i + nrow(mix)*log(rss/nrow(mix))
      
      aic[i] <- temp
    }
    
    clusters <- 1:9
    plot(clusters, aic, type="b",
         lty=1,  pch=1)
  })
  
  output$codo <- renderUI({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    HTML(paste("<h4 style='text-align:center;'>","Numero de clusters","</h4>"))
  })
  
  output$datosclaskmeans <- renderUI({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    HTML(paste("<h4 style='text-align:center;'>","Como se ven tus datos","</h4>"))
  })
  
  
})