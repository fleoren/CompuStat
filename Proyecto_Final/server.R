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
    dataset <- read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote )
    if(nrow(dataset) * ncol(dataset) > 2500)
    {
      index <- sample(1:nrow(dataset), size = nrow(dataset)/2, replace=FALSE)
      dataset <- dataset[index,]
    }
    return(dataset)
  })
  
  output$numclusters <- renderUI({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    if(input$pca==TRUE)
    {
      pca <- prcomp(filedata(), center=TRUE, scale=TRUE)
      mix <- as.data.frame(pca$x[,1:3])
    }
    else
    {
      mix<-as.matrix(filedata())
    }
    
    mixclust = Mclust(mix)
    
    if(input$pca==TRUE)
    {
      suma_pca <- round(sum(pca$sdev[1:3])/sum(pca$sdev) * 100,2)
      etiqueta_pca <- paste("Las primeras 3 componentes explican el ", suma_pca, "% de la varianza.")
      HTML(paste("<span style='font-size: 18px;'>", "Se encontraron ","<strong>",mixclust$G,"</strong>"," clusters.","</span><br /><span>", etiqueta_pca,"</span>"))
    }
    else
    {
      HTML(paste("<span style='font-size: 18px;'>", "Se encontraron ","<strong>",mixclust$G,"</strong>"," clusters.","</span>"))
    }
    
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
    
    if(input$pca==TRUE)
    {
      mix <- prcomp(filedata(), center=TRUE, scale=TRUE)
      mix <- as.data.frame(mix$x[,1:3])
    }
    else
    {
      mix<-as.matrix(filedata())
    }
    
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
    
    if(input$pca==TRUE)
    {
    scatterplot3d(mix_df, pch = 16, color=colors,
                  #main="Clusters Encontrados",
                  col.axis="grey", angle=input$angle,
                  type="h", lty.hplot=2) #yay plot
    }
    else
    {
      scatterplot3d(mix_df[input$columnas], pch = 16, color=colors,
                    #main="Clusters Encontrados",
                    col.axis="grey", angle=input$angle,
                    type="h", lty.hplot=2) #yay plot
    }
  })
  
  output$density <- renderPlot({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    if(input$pca==TRUE)
    {
      mix <- prcomp(filedata(), center=TRUE, scale=TRUE)
      mix <- as.data.frame(mix$x[,1:3])
    }
    else
    {
      mix<-as.matrix(filedata())
    }
    
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
    
    if(input$pca==TRUE)
    {
      mix <- prcomp(filedata(), center=TRUE, scale=TRUE)
      mix <- as.data.frame(mix$x[,1:3])
    }
    else
    {
      mix<-as.matrix(filedata())
    }
    
    mixclust = Mclust(mix)
    
    plot(mixclust, what = "classification", main = "")
  })
  
  output$selectMultiple <- renderUI({
    if (is.null(filedata()))
      return(NULL)
    
    if(input$pca==TRUE)
    {
      mix <- prcomp(filedata(), center=TRUE, scale=TRUE)
      mix <- as.data.frame(mix$x[,1:3])
    }
    else
    {
      mix<-as.matrix(filedata())
    }
    
    selectizeInput('columnas', 'Que columnas quieres visualizar', 
                   choices = colnames(mix), multiple = TRUE,
                   options = list(maxItems = 3))
  })
  
  output$bic <- renderPlot({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    if(input$pca==TRUE)
    {
      mix <- prcomp(filedata(), center=TRUE, scale=TRUE)
      mix <- as.data.frame(mix$x[,1:3])
    }
    else
    {
      mix<-as.matrix(filedata())
    }
    
    BIC = mclustBIC(mix)
    plot(BIC)
  })
  
  
  
  ############ kmeans
  output$datoskmeans <- renderPlot({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    if(input$pca==TRUE)
    {
      pca <- prcomp(filedata(), center=TRUE, scale=TRUE)
      mix <- as.matrix(pca$x[,1:3])
    }
    else
    {
      mix<-as.matrix(filedata())
    }
    
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
    
    if(input$pca==TRUE)
    {
      scatterplot3d(mix, pch = 16, color=colors,
                    #main="Clusters Encontrados",
                    col.axis="grey", angle=input$angle,
                    type="h", lty.hplot=2) #yay plot
    }
    else
    {
      scatterplot3d(mix[input$columnas], pch = 16, color=colors,
                    #main="Clusters Encontrados",
                    col.axis="grey", angle=input$angle,
                    type="h", lty.hplot=2) #yay plot
    }
  })
  
  output$numclusterskmeans <- renderUI({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    if(input$pca==TRUE)
    {
      pca <- prcomp(filedata(), center=TRUE, scale=TRUE)
      mix <- as.matrix(pca$x[,1:3])
    }
    else
    {
      mix<-as.matrix(filedata())
    }
    
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
    
    if(input$pca==TRUE)
    {
      suma_pca <- round(sum(pca$sdev[1:3])/sum(pca$sdev) * 100,2)
      etiqueta_pca <- paste("Las primeras 3 componentes explican el ", suma_pca, "% de la varianza.")
      HTML(paste("<span style='font-size: 18px;'>", "Se encontraron ","<strong>", optimo,"</strong>"," clusters.","</span><br /><span>", etiqueta_pca,"</span>"))
    }
    else
    {
      HTML(paste("<span style='font-size: 18px;'>", "Se encontraron ","<strong>", optimo,"</strong>"," clusters.","</span>"))
    }
    
  })
  
  output$codokmeans <- renderPlot({
    if (is.null(filedata()) || is.null(input$columnas) || length(input$columnas) < 2)
    {
      return(NULL) # si no han seleccionado archivo no hacer nada
    }
    
    if(input$pca==TRUE)
    {
      pca <- prcomp(filedata(), center=TRUE, scale=TRUE)
      mix <- as.matrix(pca$x[,1:3])
    }
    else
    {
      mix<-as.matrix(filedata())
    }
    
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