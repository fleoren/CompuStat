
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
  
  #TODO ESTO DEBE SER BORRADO CUANDO FUNCIONE UPLOADFILE
  #x1 = rnorm(n=20, mean=1, sd=1)   # get 20 normal distributed points for x axis with mean=1 and std=1 (1st class)
  #y1 = rnorm(n=20, mean=1, sd=1)   # get 20 normal distributed points for x axis with mean=1 and std=1 (1st class)
  #z1 = rnorm(n=20, mean=1, sd=1)   # get 20 normal distributed points for x axis with mean=1 and std=1 (1st class)
  #x2 = rnorm(n=20, mean=5, sd=1)   # get 20 normal distributed points for x axis with mean=5 and std=1 (2nd class)
  #y2 = rnorm(n=20, mean=5, sd=1)   # get 20 normal distributed points for x axis with mean=5 and std=1 (2nd class)
  #z2 = rnorm(n=20, mean=5, sd=1)   # get 20 normal distributed points for x axis with mean=5 and std=1 (2nd class)
  #x3 = rnorm(n=20, mean=-2, sd=1)   # get 20 normal distributed points for x axis with mean=5 and std=1 (2nd class)
  #y3 = rnorm(n=20, mean=-2, sd=1)   # get 20 normal distributed points for x axis with mean=5 and std=1 (2nd class)
  #z3 = rnorm(n=20, mean=-2, sd=1)   # get 20 normal distributed points for x axis with mean=5 and std=1 (2nd class)
  #mix = matrix(nrow=60, ncol=3)    # create a dataframe matrix 
  #mix[,1] = c(x1, x2, x3)              # insert first class points into the matrix
  #mix[,2] = c(y1, y2, y3)              # insert second class points into the matrix
  #mix[,3] = c(z1, z2 ,z3)              # insert third class points into the matrix

  output$clustPlot <- renderPlot({

    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL) # si no han seleccionado archivo no hacer nada
    
    datos <- read.csv(inFile$datapath, header = input$header,
                      sep = input$sep, quote = input$quote)
    
    mix<-as.matrix(datos)
    # generate the clustering model
    mixclust = Mclust(mix)           # initialize EM with hierarchical clustering, execute BIC and EM
    #add classification as a variable, then use as data frame for easy plotting
    mix<-cbind(mixclust$classification,mix)
    mix_df<-as.data.frame(mix)
    
    #partir colors en K (num clusters)
    a <- rainbow(201)
    b <- a[seq(1, length(a), 201/mixclust$G)]
    #colors <- sample(rainbow(201), size=mixclust$G, replace = FALSE) this made it random
    colors <- b[as.numeric(mix_df$V1)] #for coloring clusters
    
    scatterplot3d(mix_df[1:3], pch = 16, color=colors,main="Clusters Encontrados",
                  col.axis="grey", angle=input$angle) #yay plot

  })

})
