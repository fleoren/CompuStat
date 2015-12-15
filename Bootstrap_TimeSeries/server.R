
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

    #create an AR(1) process with specified parameters
    create_time_series<-function(alpha,n){
      x <- double(n+100)
      e <- rnorm(n+100) 
      x[1] <- rnorm(1)
      for(i in 2:(n+100)) {
        x[i] <- alpha * x[i-1] + e[i]
      }
      x <- ts(x[-(1:100)])
    }
    
    #the function of interest to bootstrap
    acflag1 <- function(x)
    {
      xo <- c(x[,1], x[1,2])
      xm <- mean(xo)
      return(mean((x[,1]-xm)*(x[,2]-xm))/mean((xo-xm)^2))
    }

shinyServer(function(input, output) {

#reactive
  xx <- reactive(create_time_series(alpha=input$a,n=input$n))
  output$TSPlot <- renderPlot({
    # Generate AR(1) process
    x <- xx()
   # draw the time series
    plot(x,main="Serie de tiempo generada",xlab="tiempo")
  })
  
  output$BSPlot <- renderPlot({
    x <- xx()
    #estimate alpha with R package
    alpha_estimate<-ar(x, aic = FALSE, order.max = 1)
    alpha_est<-alpha_estimate$ar
    #the actual process of bootstrapping
    boot<-tsbootstrap(x, nb=input$nb, statistic=acflag1, m=2)
    #plot
    plot(density(boot$statistic),main="Densidad de alpha - Bootstrapped",
         xlab="alpha",ylab='densidad',col="blue")
    lines(density(boot$statistic, adjust=1.5), lty="dotted", col="darkgreen", lwd=2) 
    abline(v=input$a,col="red")
    abline(v=alpha_est,col="grey")
    dens<-density(boot$statistic)
    #coordenates to put the legend - only to assure it fits
    coor_x<-max(boot$statistic)-0.07
    coor_y<-max(dens$y)-mean(dens$y)
    legend(coor_x,coor_y,c("Densidad Boostrap","Dens. BStrap Suavizada","Valor Real","Valor Estimado por R"),
           lwd=2,col=c("blue","darkgreen","red","grey"),cex = .65)
  })

})
