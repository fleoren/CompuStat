
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

Exp<-function(nsim,lambda){
  u<-runif(nsim)
  v<-log(1-u)/-lambda
  v<-sort(v, decreasing = TRUE)
  return(v)
}

fexp<-function(v,lambda){
  f<-lambda*exp(-lambda*v)
  return(f)
}

shinyServer(function(input, output) {

  
  output$distPlot <- renderPlot({
    v<-Exp(nsim=input$nsim,lambda=input$lambda)
    f<-fexp(v=v,lambda=input$lambda)
    plot(1, type="n", axes=T, xlab="x", ylab="f(x)",xlim=c(0,max(v)),ylim=c(0,max(f)))
    lines(v,f,col='blue')
    
  tabl<-t(t(sort(v,decreasing=FALSE))) 
  output$table <- renderTable(tabl)
    
  })
  
})
