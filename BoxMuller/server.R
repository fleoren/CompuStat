
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

BoxM<-function(nsim){
  x<-c(1:nsim)
  i=1
  while (i<nsim+1){
    u1<-runif(1);u2<-runif(1)
    x[i]<-sqrt(-2*log(u1))*cos(2*pi*u2)
    x[i+1]<-sqrt(-2*log(u1))*sin(2*pi*u2)
    i=i+2  
  }
  return(x)
}


shinyServer(function(input, output) {
  
  
  output$distPlot <- renderPlot({
    v<-sort(BoxM(nsim=input$nsim),decreasing=FALSE)
    f<-dnorm(v)
    plot(1, type="n", axes=T, xlab="x", ylab="f(x)",xlim=c(min(v),max(v)),ylim=c(min(f),max(f)))
    lines(v,f,col='blue')
    
    tabl<-t(t(sort(v,decreasing=FALSE))) 
    output$table <- renderTable(tabl)
    
  })
  
})

