set.seed(917)

f <- function(x){
  (2/sqrt(2*pi))*exp(-x^2/2)
}

g<-function(x){
  exp(-x)
}

plot(f,ylim=c(0,2),xlim=c(0,6))
plot(g,add=TRUE,col="red",ylim=c(0,2),xlim=c(0,6))

h<-function(x){
  f(x)/g(x)
}

plot(h,xlim=c(0.1,4))

M<-2


genera.una<-function(...){
#Aceptacion rechazo
MAXITER<-10000
continuar =  TRUE
iter<-1

while(continuar | iter<=MAXITER){
### paso 1 hacer exponencial y uniforme
Y<-rexp(1)
U<-runif(1)
# ----- #
### paso 2 acepto o rechazo?
if(U<=f(Y)/M*g(Y)){
  X<-Y
  continuar = FALSE
}
iter<-iter+1
}
return(X)
}

genera.muchas<-function(n){
  sapply(1:n,FUN=genera.una)
}

vec<-genera.muchas(500)

hist(vec,breaks=10)

genera.normales<-function(n){
  sample(c(-1,1),size=n,replace=TRUE)*genera.muchas(n)
  
}

vec1<-genera.normales(500)
hist(vec1,breaks=20)
