setwd('/Users/fernanda/Dropbox/batmelon/Estadística Computacional/Imputacion')
library(foreign)
#install.packages('mvtnorm')
library(mvtnorm)
library(e1071)

table <- read.dta("datos_politicos.dta") # LEEMOS LOS DATOS DE FORMATO STATA
ano <- 1986 # POR EL MOMENTO ESCOJAMOS UN SOLO A?O PUES NO SABEMOS NADA DE DATOS PANEL
data <- table[table$year==ano, ]
labels <- paste(names(data), attributes(data)$var.labels, sep=": ") # NOMBRES DE LAS VARIABLES
#edit(labels) # para ver la explicacion de cada variable

Y <- data$reg # INDICADORA DE SI SE ES O NO UNA DEMOCRACIA
list.depend <- c("level", "open", "g", "strikes", "govpwt", "ls", "invpwt",
                 "fertil", "lfagric", "popg", "femsec", "EnergProd") # VARIABLES ECON?MICAS EXPLICATIVAS

#seleccionamos solamente las numericas para trabajar
#X<-as.data.frame(data[list.depend]) #esto tambien funciona
X <- subset(data,select=list.depend)
for(j in 1:ncol(X)) X[ ,j] <- as.numeric(X[ ,j])
row.names(X)<-data$name

nrows<-nrow(X)
ncols<-ncol(X)
m=5 #NUMERO DE IMPUTACIONES BOOTSTRAP
tol <- 1e-2
res <- list()
imputed.sets <- list()
pred.success <- numeric(m)

for (rep in 1:m){
  #BOOTSTRAP
  print(paste("Bootstrap ",rep))
  samp <- sample(1:nrows,nrows,replace=TRUE)
  Xb <- X[samp,]
  #INICIALIZACION
  M <- is.na(Xb)
  Sigma <- cov(Xb[complete.cases(Xb),])
  sd.vec<-sqrt(diag(Sigma))
  mu<-apply(Xb[complete.cases(Xb),],2,mean)
  for(i in 1:nrows) for (j in 1:ncols) if(M[i,j]) Xb[i,j] <- rnorm(1,mu[j],sd.vec[j])
  logv<- sum(apply(Xb,1,function(row) log(dmvnorm(row,mu,Sigma))))
  #ITERACION
  iter <- 1
  repeat{
    #VALOR ACTUAL DE LA VEROSIMILITUS
    #ITERACIONES POR VARIABLES
    for(j in 1:ncol(Xb)){
      ind <- as.matrix(Xb[,j],ncol=1)
      dep <- as.matrix(Xb[,-j])
      mod <- lm(ind ~dep)
      pred <- predict(mod)
      #for (k in 1:nrows)  if (M[k,j]) Xb[k,j] <- pred[M[k,j]]
      Xb[M[,j],j] <- pred[M[,j]]
    }
    #NUEVA MATRIZ DE COVARIANZAS
    Sigma <- cov(Xb)
    mu <- apply(Xb,2,mean)
    logv[iter+1] <- sum(apply(Xb,1,function(row) log(dmvnorm(row,mu,Sigma))))
    if(abs(logv[iter+1]-logv[iter])<tol) break
    iter <- iter +1
  }
  print(paste("   - iteraciones totales: ",iter))
  imputed.sets[[rep]] <- Xb
  #GRAFICA
  plot(logv[-(1:3)],type='l',col='blue',main=paste("Bootstrap ",rep))
  #MODELO
  data.full <- data.frame(Y[samp],Xb)
  names(data.full)[1]<-'Y'
  res[[rep]] <- glm(Y ~. ,data=data.full,family="binomial") #REGRESION LOGIT
  guess<-round(predict(res[[rep]],type="response"))
  pred.success[rep]<-sum(guess==data.full$Y)/nrows
}


#Haz pooling para presentar los datos juntos.
beta.all <- matrix(0,nrow=ncols,ncol=m)
for(rep in 1:m){
  beta.all[,rep] <- coef(res[[rep]])[-1]
}
#PROMEDIO DE LAS BETAS
beta.estims <- apply(beta.all,1,mean)
#ESTIMACION DE LAS VARIANZAS
beta.var.within<-numeric(ncols)
for (rep in 1:m){
  beta.var.within <- beta.var.within +
    (summary(res[[rep]])$coefficients[,2][-1])^2/m  
}
beta.var.between <- apply(beta.all,1,var)
beta.var <- beta.var.within + (1+1/m)*beta.var.between

#Z-VALUES FINALES
table <- data.frame(beta=beta.estims,sd=sqrt(beta.var))
table$low<-table$beta-1.96*table$sd/sqrt(nrows)
table$high<-table$beta+1.96*table$sd/sqrt(nrows)

