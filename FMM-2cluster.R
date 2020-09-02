
library(boot)
library(coda)
library(R2WinBUGS)

# install terlebih dahulu link berikut
# https://sourceforge.net/projects/mcmc-jags/files/
# Untuk Menghitung Nilai DIC

library(R2jags)

#DATA
dataset1 <- datasets::iris

# Tambahkan Fitur T
a<-c(1)
b<-data.frame(rep(c('NA'), each=148))
c<-c(2)
d<-c(3)
e<-c(4)

f<-rbind(a,b,c)
names(f)<-'T2'

g<-rbind(a,b,d)
names(g)<-'T3'

h<-rbind(a,b,e)
names(h)<-'T4'

data<-cbind(dataset1,f,g,h)
head(data)

var1 <- data[,1:4]

N <- nrow(dataset1) 

Sepal.Length<-var1$Sepal.Length
Sepal.Width<-var1$Sepal.Width
Petal.Length<-var1$Petal.Length
Petal.Width<-var1$Petal.Width

T <- data$T2
alpha <- c(1,1)
df<-list("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","N","alpha","T")

#INIT
inits = function() {
  lambda1 = mean(var1$Sepal.Length[1:100]) +rnorm(1,0,.01)
  theta = 50
  sigma2 = var(var1$Sepal.Length[1:100])
  return(list(lambda = c(lambda1, NA),
              theta1 = theta,
              tau = 1/sigma2,
              P = c(100, 100-60)/60))
}

#MODEL
mixmodel=function() {
  for( i in 1 : N ) {
    Sepal.Length[i] ~ dnorm(mu1[i], tau)
    Sepal.Width[i] ~ dnorm(mu2[i], tau)
    Petal.Length[i] ~ dnorm(mu3[i], tau)
    Petal.Width[i] ~ dnorm(mu4[i], tau)
    mu1[i] <- lambda[T[i]]
    mu2[i] <- lambda[T[i]]
    mu3[i] <- lambda[T[i]]
    mu4[i] <- lambda[T[i]]
    T[i] ~ dcat(P[]) }
  P[1:2] ~ ddirch(alpha[])
  theta1 ~ dnorm(0.0, 1.0E-6)%_%I(0.0, )
  lambda[1] ~ dnorm(0.0, 1.0E-6)
  lambda[2] <- lambda[1] + theta1
  tau ~ dgamma(0.001,0.001)
  sigma <- 1 / sqrt(tau)
}

parameters <- c("lambda","sigma","P")

FMM.fit<-jags(df, inits=inits, parameters, n.chains=1, n.burnin=100000, 
              n.iter=200000, model.file=mixmodel, DIC=TRUE)

# Jika ada notif merah setelah melakukan fitting jangan khawatir langsung PRINT saja koding print(FMM.fit)
print(FMM.fit)

