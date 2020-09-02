
library(readxl)
library(boot)
library(coda)
library(R2WinBUGS)

# install terlebih dahulu link berikut
# https://sourceforge.net/projects/mcmc-jags/files/
# Untuk Menghitung Nilai DIC

library(R2jags)

#DATA
dataset1 <- read_excel('Data.xlsx')

var1 <- dataset1[,1:6]
T <- dataset1$TR4

alpha <- c(1,1,1,1)
N <- nrow(dataset1)
NLR<-var1$NLR
LMR<-var1$LMR
PLR<-var1$PLR
HPR<-var1$HPR
PWR<-var1$PWR
LWR<-var1$LWR

df<-list("NLR","LMR","PLR","HPR","PWR","LWR","N","alpha","T")

#INIT
inits3 = function() {
  lambda1 = mean(var1$NLR[1:40]) +rnorm(1,0,.01)
  theta = mean(var1$NLR[41:60])-lambda1
  theta2 = 60
  theta3 = 40 
  sigma2 = var(var1$NLR[1:40])
  return(list(lambda = c(lambda1, NA, NA, NA),
              theta1 = theta,
              theta2 = theta2,
              theta3 = theta3,
              tau = 1/sigma2,
              P = c(40, 60-40, 40, 60)/60))
}

#MODEL
mixmodel3=function() {
  for( i in 1 : N ) {
    NLR[i] ~ dnorm(mu1[i], tau)
    LMR[i] ~ dnorm(mu2[i], tau)
    PLR[i] ~ dnorm(mu3[i], tau)
    HPR[i] ~ dnorm(mu4[i], tau)
    PWR[i] ~ dnorm(mu5[i], tau)
    LWR[i] ~ dnorm(mu6[i], tau)
    mu1[i] <- lambda[T[i]]
    mu2[i] <- lambda[T[i]]
    mu3[i] <- lambda[T[i]]
    mu4[i] <- lambda[T[i]]
    mu5[i] <- lambda[T[i]]
    mu6[i] <- lambda[T[i]]
    T[i] ~ dcat(P[]) }
  P[1:4] ~ ddirch(alpha[])
  theta1 ~ dnorm(0.0, 1.0E-6)%_%I(0.0, )
  theta2 ~ dnorm(0.0, 1.0E-6)%_%I(0.0, )
  theta3 ~ dnorm(0.0, 1.0E-6)%_%I(0.0, )
  lambda[1] ~ dnorm(0.0, 1.0E-6)
  lambda[2] <- lambda[1] + theta1
  lambda[3] <- lambda[2] + theta2
  lambda[4] <- lambda[3] + theta3
  tau ~ dgamma(0.001,0.001)
  sigma <- 1 / sqrt(tau)
}

parameters <- c("lambda","sigma","P")

FMM.fit<-jags(df, inits=inits3, parameter, n.chains=1, n.burnin=100000, 
              n.iter=200000, model.file=mixmodel3, DIC=TRUE)
print(FMM.fit)

# ada notif merah setelah melakukan fitting jangan khawatir langsung PRINT saja
# Koding print(FMM.fit)
