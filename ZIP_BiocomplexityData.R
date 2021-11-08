##Zero inflated Poisson model for the Biocomplexity Data

##Load packages
library(ggplot2)
library(tidyverse)
library(gapminder)
library(lattice)
library(parallel)
library(MASS)
require(pscl) # alternatively can use package ZIM for zero-inflated models
library(lmtest)
library(simr)
library(readr)
library(dplyr)
require(boot)
library(VGAM)


##Load data
dat <- read.csv("Biocomplexity_data.csv", header = TRUE, sep = ",")
head(dat)
dim(dat)

##Log transformation distance to anthropogenic features
dat$distanceanthrop <- log(dat$distanthrop)
hist(dat$distanceanthrop)


##remove rows with NA values from covariates
ww<- which(!is.na(dat$tcmean))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$tamean))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rhmean))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rfmean))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rfsum))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$wsmean))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$tcmean_lag1))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$tamean_lag1))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rhmean_lag1))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rfmean_lag1))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rfsum_lag1))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$tcmean_lag2))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$tamean_lag2))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rhmean_lag2))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rfmean_lag2))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rfsum_lag2))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$tcmean_lag3))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$tamean_lag3))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rhmean_lag3))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rfmean_lag3))
dat<- dat[ww,]
dim(dat)

ww<- which(!is.na(dat$rfsum_lag3))
dat<- dat[ww,]
dim(dat)

dat1<- dat[ww,]
dim(dat1)
head(dat1)

##Check for correlations
soc<- c(4,5,6,16,17,20,21,35,36,37,38)
summary(dat1[,soc])
pairs(dat1[,soc])
 
##histogram of mosquito counts
hist(dat1$countCO2Culex, xlab = "Counts Culex mosqutioes_CO2 traps", main = NULL, xlim = c(0,60),
     ylim = c(0,4500))


## zero inflated Poisson model

m1 <- zeroinfl(countCO2Culex ~  month + tcmean + tamean + rhmean + wsmean + rfmean + rfsum + distanceanthrop + elev +
                 tcmean_lag1 + tamean_lag1 + rhmean_lag1 + rfmean_lag1 + rfsum_lag1 + tcmean_lag2 + tamean_lag2 +
                 rhmean_lag2 + rfmean_lag2 + rfsum_lag2 +  tcmean_lag3 + tamean_lag3 + rhmean_lag3 + rfmean_lag3 + rfsum_lag3 |
                 month + tcmean + tamean + rhmean + wsmean + rfmean + rfsum + distanceanthrop + elev + tcmean_lag1 +
                 tamean_lag1 + rhmean_lag1 + rfmean_lag1 + rfsum_lag1 + tcmean_lag2 + tamean_lag2 + rhmean_lag2 + rfmean_lag2 + rfsum_lag2 +
                 tcmean_lag3 + tamean_lag3 + rhmean_lag3 + rfmean_lag3 + rfsum_lag3,
               dist = 'poisson', data = dat1)

summary(m1)



##Calculate the dispersion statistic
E1 <- resid(m1, type = "pearson")
N  <- nrow(dat1)
N
p  <- length(coef(m1))
p
sum(E1^2) / (N - p)


## Check for residuals 
par(mfrow = c(1,1), mar = c(5,5,2,2))
E1 <- resid(m1, type = "pearson")
plot(x = fitted(m1),
     y = E1, ylim = c(-5,10),
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)


##Simulate data from the ZIP model
MyDispersionZIP <- function(y, ExpY, VarY,  N, p){
  e <- (y - ExpY) / sqrt(VarY)
  sum(e^2) / (N - p) 
}

X <- model.matrix( ~  month + 
                     tcmean + tamean + rhmean + wsmean + rfmean + rfsum + distanceanthrop + elev + tcmean_lag1 + tamean_lag1 + 
                     rhmean_lag1 + rfmean_lag1 + rfsum_lag1 + tcmean_lag2 + tamean_lag2 + rhmean_lag2 + rfmean_lag2 + rfsum_lag2 +
                     tcmean_lag3 + tamean_lag3 + rhmean_lag3 + rfmean_lag3 + rfsum_lag3,
                   data = dat1)

##Extract regression parameters
beta <- coef(m1, model = "count")
beta
gamma <- coef(m1, model = "zero")
gamma

mu   <- exp (X %*% beta)
mu

Pi   <- exp (X %*% gamma) / (1 +exp (X %*% gamma))
Pi

##Calculate the expected value and the variance from the model
ExpY <- (1 - Pi) * mu
VarY <- (1 - Pi) * (mu + Pi * mu * mu)

##Simulate data using a zero inflated poisson distribution 
DispersionSim <- vector(length = 100000)
Ysim <- matrix(nrow = N, ncol = 100000)
for (i in 1:100000){
  Ysim[,i] <- rzipois(N, lambda = mu, pstr0 = Pi)
  DispersionSim[i] <- MyDispersionZIP(Ysim[,i], 
                                      ExpY, VarY, N, p)
}


## Plot the dispersion statistic
par(mar = c(5,5,2,2), cex.lab = 1.5)
hist(DispersionSim, breaks =60, main = "", xlim= c(0.7, 4), xlab = "Dispersion statistics", ylab = "Frequency")
abline(v = sum(E1^2) / (N - p), col = 2, lty = 2, lwd = 3)
legend(0.1, 27000, paste('', "A"), text.font = 3, bty = "n", cex= 2.5)

##Simulate the zero portion of teh model
Dispersion <- sum(E1^2) / (N - p)
sum(DispersionSim < Dispersion) / 100000
sum(dat1$countCO2Culex == 0) / N
sum(Ysim[,1] == 0) / N
sum(Ysim[,2] == 0) / N

zeros <- vector(length = 100000)
for (i in 1:100000){
  zeros[i] <- sum(Ysim[,i] == 0) / N
}


##plot the simulated zeros and check if the model predicts the zeroes in the data
par(mar = c(5,5,2,2))
plot(table(zeros), 
     xlim = c(0.5, 0.9),
     axes = FALSE,
     xlab = "Percentage of zeros",
     ylab = "Frequency",
     cex.lab = 1.5)
axis(2)
axis(1, at = c(0.5, 0.6, 0.7, 0.8, 0.9),
     labels = c("50%", "60%", "70%", "80%", "90%"))             
points(x = sum(dat1$countCO2Culex==0) / N, y = 0, pch = 16, cex = 5, col = 2)
legend(0.43, 2000, paste('', "A"), text.font = 3, bty = "n", cex= 2.5)

