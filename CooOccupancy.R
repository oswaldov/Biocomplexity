##Load Packages

library(unmarked)
library(tidyverse)
library(lubridate)
library(ggplot2)

##Load Data

dat <- read.csv("~/cooperpafull.csv", header = TRUE, sep = ",")
head(dat,2)
dim(dat)

##Presen/absence data

y <- dat[,1:36]
head(y,2)
dim(y)

##Site covariates

SiteCov <- dat[c("site","distanthro","elev")]
head(SiteCov)
dim(SiteCov)

##Observation covariates

ObsCov <- list(precp = dat[,41:76],
               temp = dat[,77:112])

year <- matrix(c('01','02','03'), nrow(y), 3, byrow = TRUE)
year

##Frame data for umarked

umf <- unmarkedMultFrame(y = y, obsCovs = ObsCov, yearlySiteCovs = list(year = year), numPrimary = 3, siteCovs = SiteCov)
summary(umf)

##models
m0 <- colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data = umf) ##null model

m0


##bactransform results

backTransform(m0, type = "psi"); backTransform(m0, type = "col"); backTransform(m0, type = "ext"); 
backTransform(m0, type = "det")

confint(backTransform(m0, type = "psi")); confint(backTransform(m0, type = "col"));confint(backTransform(m0, type = "ext"));
confint(backTransform(m0, type = "det"))


m1 <- colext(psiformula = ~1, gammaformula = ~ year-1, epsilonformula = ~ year-1, pformula = ~ year-1, 
             data = umf)   ## effect of season on occupancy parameters: initial, colonization, extinction, detection

summary(m1)

##Models with different combinations of covariates

m2 <- colext(psiformula = ~1, gammaformula = ~ year-1 + distanthro, epsilonformula = ~year-1 + distanthro, pformula = ~year-1,
             data = umf)

summary(m2)

m3 <- colext(psiformula = ~1, gammaformula = ~year-1 + elev, epsilonformula = ~year-1 + elev, pformula = ~year-1,
             data = umf)

summary(m3)

m4 <- colext(psiformula = ~1, gammaformula = ~ year-1 + distanthro + elev, epsilonformula = ~ year-1 + distanthro + elev,
             pformula = ~ year-1,
             data = umf)

summary(m4)


m5 <- colext(psiformula = ~1, gammaformula = ~ year-1 + distanthro + elev, epsilonformula = ~ year-1 + distanthro + elev,
             pformula = ~ year-1 + precp,
             data = umf)

summary(m5)

m6 <- colext(psiformula = ~1, gammaformula = ~ year-1 + distanthro + elev, epsilonformula = ~ year-1 + distanthro + elev,
             pformula = ~ year-1 + temp,
             data = umf)

summary(m6)


m7 <- colext(psiformula = ~1, gammaformula = ~ year-1 + distanthro + elev, epsilonformula = ~ year-1 + distanthro + elev,
             pformula = ~ year-1 + temp + precp,
             data = umf)

summary(m7)

m8 <- colext(psiformula = ~1, gammaformula = ~ year-1 + distanthro + elev, epsilonformula = ~ year-1 + distanthro + elev,
             pformula = ~ year-1 +temp + precp + distanthro,
             data = umf)

summary(m8)

m9 <- colext(psiformula = ~1, gammaformula = ~ year-1 + distanthro + elev, epsilonformula = ~ year-1 + distanthro + elev,
             pformula = ~ year-1 + temp + precp + elev,
             data = umf)

summary(m9)

m10 <- colext(psiformula = ~1, gammaformula = ~ year-1 + distanthro + elev, epsilonformula = ~ year-1 + distanthro + elev,
              pformula = ~ year-1 + temp + precp + distanthro + elev,
              data = umf)

summary(m10)

## Model comparison

modelselec <- fitList('psi(.)gam(.)eps(.)p(.)' = m0, 'psi(.)gam(year)eps(year)p(year)' = m1, 'psi(.)gam(year+distanthro)eps(year+distanthro)p(year)' = m2,
                      'psi(.)gam(year+elev)eps(year+elev)p(year)' = m3, 'psi(.)gam(year+distanthro+elev)eps(year+distanthro+elev)p(year)' = m4,
                      'psi(.)gam(year+distanthro+elev)eps(year+distanthro+elev)p(year+precp)' = m5, 
                      'psi(.)gam(year+distanthro+elev)eps(year+distanthro+elev)p(year+temp)' = m6,
                      'psi(.)gam(year+distanthro+elev)eps(year+distanthro+elev)p(year+temp+precp)' = m7,
                      'psi(.)gam(year+distanthro+elev)eps(year+distanthro+elev)p(year+temp+precp+distanthro)' = m8,
                      'psi(.)gam(year+distanthro+elev)eps(year+distanthro+elev)p(year+temp+precp+elev)' = m9,
                      'psi(.)gam(year+distanthro+elev)eps(year+distanthro+elev)p(year+temp+precp+distanthro+elev)' = m10)

modelcomp <- modSel(modelselec)
modelcomp


## Plot of all the parameters: occupancy, ext, col, detection vs season 

psi <- plogis(2.17)
ci <- confint(backTransform(m1, type = "psi"))

ext1 <- plogis(-9.99)
ext2 <- plogis(-2.65)

col1 <- plogis(0.463)
col2 <- plogis(7.491)

det1 <- plogis(1.369)
det2 <- plogis(0.820)
det3 <- plogis(0.598)

nd <- data.frame(year=c('01','02'))
nd
E.ext <- predict(m1, type='ext', newdata=nd)
E.ext
E.col <- predict(m1, type='col', newdata=nd)
E.col

nd <- data.frame(year=c('01','02','03'))
E.det <- predict(m1, type='det', newdata=nd)
E.det

#Predict returns the predictions along with SE and CI.
ext1
ext2


op <- par(mfrow=c(4,1), mai=c(0.6, 0.6, 0.1, 0.1))

x <- c(1,1,1)
x
y <- c(0.653, 0.897, 0.976)
y
dat <- data.frame(x,y)
dat

l <- 1
m <- 0.897

ppar <- data.frame(l,m)
ppar

plot(dat, pch = "-", cex = 1.5, col = 4, type = "o", lwd = 1.5, ylim = c(0,1), xaxt = "n", xlab = "", ylab = "First year occupancy (ψ)")
points(ppar, type = "p", col=1, lwd = 1, pch=16)
legend("topright", c('Parameter', 'Estimate'), col=c(1,4), pch=c(16, 1),
       cex=1.4)


with(E.ext, { # Plot for extinction probability
  plot(1:2, Predicted, pch=1, xaxt='n', xlab='Year',
       ylab=expression(paste('Extinction probability ( ', epsilon, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:2, labels=nd$year[1:2])
  arrows(1:2, lower, 1:2, upper, code=3, angle=90, length=0.05, col=4, lty = 1, lwd = 1.5)
  points((1:2), c(ext1,ext2), col=1, lwd = 1, pch=16)
  #legend("topright", c('Parameter', 'Estimate'), col=c(1,4), pch=c(16, 1),
  #cex=1.4)
})

with(E.col, { # Plot for colonization probability
  plot(1:2, Predicted, pch=1, xaxt='n', xlab='Year',
       ylab=expression(paste('Colonization probability ( ', gamma, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:2, labels=nd$year[1:2])
  arrows(1:2, lower, 1:2, upper, code=3, angle=90, length=0.05, col=4, lty=1, lwd = 1.5)
  points((1:2), c(col1,col2), col=1, lwd = 1, pch=16)
  #legend("topright", c('Parameter', 'Estimate'), col=c(1,4), pch=c(16, 1),
  #cex=0.8)
})

with(E.det, { # Plot for detection probability: note 10 years
  plot(1:3, Predicted, pch=1, xaxt='n', xlab='Year',
       ylab=expression(paste('Detection probability ( ', p, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:3, labels=nd$year)
  arrows(1:3, lower, 1:3, upper, code=3, angle=90, length=0.05, col=4, lty = 1, lwd = 1.5)
  points((1:3),c(det1,det2,det3), col=1, lwd = 1, pch=16)
  #legend(7.5, 1, c('Parameter','Estimate'), col=c(1,4), pch=c(16, 1),
  #cex=0.8)
})
par(op)


### PREDICTIONS USING BEST MODEL BASED ON AIC

#Extinction


nd <- data.frame(year=c('01','01', '01','01','01','01','01','01','01','01','01','01', '01','01','01','01','01','01',
                        '01','01', '01','01','01','01','01'), 
                 distanthro=seq(0, 600, length=25), elev=seq(1000, 1200, length =25))
nd
E.psi <- predict(m8, type="ext", newdata=nd, appendData=TRUE)
E.psi




op <- par(mfrow=c(1,1), mai=c(0.8,0.8,0.1,0.1))
nd2 <- data.frame(year=c('02','02', '02','02','02','02','02','02','02','02','02','02', '02','02','02','02','02','02',
                         '02','02', '02','02','02','02','02'), 
                  distanthro=seq(0, 600, length=25), elev=seq(1000, 1200, length =25))
nd2
E.psi2 <- predict(m8, type="ext", newdata=nd2, appendData=TRUE)
E.psi2

op <- par(mfrow=c(2,1), mai=c(0.8,0.8,0.1,0.1))
plot(E.psi$distanthro, E.psi$Predicted, ylim=c(-0.05,0.1), type="l",
     xlab="Distance to Anthropogenic features (m) - year 01",
     ylab=expression(paste('Extinction probability ( ', epsilon, ' )')), cex.lab=0.8, cex.axis=0.8)
lines(E.psi$distanthro, E.psi$lower, col="red")
lines(E.psi$distanthro, E.psi$upper, col="red")
legend("topright", legend=c("Predicted", "Prediction intervals"), col=c("black","red"), lty = 1:1,
       cex=1)


plot(E.psi2$distanthro, E.psi2$Predicted, ylim=c(-0.05,0.1), type="l",
     xlab="Distance to Anthropogenic features (m) - year 02",
     ylab=expression(paste('Extinction probability ( ', epsilon, ' )')), cex.lab=0.8, cex.axis=0.8)
lines(E.psi$distanthro, E.psi2$lower, col="red")
lines(E.psi$distanthro, E.psi2$upper, col="red")
par(op)



# Colonization

ndc <- data.frame(year=c('01','01', '01','01','01','01','01','01','01','01','01','01', '01','01','01','01','01','01',
                         '01','01', '01','01','01','01','01'), 
                  distanthro=seq(0, 600, length=25), elev=seq(1000, 1200, length =25))
ndc
C.psi <- predict(m8, type="col", newdata=ndc, appendData=TRUE)
C.psi


##year 2
ndc2 <- data.frame(year=c('02','02', '02','02','02','02','02','02','02','02','02','02', '02','02','02','02','02','02',
                          '02','02', '02','02','02','02','02'), 
                   distanthro=seq(0, 600, length=25), elev=seq(1000, 1200, length =25))
ndc2
C.psi2 <- predict(m8, type="col", newdata=ndc2, appendData=TRUE)
C.psi2

op <- par(mfrow=c(2,1), mai=c(0.8,0.8,0.1,0.1))
plot(C.psi$distanthro, C.psi$Predicted, ylim=c(-0.05,1.1), type="l",
     xlab="Distance to Anthropogenic features (m) - year 01",
     ylab=expression(paste('Colonization probability ( ', gamma, ' )')), cex.lab=0.8, cex.axis=0.8)
lines(C.psi$distanthro, C.psi$lower, col="red")
lines(C.psi$distanthro, C.psi$upper, col="red")
legend("topright", legend=c("Predicted", "Prediction intervals"), col=c("black","red"), lty = 1:1,
       cex=1)


plot(C.psi2$distanthro, C.psi2$Predicted-0.02, ylim=c(-0.05,1.1), type="l",
     xlab="Distance to Anthropogenic features (m) - year 02",
     ylab=expression(paste('Colonization probability ( ', gamma, ' )')), cex.lab=0.8, cex.axis=0.8)
lines(C.psi2$distanthro, C.psi2$lower, col="red")
lines(C.psi2$distanthro, C.psi2$upper, col="red")
par(op)


## Detection

ndd <- data.frame(year=c('01','01', '01','01','01','01','01','01','01','01','01','01', '01','01','01','01','01','01',
                         '01','01', '01','01','01','01','01'), temp=seq(14, 20, length = 25),
                  precp=seq(20,700, length=25), distanthro=seq(0, 600, length=25))
ndd
D.psi <- predict(m8, type="det", newdata=ndd, appendData=TRUE)
D.psi


##year 2
ndd2 <- data.frame(year=c('02','02', '02','02','02','02','02','02','02','02','02','02', '02','02','02','02','02','02',
                          '02','02', '02','02','02','02','02'), temp=seq(14, 20, length = 25),
                   precp=seq(20,700, length=25), distanthro=seq(0, 600, length=25))
ndd2
D.psi2 <- predict(m8, type="det", newdata=ndd2, appendData=TRUE)
D.psi2

##year 3
ndd3 <- data.frame(year=c('03','03', '03','03','03','03','03','03','03','03','03','03', '03','03','03','03','03','03',
                          '03','03', '03','03','03','03','03'), temp=seq(14, 20, length = 25),
                   precp=seq(20,700, length=25), distanthro=seq(0, 600, length=25))
ndd3
D.psi3 <- predict(m8, type="det", newdata=ndd3, appendData=TRUE)
D.psi3



op <- par(mfrow=c(3,1), mai=c(0.8,0.8,0.1,0.1))
plot(D.psi$temp, D.psi$Predicted, ylim=c(-0.05,1.1), type="l",
     xlab="Temperature (°C) - year 01",
     ylab=expression(paste('Detection probability ( ', p, ' )')), cex.lab=0.8, cex.axis=0.8)
lines(D.psi$temp, D.psi$lower, col="red")
lines(D.psi$temp, D.psi$upper, col="red")
legend("topright", legend=c("Predicted", "Prediction intervals"), col=c("black","red"), lty = 1:1,
       cex=1)


plot(D.psi2$temp, D.psi2$Predicted, ylim=c(-0.05,1.1), type="l",
     xlab="Temperature (°C) - year 02",
     ylab=expression(paste('Detection probability ( ', p, ' )')), cex.lab=0.8, cex.axis=0.8)
lines(D.psi2$temp, D.psi2$lower, col="red")
lines(D.psi2$temp, D.psi2$upper, col="red")



plot(D.psi3$temp, D.psi3$Predicted, ylim=c(-0.05,1.1), type="l",
     xlab="Temperature (°C) - year 03",
     ylab=expression(paste('Detection probability ( ', p, ' )')), cex.lab=0.8, cex.axis=0.8)
lines(D.psi3$temp, D.psi3$lower, col="red")
lines(D.psi3$temp, D.psi3$upper, col="red")
par(op)





