##Load Packages

library(unmarked)
library(tidyverse)
library(lubridate)
library(ggplot2)

##Load Data

dat <- read.csv("C:\\Users\\oswal\\OneDrive\\Documentos\\RCUH\\Occupancy\\cooperpafull.csv", header = TRUE, sep = ",")
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
m0 <- colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data = umf)

m0

names(m0)

##bactransform results

backTransform(m0, type = "psi"); backTransform(m0, type = "col"); backTransform(m0, type = "ext"); 
backTransform(m0, type = "det")

confint(backTransform(m0, type = "psi")); confint(backTransform(m0, type = "col"));confint(backTransform(m0, type = "ext"));
confint(backTransform(m0, type = "det"))