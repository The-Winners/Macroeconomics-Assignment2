# Exercise 2
# Author: The Winners
# Date: 10/10/2017
# library


# PRELIMINARY OPERATIONS


# Clear the variables
rm(list = ls())

# Set the working directory to source file location with
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install packages
packages <- c("tidyverse", "rsdmx", "eurostat", "tbl2xts", 
              "tidyquant", "BCDating", "pwt10", "dplyr",
              "stargazer", "car", "forecast", "tseries", 
              "quantmod", "eurostat", "stargazer",
              "skedastic","Metrics","mFilter", "aTSA","lmtest","xts")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))

# Load packages
library(quantmod)
library(eurostat)

##################
##### POINT 1 ####
##################


# 1 - Data generating process

# Simulate an AR(i), a MA(i) and an ARMA(i,j) study the correlogram,
# where you must choose i and j as
# positive integers, while the parameters can be positive or negative
# sampling for value for i and j
i <- 1
j <- 2

set.seed(1111)

# Define a function to generate a general ARMA time series of order i,j
# so that it could be also an AR or an MA

arma=function(ar,ma){
  armats <- arima.sim(model=list(ar=ar,ma=ma), n=1000)
  armats
  return(armats)
}

armafit=function(armats,order){
  armatsfit = arima(armats, order)
  return(armatsfit)
}

plotcorr=function(ts,main){
  plot(ts, type="l", main=main)
  pacf(ts, main=main)
}

#Simulate an AR(1) with certain parameters

ar=c(0.3)
ma=c(0)
order=c(1,0,0)


arts1=arma(ar,ma)
artsfit1 = armafit(arts1, order)
stargazer(artsfit1, type="text")

plotcorr(arts1,main="AR(1)")

#Simulate an AR(1) with certain parameters

ar=c(0.5)
ma=c(0)
order=c(1,0,0)


arts1=arma(ar,ma)
artsfit1 = armafit(arts1, order)
stargazer(artsfit1, type="text")

plotcorr(arts1,main="AR(1)")

#Simulate an AR(1) with certain parameters

ar=c(-0.3)
ma=c(0)
order=c(1,0,0)


arts1=arma(ar,ma)
artsfit1 = armafit(arts1, order)
stargazer(artsfit1, type="text")

plotcorr(arts1,main="AR(1)")

#Simulate an AR(1) with certain parameters

ar=c(-0.5)
ma=c(0)
order=c(1,0,0)


arts1=arma(ar,ma)
artsfit1 = armafit(arts1, order)
stargazer(artsfit1, type="text")

plotcorr(arts1,main="AR(1)")


#Simulate an AR(4) with certain parameters

ar=c(0.3,0.25,-0.22,-0.15)
ma=c(0)
order=c(4,0,0)


arts1=arma(ar,ma)
artsfit1 = armafit(arts1, order)
stargazer(artsfit1, type="text")

plotcorr(arts1,main="AR(4)")

# Simulate an AR(6) with certain parameters

ar=c(0.35,0.33,0.3,-0.18, -0.15, -0.1)
ma=c(0)
order=c(6,0,0)


arts2=arma(ar,ma)
artsfit2 = armafit(arts2, order)
stargazer(artsfit2, type="text")

plotcorr(arts2,main="AR(6)")


# Simulate an MA(4) with certain parameters

ar=c(0)
ma=c(0.2,0.2,-0.2,-0.2)
order=c(0,0,4)


mats1=arma(ar,ma)
matsfit1 = armafit(mats1, order)
stargazer(matsfit1, type="text")

plotcorr(mats1,main="MA(4)")

# Simulate an MA(6) with certain parameters

ar=c(0)
ma=c(0.2,0.2,0.2,-0.2,-0.2,-0.2)
order=c(0,0,6)


mats2=arma(ar,ma)
matsfit2 = armafit(mats2, order)
stargazer(matsfit2, type="text")

plotcorr(mats2,main="MA(6)")

# Simulate an ARMA(3,1) with certain parameters

ar=c(0.2,0.2,-0.2)
ma=c(-0.2)
order=c(3,0,1)


armats1=arma(ar,ma)
armatsfit1 = armafit(armats1, order)
stargazer(armatsfit1, type="text")

plotcorr(armats1,main="ARMA(3,1)")

# Simulate an ARMA(1,3) with certain parameters

ar=c(0.1)
ma=c(0.1,-0.1,-0.1)
order=c(1,0,3)


armats2=arma(ar,ma)
armatsfit2 = armafit(armats2, order)
stargazer(armatsfit2, type="text")

plotcorr(armats2,main="ARMA(1,3)")

# plot the prediction of AR(i) and the original data
plot(y, type = "l", col = "blue", lwd = 2, xlab = "Time", ylab = "Log PCEC")
lines(pred.ar.i, type = "l", col = "red", lwd = 2, lty = 2)








# compute MA(i) of y
ma.i <- ma(y, i)

# compute ARMA(i,j) of y
arma.ij <- arma(x = y, order = c(i, j))


# Plot the correlogram of the three models

# plot the AR(i)
plot(ar.i)

# plot the MA(i)
plot(ma.i)

# plot the ARMA(i,j)
plot(arma.ij)

# 3 - Compute the autocorrelation function of the three models
# compute the autocorrelation function of the AR(i)
acf.ar.i <- acf(ar.i)
# compute the autocorrelation function of the MA(i)
acf.ma.i <- acf(ma.i)
# compute the autocorrelation function of the ARMA(i,j)
acf.arma.ij <- acf(arma.ij)

# 4 - Plot the autocorrelation function of the three models
# plot the autocorrelation function of the AR(i)
plot(acf.ar.i)
# plot the autocorrelation function of the MA(i)
plot(acf.ma.i)
# plot the autocorrelation function of the ARMA(i,j)
plot(acf.arma.ij)






##################
##### POINT 2 ####
##################

