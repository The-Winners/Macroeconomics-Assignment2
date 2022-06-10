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
  acf(ts, main=main)
  pacf(ts, main=main)
}


# INTERPRETATION 1
#Simulate an AR(1) with a positive parameter

ar=c(0.3)
ma=c(0)
order=c(1,0,0)


arts1=arma(ar,ma)
artsfit1 = armafit(arts1, order)
stargazer(artsfit1, type="text")

plotcorr(arts1,main="AR(1)pos.a")

#Simulate an AR(1) with a positive parameter

ar=c(0.5)
ma=c(0)
order=c(1,0,0)


arts2=arma(ar,ma)
artsfit2 = armafit(arts2, order)
stargazer(artsfit2, type="text")

plotcorr(arts2,main="AR(1)pos.b")

#Simulate an AR(1) with a negative parameter

ar=c(-0.3)
ma=c(0)
order=c(1,0,0)


arts3=arma(ar,ma)
artsfit3 = armafit(arts3, order)
stargazer(artsfit3, type="text")

plotcorr(arts3,main="AR(1)neg.a")

#Simulate an AR(1) with a negative parameter

ar=c(-0.5)
ma=c(0)
order=c(1,0,0)


arts4=arma(ar,ma)
artsfit4 = armafit(arts4, order)
stargazer(artsfit4, type="text")

plotcorr(arts4,main="AR(1)neg.b")


#Simulate an MA(1) with a positive parameter

ar=c(0)
ma=c(0.2)
order=c(0,0,1)


mats1=arma(ar,ma)
matsfit1 = armafit(mats1, order)
stargazer(matsfit1, type="text")

plotcorr(mats1,main="MA(1)pos.a")


#Simulate an MA(1) with a positive parameter

ar=c(0)
ma=c(0.3)
order=c(0,0,1)


mats2=arma(ar,ma)
matsfit2 = armafit(mats2, order)
stargazer(matsfit2, type="text")

plotcorr(mats2,main="MA(1)pos.b")


#Simulate an MA(1) with a negative parameter

ar=c(0)
ma=c(-0.2)
order=c(0,0,1)


mats3=arma(ar,ma)
matsfit3 = armafit(mats3, order)
stargazer(matsfit3, type="text")

plotcorr(mats3,main="MA(1)neg.a")



#Simulate an MA(1) with a negative parameter

ar=c(0)
ma=c(-0.3)
order=c(0,0,1)


mats4=arma(ar,ma)
matsfit4 = armafit(mats4, order)
stargazer(matsfit4, type="text")

plotcorr(mats4,main="MA(1)neg.b")



# Simulate an ARMA(2,2) with certain parameters

ar=c(0.5,-0.3)
ma=c(0.2,-0.2)
order=c(2,0,2)


armats1=arma(ar,ma)
armatsfit1 = armafit(armats1, order)
stargazer(armatsfit1, type="text")

plotcorr(armats1,main="ARMA(2,2)")


# Simulate an ARMA(2,2) with certain parameters

ar=c(0.3,-0.5)
ma=c(-0.2, 0.2)
order=c(2,0,2)


armats2=arma(ar,ma)
armatsfit2 = armafit(armats2, order)
stargazer(armatsfit2, type="text")

plotcorr(armats2,main="ARMA(2,2)")


# INTERPRETATION 2

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







##################
##### POINT 2 ####
##################

