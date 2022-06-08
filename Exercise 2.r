#Exercise 2
#Author: The Winners
#Date: 10/10/2017
#library
library(forecast)
library(tseries)
#1 - Data generating process
#Simulate an AR(i), a MA(i) and an ARMA(i,j) study the correlogram, where you must choose i and j as
#positive integers, while the parameters can be positive or negative
#sampling for value for i and j
i <- sample(seq(1,10), 4, replace = F)
j <- sample(seq(1,10), 4, replace = F)
#generate the data
#get a random univariate time series of length 100 (positive values)
y <- ts(c(1:100))
plot(y)

#compute AR(i) of y
ar.i <- ar(y, i)
#compute MA(i) of y
ma.i <- ma(y,i)
#compute ARMA(i,j) of y
arma.ij <- arma(y,i,j)
#Plot the correlogram of the three models
#plot the AR(i)
plot(ar.i)
#plot the MA(i)
plot(ma.i)
#plot the ARMA(i,j)
plot(arma.ij)
#3 - Compute the autocorrelation function of the three models
#compute the autocorrelation function of the AR(i)
acf.ar.i <- acf(ar.i)
#compute the autocorrelation function of the MA(i)
acf.ma.i <- acf(ma.i)
#compute the autocorrelation function of the ARMA(i,j)
acf.arma.ij <- acf(arma.ij)
#4 - Plot the autocorrelation function of the three models
#plot the autocorrelation function of the AR(i)
plot(acf.ar.i)
#plot the autocorrelation function of the MA(i)
plot(acf.ma.i)
#plot the autocorrelation function of the ARMA(i,j)
plot(acf.arma.ij)


