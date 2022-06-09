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
              "stargazer", "car", "forecast", "tseries", "quantmod", "eurostat")
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

# generate a random series
y <- rnorm(100)

# compute AR(i) of y
ar.i <- ar(y, i)

# prediction of AR(i)
pred.ar.i <- ts(predict(ar.i, h = 12))

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

