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

# ----

# 1 - Data generating process
# Simulate an AR(i), a MA(i) and an ARMA(i,j) study the correlogram,
# where you must choose i and j as
# positive integers, while the parameters can be positive or negative
# sampling for value for i and j

set.seed(1111)


# Define a function to generate a general ARMA time series of order i,j
# so that it could be also an AR or an MA

arma=function(ar,ma){
  armats <- arima.sim(model=list(ar=ar,ma=ma), n=1000)
  armats
  return(armats)
}

armafit=function(armats,order){
  armatsfit = arima(armats, order, include.mean = FALSE)
  return(armatsfit)
}

plotcorr=function(ts,main){
  plot(ts, type="l", main=main)
  acf(ts, main=main)
  pacf(ts, main=main)
}



# INTERPRETATION 1 ----

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

# Simulate an ARMA(1,1) with certain parameters
ar=c(0.999999)
ma=c(0.5)
order=c(1,0,1)
armats1=arma(ar,ma)
armatsfit1 = armafit(armats1, order)
stargazer(armatsfit1, type="text")
plotcorr(armats1,main="ARMA(1,1)")

# Simulate an ARMA(2,2) with certain parameters
ar=c(0.3,-0.5)
ma=c(-0.2, 0.2)
order=c(2,0,2)
armats2=arma(ar,ma)
armatsfit2 = armafit(armats2, order)
stargazer(armatsfit2, type="text")
plotcorr(armats2,main="ARMA(2,2)")



# INTERPRETATION 2 ----

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

# ----

# Simulating an arma(2,1) as a baseline time series
ar=c(0.43,0.37)
ma=c(0.6)
order=c(2,0,1)
armatsnew=arma(ar,ma)
acf(armatsnew)


#Divide the time series in 2 parts
n=length(armatsnew)/2
armatstrain=armatsnew[1:n]
armatstest=armatsnew[(n+1):length(armatsnew)]


#Model fitting with an AR(1), ARMA(2,1), naive forecast, mean forecast
order=c(1,0,0)
arfit=armafit(armatstrain,order)
plot(arfit$coef)
summary(arfit)
arfitpred=predict(arfit,n.ahead=500, interval="confidence")
maear=mae(armatstest,arfitpred$pred)
rmsear=rmse(armatstest,arfitpred$pred)
cbind(maear,rmsear)



#Do it for ARMA(2,1)
order=c(2,0,1)
armafitting=armafit(armatstrain,order)
plot(armafitting$coef)
summary(armafitting)
armafitpred=predict(armafitting,n.ahead=500, interval="confidence")
maearma=mae(armatstest,armafitpred$pred)
rmsearma=rmse(armatstest,armafitpred$pred)
cbind(maearma,rmsearma)

#Do it for a naive forecast
naivefit=naive(armatstest, h=500)
plot(naivefit)
naivefitpred=predict(naivefit,n.ahead=500, interval="confidence")
maenaive=mae(armatstest,naivefitpred$fitted[-c(1)])
rmsenaive=rmse(armatstest,naivefitpred$fitted[-c(1)])
cbind(maenaive,rmsenaive)

#Do it for a mean forecast
meanfit=lm(armatstrain ~ 1)
plot(c(armatstrain,meanfit$fitted.values))
meanfitpred=predict(meanfit,n.ahead=500, interval="confidence")
maemean=mae(armatstest,meanfit[["coefficients"]])
rmsemean=rmse(armatstest,meanfit[["coefficients"]])
cbind(maemean,rmsemean)















##################
##### POINT 3 ####
##################

# ----


##################
##### POINT 4 ####
##################

# ----


# Download treasury rates from Fred
rates <- c("DTB3","DGS2","DGS10")
for (i in 1:length(rates)) {
  getSymbols(rates[i], src = "FRED")
}
plot(DTB3)

#Clean the time series from days of market closure
nas=c(which(is.na(DTB3)))
for (i in 1:length(nas)){
  DTB3[nas[i]]=DTB3[nas[i]-1]
}

nas2=c(which(is.na(DGS2)))
for (i in 1:length(nas2)){
  DGS2[nas2[i]]=DGS2[nas2[i]-1]
}

nas3=c(which(is.na(DGS10)))
for (i in 1:length(nas3)){
  DGS10[nas3[i]]=DGS10[nas3[i]-1]
}





#Construct term spread

termspread=DGS10-DTB3
plot(termspread)

#Construct the indicator for an advent the recession
indicator=DGS10-DGS2
plot(indicator)


plotcorr(DTB3, main="DTB3")
plotcorr(DGS2, main="DGS2")
plotcorr(DGS10, main="DGS10")

#Fit the models using an ARMA(1,1)
ordercomb=NULL
ordercomb=matrix(NA,nrow=9,ncol=3)
for (z in 1:3){
  for (x in 1:3){
    ordercomb[x+3*(z-1),]=c(x,0,z)
  }
}

ordercomb


#Check which one is the best model specification for DTB3

dtb3train=DTB3[1:(length(DTB3)*0.8)]
dtb3test=DTB3[((length(DTB3)*0.8)+1):length(DTB3)]

order=ordercomb[1,]
fit=armafit(dtb3train,order)
fitpred=predict(fit,n.ahead=length(dtb3test), interval="confidence")
mae=mae(dtb3test,fitpred$pred)
rmse=rmse(dtb3test,fitpred$pred)


maelist=c()
rmselist=c()
aiclist=c()
biclist=c()
for (c in 1:nrow(ordercomb)){
  order=ordercomb[c,]
  fit=armafit(dtb3train,order)
  fitpred=predict(fit,n.ahead=length(dtb3test), interval="confidence")
  maelist[c]=mae(dtb3test,fitpred$pred)
  rmselist[c]=rmse(dtb3test,fitpred$pred)
  aiclist[c]=-1*AIC(fit)
  biclist[c]=-1*BIC(fit)
}

errorlist=cbind(maelist, rmselist, aiclist, biclist)
which.min(errorlist[,1])
which.min(errorlist[,2])
which.min(errorlist[,3])
which.min(errorlist[,4])


plot(maelist, type= "h")
plot(rmselist, type="h")
plot(aiclist, type= "h")
plot(biclist, type="h")


order=c(3,0,1)
dtb3fit=armafit(DTB3, order)
stargazer(dtb3fit, type="text")

#Same Procedure for DGS2
dgs2train=DGS2[1:(length(DGS2)*0.8)]
dgs2test=DGS2[((length(DGS2)*0.8)+1):length(DGS2)]

order=ordercomb[1,]
fit=armafit(dgs2train,order)
fitpred=predict(fit,n.ahead=length(dgs2test), interval="confidence")
mae=mae(dgs2test,fitpred$pred)
rmse=rmse(dgs2test,fitpred$pred)


maelist=c()
rmselist=c()
aiclist=c()
biclist=c()
for (c in 1:nrow(ordercomb)){
  order=ordercomb[c,]
  fit=armafit(dgs2train,order)
  fitpred=predict(fit,n.ahead=length(dgs2test), interval="confidence")
  maelist[c]=mae(dgs2test,fitpred$pred)
  rmselist[c]=rmse(dgs2test,fitpred$pred)
  aiclist[c]=-1*AIC(fit)
  biclist[c]=-1*BIC(fit)
}

errorlist=cbind(maelist, rmselist, aiclist, biclist)
which.min(errorlist[,1])
which.min(errorlist[,2])
which.min(errorlist[,3])
which.min(errorlist[,4])


plot(maelist, type= "h")
plot(rmselist, type="h")
plot(aiclist, type= "h")
plot(biclist, type="h")


order=c(1,0,1)
dgs2fit=armafit(DGS2, order)
stargazer(dgs2fit, type="text")




#Same Procedure for DGS2
dgs2train=DGS2[1:(length(DGS2)*0.8)]
dgs2test=DGS2[((length(DGS2)*0.8)+1):length(DGS2)]

order=ordercomb[1,]
fit=armafit(dgs2train,order)
fitpred=predict(fit,n.ahead=length(dgs2test), interval="confidence")
mae=mae(dgs2test,fitpred$pred)
rmse=rmse(dgs2test,fitpred$pred)


maelist=c()
rmselist=c()
aiclist=c()
biclist=c()
for (c in 1:nrow(ordercomb)){
  order=ordercomb[c,]
  fit=armafit(dgs2train,order)
  fitpred=predict(fit,n.ahead=length(dgs2test), interval="confidence")
  maelist[c]=mae(dgs2test,fitpred$pred)
  rmselist[c]=rmse(dgs2test,fitpred$pred)
  aiclist[c]=-1*AIC(fit)
  biclist[c]=-1*BIC(fit)
}

errorlist=cbind(maelist, rmselist, aiclist, biclist)
which.min(errorlist[,1])
which.min(errorlist[,2])
which.min(errorlist[,3])
which.min(errorlist[,4])


plot(maelist, type= "h")
plot(rmselist, type="h")
plot(aiclist, type= "h")
plot(biclist, type="h")


order=c(1,0,1)
dgs2fit=armafit(DGS2, order)
stargazer(dgs2fit, type="text")




#Same Procedure for DGS2
dgs2train=DGS2[1:(length(DGS2)*0.8)]
dgs2test=DGS2[((length(DGS2)*0.8)+1):length(DGS2)]

order=ordercomb[1,]
fit=armafit(dgs2train,order)
fitpred=predict(fit,n.ahead=length(dgs2test), interval="confidence")
mae=mae(dgs2test,fitpred$pred)
rmse=rmse(dgs2test,fitpred$pred)


maelist=c()
rmselist=c()
aiclist=c()
biclist=c()
for (c in 1:nrow(ordercomb)){
  order=ordercomb[c,]
  fit=armafit(dgs2train,order)
  fitpred=predict(fit,n.ahead=length(dgs2test), interval="confidence")
  maelist[c]=mae(dgs2test,fitpred$pred)
  rmselist[c]=rmse(dgs2test,fitpred$pred)
  aiclist[c]=-1*AIC(fit)
  biclist[c]=-1*BIC(fit)
}

errorlist=cbind(maelist, rmselist, aiclist, biclist)
which.min(errorlist[,1])
which.min(errorlist[,2])
which.min(errorlist[,3])
which.min(errorlist[,4])


plot(maelist, type= "h")
plot(rmselist, type="h")
plot(aiclist, type= "h")
plot(biclist, type="h")


order=c(1,0,1)
dgs2fit=armafit(DGS2, order)
stargazer(dgs2fit, type="text")

#Same Procedure for DGS10
dgs10train=DGS10[1:(length(DGS10)*0.8)]
dgs10test=DGS10[((length(DGS10)*0.8)+1):length(DGS10)]

order=ordercomb[1,]
fit=armafit(dgs10train,order)
fitpred=predict(fit,n.ahead=length(dgs10test), interval="confidence")
mae=mae(dgs10test,fitpred$pred)
rmse=rmse(dgs10test,fitpred$pred)


maelist=c()
rmselist=c()
aiclist=c()
biclist=c()
for (c in 1:(nrow(ordercomb)-1)){
  order=ordercomb[c,]
  fit=armafit(dgs10train,order)
  fitpred=predict(fit,n.ahead=length(dgs10test), interval="confidence")
  maelist[c]=mae(dgs10test,fitpred$pred)
  rmselist[c]=rmse(dgs10test,fitpred$pred)
  aiclist[c]=-1*AIC(fit)
  biclist[c]=-1*BIC(fit)
}

errorlist=cbind(maelist, rmselist, aiclist, biclist)
which.min(errorlist[,1])
which.min(errorlist[,2])
which.min(errorlist[,3])
which.min(errorlist[,4])

ordercomb

plot(maelist, type= "h")
plot(rmselist, type="h")
plot(aiclist, type= "h")
plot(biclist, type="h")


order=c(2,0,1)
dgs10fit=armafit(DGS10, order)
stargazer(dgs10fit, type="text")


#After having chosen the three models we are going to use, we perform
#the Dickey-Fueller test

adf.test(DTB3)
adf.test(DGS2)
adf.test(DGS10)

#We conclude that all three time series are stationary



