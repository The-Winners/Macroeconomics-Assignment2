# Clear the variables
rm(list = ls())

# Set the working directory to source file location with
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
library(quantmod)
library(tseries)
library(prediction)
# Install packages
packages <- c(
    "tidyverse", "rsdmx", "eurostat", "tbl2xts",
    "tidyquant", "BCDating", "pwt10", "dplyr", "tseries", "prediction"
)
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))


# Get Data from Fred website
nipa <- c(
    "GNP", "GNPC96", "GNPGDPUSA156NUPN",
    "INDPRO", "CE16OV", "UNRATE", "A001RI1Q225SBEA",
    "FPCPITOTLZGUSA", "CES0500000003", "M2REAL", "M2V",
    "DAAA", "M1125AUSM343NNBR"
)
for (i in 1:length(nipa)) {
    getSymbols(nipa[i], src = "FRED")
}


# Take the log of the data
GNP <- log(GNP)
GNPC96 <- log(GNPC96) # Real GNP at cosntant price level 1996
GNP_GDP_ratio <- log(GNPGDPUSA156NUPN)
INDPRO <- log(INDPRO) # industrial production
emp <- log(CE16OV) # Employment in industry
unemp <- log(UNRATE) # Unemployment rate
GNP_defletor <- log(A001RI1Q225SBEA) # GNP deflator
cons_prices <- log(FPCPITOTLZGUSA) # Consumer prices
wages <- log(CES0500000003) # avg hourly wage private sector
real_wages <- log((CES0500000003 / A001RI1Q225SBEA) * 100) # real wages
Money_stock <- log(M2REAL) # Money stock
Velocity <- log(M2V) # Velocity of money
bond_yield <- DAAA # Bond yield
common_stock_prices <- log(M1125AUSM343NNBR) # Common stock prices

generateRandomWalk <- function(sim.num){
    # Create a Random walk time series
    # Create a Random walk time series with drift
    GNP_sim_rw_drift <- arima.sim(
        n = sim.num,
        model = list(order = c(0, 0, 0)), mean = 0.9
    )
    # Use cum sum to convert GNP_sim to a RW
    GNP_sim_rw_drift_rw <- cumsum(GNP_sim_rw_drift)
    return(GNP_sim_rw_drift_rw)
}
GNP_sim_rw_drift_rw <- generateRandomWalk(1000)
# create a dataframe with the data
data <- list(
    GNP, GNPC96, GNP_GDP_ratio, INDPRO, emp, unemp,
    GNP_defletor, cons_prices, wages, real_wages,
    Money_stock, Velocity, bond_yield, common_stock_prices,
    GNP_sim_rw_drift_rw
)

# sobstitute the NA values in each elements of th data with 0
for (i in 1:length(data)) {
    for (j in 1:length(data[[i]])) {
        if (!is.finite(data[[i]][j])) {
            data[[i]][j] <- 0
        }
    }
}
#function that compute the standard deviation of each element in the data
stdError <- function(data,i){
    return(sqrt(var(data[[i]])))
}
generateTable <- function(data){

    acf_list <- matrix(NA, nrow = length(data), ncol = 9)
    # compute the autocorrelation function of the data using acf()

    # assigned names to the rows of the matrix using the names of the elements in the vector
    label <- c(
        "GNP", "Real GNP", "GNP GDP ratio", "Industrial Production", "Employment",
        "Unemployment rate", "GNP deflator", "Consumer prices", "Wages", "Real wages",
        "Money stock", "Velocity", "Bond yield", "Common stock prices",
        "Random walk with drift"
    )
    # column names
    col.label <- c("Period", "T", "r1", "r2", "r3", "r4", "r5", "r6","sd")
    for (i in 1:length(data)) {
        acf_list[i, 1] <- paste(min(index(data[[i]])), max(index(data[[i]])), sep = " <-> ") # period
        acf_list[i, 2] <- length(data[[i]]) # sample size
        acf_list[i, 3:(ncol(acf_list)-1)] <- round(acf(data[[i]], lag.max = 6, plot = FALSE)$acf[-1], 3) # autocorrelation function
        acf_list[i, ncol(acf_list)] <- round(stdError(data, i), 3) # standard error
    }
    colnames(acf_list) <- col.label
    rownames(acf_list) <- label
    return(acf_list)
}


#table 1: sample autocorrelation of the data
acf_autcorr <- generateTable(data)

#table 2: autocorrelation of the first difference of the data
#take the first difference of each element of the data
#list of the first differences with the same length as the original data

#function the compute the first difference of the data
firstDiff <- function(data){
    first_diff <- list()
    for (i in 1:(length(data))) {
        first_diff[[i]] <- diff(data[[i]], lag=1)
        # replace the NA values in each element of the data with 0
        for (j in 1:length(data[[i]])) {
            if (!is.finite(data[[i]][j])) {
                first_diff[[i]][j] <- 0
            }
        }
    }
    # replace the first element in each element of the first_diff with 0
    for (i in 1:length(first_diff)) {
        first_diff[[i]][1] <- 0
    }
    return(first_diff)
}
#recall the function to compute the first difference of the data
first_diff <- firstDiff(data)
View(first_diff)

#recall the function to compute the autocorrelation of the first difference of the data
acf_autcorr_diff <- generateTable(first_diff)
View(acf_autcorr_diff)

#table 3: Sample autocorrelation of the deviation of the data
#compute the residuals of the data from the prediction of the fitted model
residuals <- function(data,predicted){
    resid <- list()
    for (i in 1:(length(data))) {
        resid[[i]] <- (data[[i]] - predicted[i])^2
    }
    return(resid)
}
#compute a arima model for each element of the data
#list of the fitted models
fitted_models <- list()
#fit the arima(1,1,1) model to each element of the data
for (i in 1:length(data)) {
    fitted_models[[i]] <- arima(data[[i]], order = c(1, 1, 1))
}

#compute the residuals of the data from the fitted models
residuals <- residuals(data, fitted_models)
#compute the sample autocorrelation of the residuals
acf_autcorr_resid <- generateTable(residuals)
View(acf_autcorr_resid)

theUltimateFunction <- fucntion(first_diff){
    #given the first difference of the data,
    #select the best ARIMA model for each element of the data with AIC
    #detrend each element of data with the fitted model of the best ARIMA model
    #compute the residuals of the data from the fitted model
    #compute the sample autocorrelation of the residuals
    #return the sample autocorrelation of the residuals for each element of the data

    #define a matrix containing the iper-parameters for the best ARIMA model
    iper_param <- matrix(NA, nrow = length(first_diff), ncol = 3) 
    #define the vector containing the all possible iper-parameters
    p <- c(0:50)
    d <- c(0:50)
    q <- c(0:50)
    #fit the ARIMA model to each element of the data
    for (i in 1:length(first_diff)) {
        fitted_models[[i]] <- arima(first_diff[[i]], order = c(p[i], d[i], q[i]))
    }


}



