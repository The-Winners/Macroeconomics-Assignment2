# Clear the variables
rm(list = ls())

# Set the working directory to source file location with
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
library(quantmod)
library(tseries)

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

# Create a Random walk time series
sim.num <- 100
# Use arima.sim() to generate a White Noise time series
GNP_sim <- arima.sim(n = sim.num, model = list(order = c(0, 0, 0)))
# Use cum sum to convert GNP_sim to a RW
GNP_sim_rw <- cumsum(GNP_sim)

# Create a Random walk time series with drift
GNP_sim_rw_drift <- arima.sim(
    n = sim.num,
    model = list(order = c(0, 0, 0)), mean = 0.9
)
# Use cum sum to convert GNP_sim to a RW
GNP_sim_rw_drift_rw <- cumsum(GNP_sim_rw_drift)


# create a dataframe with the data
data <- list(
    GNP, GNPC96, GNP_GDP_ratio, INDPRO, emp, unemp,
    GNP_defletor, cons_prices, wages, real_wages,
    Money_stock, Velocity, bond_yield, common_stock_prices,
    GNP_sim, GNP_sim_rw, GNP_sim_rw_drift, GNP_sim_rw_drift_rw
)

# sobstitute the NA values in data with 0
data <- data[!is.na(data)]


acf_list <- list(
    GNP, GNPC96, GNP_GDP_ratio, INDPRO, emp, unemp,
    GNP_defletor, cons_prices, wages, real_wages,
    Money_stock, Velocity, bond_yield, common_stock_prices,
    GNP_sim_rw_drift, GNP_sim_rw_drift_rw
)

# Apply ARMA model to the data
# create a cycle and apply ARMA to each variable
for (i in 1:length(data)) {
    arma_i <- arima(data[[i]], order = c(1, 0, 0), n = sim.num)
    # Compute autocorrelation function
    acf_list[[i]] <- acf(data[[i]], lag.max = 6)
}
