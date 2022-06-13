# Clear the variables
rm(list = ls())

# Set the working directory to source file location with
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
library(quantmod)
library(tseries)
library(prediction)
library(forecast)


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

generateRandomWalk <- function(sim.num) {
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
# function that compute the standard deviation of each element in the data
stdError <- function(data, i) {
    return(sqrt(var(data[[i]])))
}
generateTable <- function(data) {
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
    col.label <- c("Period", "T", "r1", "r2", "r3", "r4", "r5", "r6", "sd")
    for (i in 1:length(data)) {
        acf_list[i, 1] <- paste(min(index(data[[i]])), max(index(data[[i]])), sep = " <-> ") # period
        acf_list[i, 2] <- length(data[[i]]) # sample size
        acf_list[i, 3:(ncol(acf_list) - 1)] <- round(acf(data[[i]], lag.max = 6, plot = FALSE)$acf[-1], 3) # autocorrelation function
        acf_list[i, ncol(acf_list)] <- round(stdError(data, i), 3) # standard error
    }
    colnames(acf_list) <- col.label
    rownames(acf_list) <- label
    return(acf_list)
}


# table 1: Sample autocorrelation of the data
acf_autcorr <- generateTable(data)
View(acf_autcorr)

# Compute the autocorrelation of the first difference of the data
# take the first difference of each element of the data
# list of the first differences with the same length as the original data

# function the compute the first difference of the data
firstDiff <- function(data) {
    first_diff <- list()
    for (i in 1:(length(data))) {
        first_diff[[i]] <- diff(data[[i]], lag = 1)
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
# recall the function to compute the first difference of the data
first_diff <- firstDiff(data)

# Table2:  the autocorrelation of the first difference of the data
acf_autcorr_diff <- generateTable(first_diff)
View(acf_autcorr_diff)





# define a function AIC_score to compute the AIC of the fitted model
AIC_score <- function(resid) {
    # compute the AIC of the fitted model
    AIC_score <- log(length(resid)) + 2 * length(resid)
    return(AIC_score)
}

theUltimateFunction <- function(first_diff) {

    # given the first difference of the data,
    # select the best ARIMA model for each element of the data with AIC
    # detrend each element of data with the fitted model of the best ARIMA model
    # compute the residuals of the data from the fitted model
    # compute the sample autocorrelation of the residuals
    # return the sample autocorrelation of the residuals
    # for each element of the data

    # define a matrix containing the iper-parameters
    # for the best ARIMA model (with min AIC)
    iper_param <- matrix(NA, nrow = length(first_diff), ncol = 3)

    # define the vector containing the all possible iper-parameters
    p <- c(0:3)
    d <- c(0:3)
    q <- c(0:3)

    # create a matrix containing all possible combinations
    # of the iper-parameters
    iper_param_matrix <- matrix(NA,
        nrow = (length(p) * length(d) * length(q)),
        ncol = 3
    )

    # create a vector containing the sample autocorrelation of
    # the residuals for each element of the data
    acf_residuals <- list()




    # fill the matrix with the possible combinations of the iper-parameters
    count <- 1
    for (i in 1:length(p)) {
        for (j in 1:length(d)) {
            for (k in 1:length(q)) {
                iper_param_matrix[count, ] <- c(p[i], d[j], q[k])
                count <- count + 1
            }
        }
    }

    # fit the ARIMA model to each element of the data with 80%
    # of the elements of the data as training data

    for (i in 1:length(first_diff)) {
        resid_vector <- c()
        # vector containing the sample autocorrelation of the residuals
        acf_vector <- list()
        # AIC vector containing the AIC of the fitted model
        AIC_vector <- c()
        # vector containing the sum of AIC residuals of the fitted model
        sum_acf_residuals <- c()


        # define the vector containing the 80% of the elements of the data
        timeserie <- as.vector(first_diff[[i]])
        train_data <- timeserie[1:(length(timeserie) * 0.8)]

        # transform train_data into a time series
        train_data <- ts(as.vector(train_data))

        # define the vector containing the 20% of the elements of the data
        test_data <- timeserie[(length(timeserie) * 0.8) +
            1:length(timeserie)]

        # transform test_data into a time series
        test_data <- ts(test_data)



        # fit the ARIMA model to the training data with all
        # possible combinations of the iper-parameters given
        # by the matrix iper_param_matrix

        for (j in 1:nrow(iper_param_matrix)) {
            check <- FALSE
            print(paste("object:", i, "iper:", j, sep = " "))
            # define the vector containing the iper-parameters
            iper_param_vector <- iper_param_matrix[j, ]
            # try if the ARIMA will work (convergence problem) otherwise pass
            check <- tryCatch(
                {
                    # fit the ARIMA model
                    fitted_model <- arima(train_data,
                        order = iper_param_vector, method = "ML"
                    )
                    # prediction of the fitted model on the test data
                    predicted <- forecast(model = fitted_model, object = test_data)
                },
                error = function(e) {
                    # if the ARIMA model does not work,
                    # pass

                    return(TRUE)
                },
                warning = function(cond) {
                    # if the ARIMA model does not work,
                    # pass

                    return(TRUE)
                }
            )
            if (class(check) != "logical") {
                fitted_model <- arima(train_data,
                    order = iper_param_vector, method = "ML"
                )
                # prediction of the fitted model on the test data
                predicted <- forecast(model = fitted_model, object = test_data)

                # store the residual in the list resid_vector only if the
                # prediction residuals is not NA
                resid_vector <- c()
                for (z in 1:length(predicted$residuals)) {
                    if (is.finite(predicted$residuals[z])) {
                        resid_vector <- c(
                            resid_vector,
                            as.double(predicted$residuals[z])
                        )
                    }
                }

                # extrat the AIC of the fitted model
                AIC_vector <- c(AIC_vector, AIC_score(resid_vector))

                # tranform the residuals into a time series
                resid_ts <- c()
                for (item in 6:length(resid_vector)) {
                    resid_ts <- c(resid_ts, resid_vector[[item]][1])
                }
                resid_ts <- ts(resid_ts)
                # compute the sample autocorrelation of the residuals
                acf_vector[[j]] <- c(acf(resid_ts,
                    lag.max = 6,
                    plot = FALSE
                )$acf)
                # sum the sample autocorrelation of the residuals
                sum_acf_residuals <- c(sum_acf_residuals, sum(abs(acf_vector[[j]])))
            } else {
                print("ARIMA model does not work")
            }
        }

        # select the best ARIMA model for each element of the data with AIC
        # define the vector containing the AIC of the best ARIMA model
        # for each element of the data


        # define the index of the best ARIMA model
        # which minimizes the sum of the sample autocorrelation of the residuals

        index_min <- which.min(sum_acf_residuals)
        print(paste("best index:", index_min, "value:", AIC_vector[index_min], sep = " "))
        # define the vector containing the iper-parameters of the best
        # ARIMA model for each element of the data
        iper_param[i, ] <- iper_param_matrix[index_min, ]

        # define a vector containting the sample autocorrelation of the residuals
        # of the best ARIMA model for each element of the data
        acf_residuals[[i]] <- acf_vector[[index_min]]
    }
    output <- matrix(NA, nrow = length(first_diff), ncol = 9)
    for (i in 1:length(first_diff)) {
        output[i, ] <- c(
            iper_param[i, ][1],
            iper_param[i, ][2],
            iper_param[i, ][3],
            acf_residuals[[i]][1],
            acf_residuals[[i]][2],
            acf_residuals[[i]][3],
            acf_residuals[[i]][4],
            acf_residuals[[i]][5],
            acf_residuals[[i]][6]
        )
    }
    # give the row names to the output matrix
    rownames(output) <- c(
        "GNP", "Real GNP", "GNP GDP ratio", "Industrial Production", "Employment",
        "Unemployment rate", "GNP deflator", "Consumer prices", "Wages", "Real wages",
        "Money stock", "Velocity", "Bond yield", "Common stock prices",
        "Random walk with drift"
    )

    # give the column names to the output matrix
    colnames(output) <- c("AR(p)", "MA(d)", "MA(q)", "t1", "t2", "t3", "t4", "t5", "t6")

    return(output)
}

theUltimateOutput <- theUltimateFunction(first_diff)
# table 3 - the sample autocorrelation of the residuals of the best ARIMA model
View(theUltimateOutput)
