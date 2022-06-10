# Macroeconomics-Assignment2
## Data Generating Process
Simulate an AR(i), a MA(i) and an ARMA(i,j) study the correlogram, where you must choose i and j as
positive integers, while the parameters can be positive or negative. Repeat the experiment for two i and two
j. Chose at least two parameters positive and two negative for each model you define. Briefly comment your
results. (Hint: the idea is to play with different models in order to understand the nature of the series from a
visual inspection).
----------------------
## Forecasting comparison
Simulate one model at your choice for example an AR(1) (do not simultae a nonstationary process) and
divide your sample in two parts, the first for estimation and forecast, the second for comparison with the
forecast. Remember that the forecast must have the same length of the sample saved for comparison. Use
MAE and RMSE to compare AR(1), ARMA(2,1), naive forecast (just repeat the last observation of the
sample used for estimation purposes as many times as the comparison sample), mean forecast (just repeat
the mean of the sample used for estimation purposes as many times as the comparison sample)
Comment your results
-------------------
## Nelson Plosser data
Study the data and the results of Nelson Plosser http://hedibert.org/wp-content/uploads/2015/03/nelson-
plosser-1982.pdf using the dataset in package (urca or tseries)
Download the same series from FRED and repeat the results of NP for the extended sample.
It is your choice using data in level in log level or detrend them. Comment the choice you made.
For each variable plot the original data (chose whether in logs) and ACF/PACF, the time detrended data
and the data without stochastic trend.
Test formally the stationarity of you series with one or two tests at your choice.
-------------------
## 4 Cointegration
Download from FRED the following interest rates: 3 Months Treasury Bill, 2 Years Treasury Bonds and 10
Years Treasury Bonds.
The difference between 10YR and 3M is known as term spread. It is a proxy of the risk in holding longer
maturities with respect to liquid cash or very short term Teasury Bills.
The difference between 10YR and 2YR is considered in the US an good indicator of future recession if negative
(https://www.chicagofed.org/publications/chicago-fed-letter/2018/404).
Study the stationarity of the series you download and verify any cointegration relationship in the two cases.
Comment your results.
