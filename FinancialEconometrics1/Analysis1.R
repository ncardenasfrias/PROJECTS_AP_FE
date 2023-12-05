##%% FE 1 
##%% @ncardenasfrias

# Load necessary packages and set personal path to documents
pacman::p_load(data.table, dplyr, urca, gridExtra, tidyverse, gplots, xts, stargazer, forecast, ggplot2)

setwd('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/FinancialEconometrics1')

###########################
#0. Preprocessing
###########################

#Load the dataset, data gathering and cleaning done in Python
df <- fread("DATA/data.csv")
as.data.table(df)
#change few names of columns to got something more clear 
names(df)[1] = 'Date'
names(df)[8] = 'splong'

#Apply log to variables for which it makes sense
df$sp500 = log(df$sp500) #get the stock market return 
df$splong = log(df$splong)
df$corp_debt = log(df$corp_debt) #return corporate debt (see index definition)


#convert into TS 
allts = list()
numeric_cols = df[, sapply(df, is.numeric)&names(df)!= 'Date'] #identifies the right columns
for(col in names(numeric_cols)){
  allts[[col]] = ts(df[[col]], start= c(year(df$Date[1]), month(df$Date[1])), frequency=12)
}

allts[['sp500']] = NULL #remove short SP500, use YahooFinance series that has the 90's ans 00's data
allts[['Date']] = NULL # R was making regressions on the date column :/

###########################
#1. Dynamics
###########################

#####
#1.1. Series decomposition

dec_infl = decompose(allts$infl_e)
dec1 = autoplot(dec_infl) + labs(title = "Inflation expectation")

dec_rate = decompose(allts$rate)
dec2 = autoplot(dec_rate) + labs(title = "Fed fund rate")

dec_spl = decompose(allts$splong) # I am using just the 'complete' series for SP500
dec3 = autoplot(dec_spl) + labs(title = "SP500")

dec_debt = decompose(allts$corp_debt)
dec4 = autoplot(dec_debt) + labs(title = "Corporate debt return")

dec_defl = decompose(allts$deflator)
dec5 = autoplot(dec_defl) + labs(title = "GDP deflator")

dec_u = decompose(allts$unempl)
dec6 = autoplot(dec_u) + labs(title = "Unemployment rate")

decomposition=list(dec_infl, dec_rate, dec_debt, dec_defl, dec_u, dec_spl)
names(decomposition) = names(allts)


#Plot the decomposed series in 2 groups, save the figures
decomp_i = grid.arrange(dec1, dec2, dec3, ncol=3)
ggsave("IMAGES/decomposition_i.png", plot = decomp_i, width =12, height=8)

decomp_ii = grid.arrange(dec4, dec5, dec6, ncol=3)
ggsave("IMAGES/decomposition_ii.png", plot = decomp_ii, width = 12, height = 8)

rm(dec1, dec2, dec3, dec4, dec5, dec6, decomp_i, decomp_ii) # I don't need the graphs anymore


#####
#1.2. UR test

#MWE
#tinfl_e = ur.df(allts$infl_e, type='trend',selectlags=c('BIC')) 
#t = summary(tinfl_e)

# Perform ur.df tests WITH TREND AND DRIFT for each variable and store the summaries
results_adf_trend <- list()
for (var_name in names(allts)) {
  result <- ur.df(allts[[var_name]], type = "trend", selectlags = "BIC")
  results_adf_trend[[var_name]] <- summary(result)
}

trend_test =cbind(t(results_adf_trend$infl_e@teststat),t(results_adf_trend$rate@teststat), 
                  t(results_adf_trend$splong@teststat),t(results_adf_trend$corp_debt@teststat),
                  t(results_adf_trend$deflator@teststat), t(results_adf_trend$unempl@teststat),
                  results_adf_trend$infl_e@cval)
colnames(trend_test) = c(names(results_adf_trend), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(trend_test, out='TABLES/adf_trend.tex', title = "ADF test - 1st regression with drift, deterministic trend and stochastic trend")
  
# Perform ur.df tests WITH DRIFT ONLY for each variable and store the summaries
results_adf_drift <- list()
for (var_name in names(allts)) {
  result <- ur.df(allts[[var_name]], type = "drift", selectlags = "BIC")
  results_adf_drift[[var_name]] <- summary(result)
}

drift_test =cbind(t(results_adf_drift$infl_e@teststat),t(results_adf_drift$rate@teststat), 
                  t(results_adf_drift$splong@teststat),t(results_adf_drift$corp_debt@teststat),
                  t(results_adf_drift$deflator@teststat), t(results_adf_drift$unempl@teststat),
                  results_adf_drift$infl_e@cval)
colnames(drift_test) = c(names(results_adf_drift), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(drift_test, out='TABLES/adf_drift.tex', title = "ADF test - 2nd regression with drift and stochastic trend")
  
# Perform ur.df tests WITH NONE for each variable and store the summaries
results_adf_none <- list()
for (var_name in names(allts)) {
  result <- ur.df(allts[[var_name]], type = "none", selectlags = "BIC")
  results_adf_none[[var_name]] <- summary(result)
}

none_test =cbind(t(results_adf_none$infl_e@teststat),t(results_adf_none$rate@teststat), 
                  t(results_adf_none$splong@teststat),t(results_adf_none$corp_debt@teststat),
                  t(results_adf_none$deflator@teststat), t(results_adf_none$unempl@teststat),
                 results_adf_none$infl_e@cval)
colnames(none_test) = c(names(results_adf_drift), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(none_test, out='TABLES/adf_none.tex', title = "ADF test - 3rd regression regression with only stochastic trend")


#### SUMMARY 
library(bootUR)
order_integration(allts$infl_e, method = "adf")$order_int # I(0)
order_integration(allts$rate, method = "adf")$order_int # I(0)
order_integration(allts$corp_debt, method = "adf")$order_int # I(1)
order_integration(allts$deflator, method = "adf")$order_int # I(0)
order_integration(allts$unempl, method = "adf")$order_int # I(1)
order_integration(allts$splong, method = "adf")$order_int # I(1)



####!!!!!! NEED TO COMPLETE THIS NEATLY 


## Get the deltas of the series integrated order one
ts_deltas = list(diff(allts$corp_debt), diff(allts$splong), diff(allts$unempl))
names(ts_deltas) = c('d_corpdebt', "d_splong", "d_unempl")

order_integration(ts_deltas$d_corpdebt, method='adf')$order_int # I(0)
order_integration(ts_deltas$d_splong, method='adf')$order_int # I(0)
order_integration(ts_deltas$d_unempl, method='adf')$order_int # I(0)
#we can work with these 3 variables in deltas 

dec_dsp = decompose(ts_deltas$d_splong)
decdesp = autoplot(dec_dsp) + labs(title = expression(paste(Delta, " SP500")))
dec_ddebt = decompose(ts_deltas$d_corpdebt)
decddebt = autoplot(dec_ddebt) + labs(title = expression(paste(Delta, " Corporate debt")))
dec_du = decompose(ts_deltas$d_unempl)
decdu = autoplot(dec_du) + labs(title = expression(paste(Delta, " Unemployment")))

decomp_iii_deltas = grid.arrange(decdesp, decddebt, decdu, ncol=3)
ggsave("IMAGES/decomposition_iii_deltas.png", plot = decomp_iii_deltas, width = 12, height = 8)
rm(decdesp, decddebt, decdu, decomp_iii_deltas) #. clear the graphs

#Add these new series to our lists of TS (decomposition and allts)
decomposition = c(decomposition, list(dec_dsp, dec_ddebt, dec_du))
names(decomposition) = c(names(allts),list("d_sp500", "d_corp_debt", "d_unempl"))
allts = c(allts, ts_deltas)

#list all I(0) series ie mixes levels and deltas                                                           
i0_series = list(allts$infl_e, allts$rate, ts_deltas$d_splong, ts_deltas$d_corpdebt,allts$deflator, ts_deltas$d_unempl)
names(i0_series) = c('infl_e', 'rate', 'd_sp500', 'd_corp_debt', "deflator", 'd_unempl')



#####
#1.3. Seasonal variations
# Use the seasonal estimation coming from decompose() done earlier, all the decompositions are stored in decomposition list (deltas and levels)
#fill table with values of the estimations rounded 3 decimal places 
seasonal_table <- data.frame(matrix(ncol = 13, nrow = length(decomposition)))
colnames(seasonal_table) <- c("Series", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                              "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
for (i in 1:length(decomposition)) {
  series_name <- names(decomposition)[i]  # Assuming series names are "Series1", "Series2", etc.
  seasonal_coef <- decomposition[[i]]$seasonal
  seasonal_coef_rounded <- round(seasonal_coef, 3)
  row_values <- c(series_name, seasonal_coef_rounded)
  seasonal_table[i, ] <- row_values
}
rownames(seasonal_table) = seasonal_table$Series #series as index, remove the Series column 
seasonal_table$Series = NULL
#stargazer(seasonal_table, out='TABLES/seasons.tex', summary=FALSE, label="tab:seasons", title="Estimation of the seasonality of each series")
#need to manually add a resize box on the latex code



## Get list of deseasonalized series of the I(0) series using STL 
deseasonalized_ts = list()
for (ts in allts) {
  decomposed_ts = stl(ts, s.window = "periodic") #'periodic' for additive seasonality
  deseason = ts - decomposed_ts$time.series[,'seasonal']
  deseasonalized_ts[[length(deseasonalized_ts)+1]] = deseason
}
names(deseasonalized_ts) = names(allts)
rm(decomposed_ts)

#Another way to deseasonalize the series with initial decomposition, here I am doing it only on I(0) series
#CAREFULL, by deseasoning now some I(0) series might become I(1)
de_seasonalized_series <- list()
i0_no_seson_decomp = list()
for (ts in i0_series) {
  decomposed = decompose(ts) #need to repeat this because we're in another list :/
  seasonal_component = decomposed$seasonal
  de_seasonalized_ts = ts - seasonal_component #additive seasonality for all series (decomposition$serie$type), just remove those components
  de_seasonalized_series[[length(de_seasonalized_series) + 1]] <- de_seasonalized_ts
  i0_no_seson_decomp[[length(i0_decomp)+1]] =decomposed
}
names(de_seasonalized_series) = names(i0_series)
names(i0_decomp) = names(i0_series)


#####
#1.3 Fit aRMA O




#####
#1.4. Cyclical component

#Check that cyclical component ie the residuals are wn ie no parameter is significant 



auto.arima(decomposition$infl_e$random) #ARIMA(3,0,1) 
auto.arima(decomposition$rate$random) #ARIMA(2,0,0)(2,0,0)
auto.arima(decomposition$d_sp500$random) #ARIMA(3,0,1) 
auto.arima(decomposition$d_corp_debt$random) #ARIMA(3,0,1) 
auto.arima(decomposition$deflator$random) #ARIMA(3,0,1) 
auto.arima(decomposition$d_unempl$random) #ARIMA(3,0,1) 


test_decomposed = slt(i0_series$rate, s.window='periodic')


### ACF plots of the I(0) series
acf_plots = list()
pacf_plots = list()
# Loop through each time series to generate its ACF plot
for (i in 1:length(i0_series)) {
  acf_plots[[i]] = ggAcf(i0_series[[i]], lag.max=20, main =names(i0_series)[i])
  pacf_plots[[i]] = ggPacf(i0_series[[i]], lag.max=20, main =names(i0_series)[i])
}
acf = grid.arrange(grobs = acf_plots, ncol = 2) 
pacf = grid.arrange(grobs = pacf_plots, ncol=2)


Acf(deseasonalized_ts$rate, lag.max = 100, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)
Acf(diff(allts$rate), lag.max = 12, type="correlation", plot=TRUE)

Pacf(x, lag.max = 40, plot = TRUE, na.action = na.contiguous, demean = TRUE)

# Arrange the ACF plots in a grid


###########################
#2. VAR
###########################

#####
#2.1. Canonical VAR: levels 



#####
#2.2. Canonical VAR: deltas




###########################
#2. Cointegration
###########################

#####
#3.1. Cointegration rank/Johansen test 
cointegration_test <- ca.jo(i0_no_seson_de$infl_e, type = "trace", ecdet = "none", K = 2)
summary(cointegration_test)



##### 
#3.1.1. Estimate cointegration relationship 

