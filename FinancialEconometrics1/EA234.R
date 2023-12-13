##%% FE 1 v3
##%% @ncardenasfrias

# Load necessary packages and set personal path to documents
pacman::p_load(data.table, tseries, dplyr, bootUR, urca, gridExtra, tidyverse, gplots, xts, stargazer, forecast, ggplot2, vars)

setwd('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/FinancialEconometrics1')

###########################
#0. Preprocessing
###########################

#Load the dataset, data gathering and cleaning done in Python
df <- fread("DATA/data.csv")
as.data.table(df)
#change few names of columns to got something more clear 
names(df)[1] = 'Date'


#Apply log to variables for which it makes sense ie prices and quantities
df$sp500 = log(df$sp500) #get the stock market return 
df$splong = log(df$splong)
df$corp_debt = log(df$corp_debt) #return corporate debt (see index definition)
df$gdp = log(df$gdp)
df$rpi  = log(df$rpi)
df$dpi  = log(df$dpi)


#convert into TS 
allts = list()
numeric_cols = df[, sapply(df, is.numeric)&names(df)!= 'Date'] #identifies the right columns
for(col in names(numeric_cols)){
  allts[[col]] = ts(df[[col]], start= c(year(df$Date[1]), month(df$Date[1])), frequency=12)
}

allts[['sp500']] = NULL #remove short SP500, use YahooFinance series that has the 90's and 00's data
allts[['Date']] = NULL # R was making regressions on the date column :/
#remove columns we ended up not using in the final version 
allts[['manufacturing']] = NULL # rather not have data that was initially yearly
allts[['rpi']] = NULL # redundent with dpi 
allts[['unempl']] = NULL # did not lead anywhere 



#Reorganise the list 
desired_order = c('gdp', 'dpi', 'infl_e', 'deflator', 'rate', 'splong', 'corp_debt')
allts = allts[desired_order]
names = c('GDP', "Disposable Income", 'Inflation expectation', 'PIB deflator', 'Fed rate', 'SP500', 'Corporate debt rate')
rm(df, desired_order)

#Remove outliers => IT DOES NOT CHANGE ANYTHING
# tsclean(allts$infl_e, iterate = 2, lambda =NULL)
# tsclean(allts$deflator, iterate = 2, lambda =NULL)
# tsclean(allts$unempl, iterate = 2, lambda =NULL)
# tsclean(allts$rate, iterate = 2, lambda =NULL)
# tsclean(allts$splong, iterate = 2, lambda =NULL)
# tsclean(allts$corp_debt, iterate = 2, lambda =NULL)



###########################
#1. Dynamics
###########################


#####
#1. UR TEST - ADF, full procedure

### ADF 1st regression: deterministic trend + drift 
results_adf_trend <- list()
for (var_name in names(allts)) {
  result <- ur.df(allts[[var_name]], type = "trend", selectlags = "BIC")
  results_adf_trend[[var_name]] <- summary(result)
}
#export summary adf
trend_test =cbind(t(results_adf_trend$gdp@teststat), t(results_adf_trend$dpi@teststat),
                  t(results_adf_trend$infl_e@teststat),t(results_adf_trend$deflator@teststat), 
                  t(results_adf_trend$rate@teststat),
                  t(results_adf_trend$splong@teststat), t(results_adf_trend$corp_debt@teststat),
                  results_adf_trend$infl_e@cval)
colnames(trend_test) = c(names(results_adf_trend), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(trend_test, type='text')
# stargazer(trend_test, out='TABLES/adf_trend.tex', label= 'tab:adftrend_hyp',title = "ADF test - 1st regression with drift, deterministic trend and stochastic trend")

#export all t-values on coefficients, harder because output format
#create empty df with columns = rhs variable in ADF
t_values_table = data.frame(matrix(nrow = 0, ncol = length(c('Intercept', 'z.lag.1', 'tt', 'z.diff.lag')))) 
colnames(t_values_table) = c('alpha', 'gamma', 'beta', 'rho')
#iterate over the summaries and extract the t stats for each series
for (i in names(results_adf_trend)){
  j = results_adf_trend[[i]]@testreg$coefficients[,'t value']
  tab = as.data.frame(j)
  row = c(tab$j[1], tab$j[2], tab$j[3], tab$j[4])
  t_values_table[nrow(t_values_table) + 1,] <- row
}
row.names(t_values_table) = names(results_adf_trend) # call the rows as the series
t_values_table = t(t_values_table) #transpose the table, looks better
stargazer(t_values_table,type='text') #all ok
# stargazer(t_values_table,out="TABLES/adf_tstats_trend.tex", title="ADF test - 1st regression t statistics",
#         notes = '\\footnotesize Notes: With N=396, critical values at 5\\%: alpha = 3.09 ; gamma= -3.43 ; beta = 2.79',
#        label='tab:tstat_trend')

#extract series that need to keep being tested
adf_v2 = allts[c(1:2, 5:7)]
names(adf_v2) = names(allts)[c(1:2, 5:7)]



### ADF 2nd regression: drift
results_adf_drift <- list()
for (var_name in names(adf_v2)) {
  result <- ur.df(adf_v2[[var_name]], type = "drif", selectlags = "BIC")
  results_adf_drift[[var_name]] <- summary(result)
}
#export summary adf
drift_test =cbind(t(results_adf_drift$gdp@teststat), t(results_adf_drift$dpi@teststat),
                  t(results_adf_drift$rate@teststat), t(results_adf_drift$splong@teststat), 
                  t(results_adf_drift$corp_debt@teststat), results_adf_drift$corp_debt@cval)
colnames(drift_test) = c(names(results_adf_drift), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(drift_test, type='text')
# stargazer(drift_test, out='TABLES/adf_drift.tex', label= 'tab:adfdrift_hyp',title = "ADF test - 2nd regression with drift and stochastic trend")

#export all t-values on coefficients, harder because output format
#create empty df with columns = rhs variable in ADF
t_values_table2 = data.frame(matrix(nrow = 0, ncol = length(c('Intercept', 'z.lag.1', 'z.diff.lag')))) 
colnames(t_values_table2) = c('alpha', 'gamma', 'rho')
#iterate over the summaries and extract the t stats for each series
for (i in names(results_adf_drift)){
  j = results_adf_drift[[i]]@testreg$coefficients[,'t value']
  tab = as.data.frame(j)
  row = c(tab$j[1], tab$j[2], tab$j[3])
  t_values_table2[nrow(t_values_table2) + 1,] <- row
}
row.names(t_values_table2) = names(results_adf_drift) # call the rows as the series
t_values_table2 = t(t_values_table2) #transpose the table, looks better
stargazer(t_values_table2,type='text') #all ok
# stargazer(t_values_table2,out="TABLES/adf_tstats_drift.tex", title="ADF test - 2nd regression t statistics",
#           notes = '\\footnotesize Notes: With N=396, critical values at 5\\%: alpha = 2.53 ; gamma= -2.88',
#           label='tab:tstat_drift')

adf_v3 = adf_v2[1:4] #series to keep testing
names(adf_v3) = names(adf_v2[1:4])


### ADF 3rd regression: UR only
results_adf_none <- list()
for (var_name in names(adf_v3)) {
  result <- ur.df(adf_v3[[var_name]], type = "none", selectlags = "BIC")
  results_adf_none[[var_name]] <- summary(result)
}
#export summary adf
none_test =cbind(t(results_adf_none$gdp@teststat), 
                t(results_adf_none$dpi@teststat), t(results_adf_none$rate@teststat),
                 t(results_adf_none$splong@teststat), results_adf_none$rate@cval)
colnames(none_test) = c(names(results_adf_none), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(none_test, type='text')
# stargazer(none_test, out='TABLES/adf_none.tex', label= 'tab:adfnone_hyp',title = "ADF test - 3rd regression with stochastic trend")


#export all t-values on coefficients, harder because output format
#create empty df with columns = rhs variable in ADF
t_values_table3 = data.frame(matrix(nrow = 0, ncol = length(c('z.lag.1', 'z.diff.lag')))) 
colnames(t_values_table3) = c('gamma', 'rho')
#iterate over the summaries and extract the t stats for each series
for (i in names(results_adf_none)){
  j = results_adf_none[[i]]@testreg$coefficients[,'t value']
  tab = as.data.frame(j)
  row = c(tab$j[1], tab$j[2], tab$j[3])
  t_values_table3[nrow(t_values_table3) + 1,] <- row
}
row.names(t_values_table3) = names(results_adf_none) # call the rows as the series
t_values_table3 = t(t_values_table3) #transpose the table, looks better
stargazer(t_values_table3,type='text') #all ok
# stargazer(t_values_table3,out="TABLES/adf_tstats_none.tex", title="ADF test - 3rd regression t statistics",
#           notes = '\\footnotesize Notes: With N=396, critical values at 5\\%: gamma= -1.95',
#           label='tab:tstat_none')



## Get deltas 
deltas = list (diff(allts$gdp), diff(allts$dpi),
                diff(allts$rate),diff(allts$splong))
names(deltas) = list("d_gdp","d_dpi","d_rate", "d_splong")

### CHECK THAT DELTAS ARE I(0), I am using same code as in levels 
results_adf_trend <- list()
for (var_name in names(deltas)) {
  result <- ur.df(deltas[[var_name]], type = "trend", selectlags = "BIC")
  results_adf_trend[[var_name]] <- summary(result)
}
#export summary adf
trend_deltas =cbind(t(results_adf_trend$d_gdp@teststat), #t(results_adf_trend$d_rpi@teststat),
                    t(results_adf_trend$d_dpi@teststat), t(results_adf_trend$d_rate@teststat),
                    t(results_adf_trend$d_splong@teststat), results_adf_trend$d_rate@cval)
colnames(trend_deltas) = c(names(results_adf_trend), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(trend_deltas, type='text')
#stargazer(trend_deltas, out='TABLES/adf_deltas.tex', label= 'tab:adfdeltas_hyp',title = "ADF test - 1st regression with drift, deterministic trend and stochastic trend for series in deltas")



## COINTEGRATION
levels_var = data.frame(
  splong = allts$splong,
  rate = allts$rate,
  gdp = allts$gdp,
  dpi = allts$dpi) 
  
#levels_var$timestamp <- time(deseasonalized_ts$deflator) 
names_level = c('splong', 'rate', 'gdp', 'dpi')


johansen_test = ca.jo(levels_var, type='trace', ecdet='trend', K=8)
johansensum = summary(johansen_test)
# r is rank of matrix == number of cointegration relationship.
#no cointegration
johansen_table = xtable(johansensum)
print(latex_table, file=file_path)

############ EMPIRICAL ANALYSIS 2
if (!require(vars)) install.packages("vars")
library(vars)

# Selecting the three series for the VAR model
var_data <- data.frame(splong = allts$splong, gdp = allts$gdp, rate = allts$rate)

# (Assuming the series are non-stationary and need differencing based on ADF test results)
diff_var_data <- diff(var_data, differences = 1)

# Identify the order of the VAR model
var_order <- VARselect(diff_var_data, lag.max = 10, type = "both")$selection

# Estimate the VAR model
var_model1 <- VAR(diff_var_data, p = var_order, type = "both")

# Output 
summary(var_model1)

######## EMPIRICAL APPLICATION 3 (after johansen no coint) 

# Selecting the same I(1) components as in Empirical Analysis 2
var_data2 <- data.frame(
    splong = allts$splong, 
    gdp = allts$gdp, 
    rate = allts$rate
)

# Differencing the data to achieve stationarity
diff_var_data2 <- diff(var_data2, differences = 1)

# Identify the order of the VAR model
var_order2 <- VARselect(diff_var_data2, lag.max = 10, type = "both")$selection

# Estimate the VAR model
var_model2 <- VAR(diff_var_data2, p = var_order2, type = "both")
summary(var_model2)

# Causality Tests for the modified model

# Causality test between GDP and Federal Reserve Rate
causality_gdp_rate2 <- causality(var_model2, cause = "gdp", effect = "rate")
print(causality_gdp_rate2)

# Causality test between SPLong and GDP
causality_splong_gdp <- causality(var_model2, cause = "splong", effect = "gdp")
print(causality_splong_gdp)

# Causality test between SPLong and Federal Reserve Rate
causality_splong_rate <- causality(var_model2, cause = "splong", effect = "rate")
print(causality_splong_rate)

###### EMPIRICAL APPLICATION 4

# Impulse Response Analysis for var_model1 from Empirical Analysis 2 + its graph
irf_var_model1 <- irf(var_model1, n.ahead = 10, boot = TRUE, ci = 0.95)
plot(irf_var_model1)

# Impulse Response Analysis for var_model2 from Empirical Application 3 + its graph
irf_var_model2 <- irf(var_model2, n.ahead = 10, boot = TRUE, ci = 0.95)
plot(irf_var_model2)
####statistical analysis

# Setting up the plotting area for side-by-side comparison
par(mfrow=c(2,1))

# Plotting IRF for VAR Model 1
plot(irf_var_model1, main = "Impulse Responses for VAR Model 1")

# Plotting IRF for VAR Model 2
plot(irf_var_model2, main = "Impulse Responses for VAR Model 2")

# Resetting the plotting layout
par(mfrow=c(1,1))
