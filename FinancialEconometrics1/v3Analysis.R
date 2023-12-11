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

allts[['sp500']] = NULL #remove short SP500, use YahooFinance series that has the 90's and 00's data
allts[['Date']] = NULL # R was making regressions on the date column :/

#Reorganise the list 
desired_order = c('infl_e', 'deflator', 'unempl', 'rate', 'splong', 'corp_debt')
allts = allts[desired_order]
names = c('Inflation expectation', 'PIB deflator', 'Unemployment rate', 'Fed rate', 'SP500', 'Corporate debt rate')
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
trend_test =cbind(t(results_adf_trend$infl_e@teststat),t(results_adf_trend$deflator@teststat), 
                  t(results_adf_trend$unempl@teststat),t(results_adf_trend$rate@teststat),
                  t(results_adf_trend$splong@teststat), t(results_adf_trend$corp_debt@teststat),
                  results_adf_trend$infl_e@cval)
colnames(trend_test) = c(names(results_adf_trend), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(trend_test, type='text')
stargazer(trend_test, out='TABLES/adf_trend.tex', label= 'tab:adftrend_hyp',title = "ADF test - 1st regression with drift, deterministic trend and stochastic trend")

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
stargazer(t_values_table,out="TABLES/adf_tstats_trend.tex", title="ADF test - 1st regression t statistics",
        notes = '\\footnotesize Notes: With N=396, critical values at 5\\%: alpha = 3.09 ; gamma= -3.43 ; beta = 2.79',
       label='tab:tstat_trend')

#extract series that need to keep being tested
adf_v2 = allts[4:6]
names(adf_v2) = names(allts)[4:6]



### ADF 2nd regression: drift
results_adf_drift <- list()
for (var_name in names(adf_v2)) {
  result <- ur.df(adf_v2[[var_name]], type = "drif", selectlags = "BIC")
  results_adf_drift[[var_name]] <- summary(result)
}
#export summary adf
drift_test =cbind(t(results_adf_drift$rate@teststat), t(results_adf_drift$splong@teststat), 
                  t(results_adf_drift$corp_debt@teststat), results_adf_drift$corp_debt@cval)
colnames(drift_test) = c(names(results_adf_drift), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(drift_test, type='text')
stargazer(drift_test, out='TABLES/adf_drift.tex', label= 'tab:adfdrift_hyp',title = "ADF test - 2nd regression with drift and stochastic trend")

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
stargazer(t_values_table2,out="TABLES/adf_tstats_drift.tex", title="ADF test - 2nd regression t statistics",
          notes = '\\footnotesize Notes: With N=396, critical values at 5\\%: alpha = 2.53 ; gamma= -2.88',
          label='tab:tstat_drift')

adf_v3 = adf_v2[1:2] #series to keep testing
names(adf_v3) = names(adf_v2[1:2])


### ADF 3rd regression: UR only
results_adf_none <- list()
for (var_name in names(adf_v3)) {
  result <- ur.df(adf_v3[[var_name]], type = "none", selectlags = "BIC")
  results_adf_none[[var_name]] <- summary(result)
}
#export summary adf
none_test =cbind(t(results_adf_none$rate@teststat),t(results_adf_none$splong@teststat),
                 results_adf_none$rate@cval)
colnames(none_test) = c(names(results_adf_none), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(none_test, type='text')
stargazer(none_test, out='TABLES/adf_none.tex', label= 'tab:adfnone_hyp',title = "ADF test - 3rd regression with stochastic trend")


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
stargazer(t_values_table3,out="TABLES/adf_tstats_none.tex", title="ADF test - 3rd regression t statistics",
          notes = '\\footnotesize Notes: With N=396, critical values at 5\\%: gamma= -1.95',
          label='tab:tstat_none')



## Get deltas 
deltas = list (diff(allts$rate),diff(allts$splong))
names(deltas) = list("d_rate", "d_splong")

### CHECK THAT DELTAS ARE I(0), I am using same code as in levels 
results_adf_trend <- list()
for (var_name in names(deltas)) {
  result <- ur.df(deltas[[var_name]], type = "trend", selectlags = "BIC")
  results_adf_trend[[var_name]] <- summary(result)
}
#export summary adf
trend_deltas =cbind(t(results_adf_trend$d_rate@teststat), t(results_adf_trend$d_splong@teststat), 
                    results_adf_trend$d_rate@cval)
colnames(trend_deltas) = c(names(results_adf_trend), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(trend_deltas, type='text')
stargazer(trend_deltas, out='TABLES/adf_deltas.tex', label= 'tab:adfdeltas_hyp',title = "ADF test - 1st regression with drift, deterministic trend and stochastic trend for series in deltas")



## CoINTEGRATION
levels_var = data.frame(
  splong = allts$splong,
  rate = allts$rate)
#levels_var$timestamp <- time(deseasonalized_ts$deflator) 
names_level = c('splong', 'rate')


johansen_test = ca.jo(levels_var, type='trace', ecdet='trend', K=8)
johansensum = summary(johansen_test)
# r is rank of matrix == number of cointegration relationship.
#no cointegration
johansen_table = xtable(johansensum)
print(latex_table, file=file_path)






















