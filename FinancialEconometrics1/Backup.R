##%% FE 1 v3
##%% @ncardenasfrias

# Load necessary packages and set personal path to documents
pacman::p_load(data.table, tseries, dplyr, bootUR, urca, gridExtra, tidyverse, gplots, xts, stargazer, forecast, ggplot2, vars)

setwd('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/FinancialEconometrics1')

###########################
#0. Preprocessing
###########################

#Load the dataset, data gathering and cleaning done in Python
df = fread("DATA/data.csv")
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
allts[['corp_debt']] = NULL # we end up not using it and the UR test are weird


#Reorganise the list 
desired_order = c('gdp', 'dpi', 'infl_e', 'deflator', 'rate', 'splong')
allts = allts[desired_order]
names = c('GDP', "Disposable Income", 'Inflation expectation', 'PIB deflator', 'Fed rate', 'SP500')
rm(df, desired_order)

#Remove outliers => IT DOES NOT CHANGE ANYTHING
# tsclean(allts$infl_e, iterate = 2, lambda =NULL)
# tsclean(allts$deflator, iterate = 2, lambda =NULL)
# tsclean(allts$unempl, iterate = 2, lambda =NULL)
# tsclean(allts$rate, iterate = 2, lambda =NULL)
# tsclean(allts$splong, iterate = 2, lambda =NULL)
# tsclean(allts$corp_debt, iterate = 2, lambda =NULL)



#################################
#1.UR TEST - ADF, full procedure
#################################

### ADF 1st regression: deterministic trend + drift 
results_adf_trend = list()
for (var_name in names(allts)) {
  result = ur.df(allts[[var_name]], type = "trend", selectlags = "BIC")
  results_adf_trend[[var_name]] = summary(result)
}
#export summary adf
trend_test =cbind(t(results_adf_trend$gdp@teststat), t(results_adf_trend$dpi@teststat),
                  t(results_adf_trend$infl_e@teststat),t(results_adf_trend$deflator@teststat), 
                  t(results_adf_trend$rate@teststat), t(results_adf_trend$splong@teststat),
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
  t_values_table[nrow(t_values_table) + 1,] = row
}
row.names(t_values_table) = names(results_adf_trend) # call the rows as the series
t_values_table = t(t_values_table) #transpose the table, looks better
stargazer(t_values_table,type='text') #all ok
# stargazer(t_values_table,out="TABLES/adf_tstats_trend.tex", title="ADF test - 1st regression t statistics",
#         notes = '\\footnotesize Notes: With N=396, critical values at 5\\%: alpha = 3.09 ; gamma= -3.43 ; beta = 2.79',
#        label='tab:tstat_trend')


#extract series that need to keep being tested ie all but deflator and inflation expectation
adf_v2 = allts[c(1:2, 5:6)]
names(adf_v2) = names(allts)[c(1:2, 5:6)]



### ADF 2nd regression: drift
results_adf_drift = list()
for (var_name in names(adf_v2)) {
  result = ur.df(adf_v2[[var_name]], type = "drif", selectlags = "BIC")
  results_adf_drift[[var_name]] = summary(result)
}
#export summary adf
drift_test =cbind(t(results_adf_drift$gdp@teststat), t(results_adf_drift$dpi@teststat),
                  t(results_adf_drift$rate@teststat), t(results_adf_drift$splong@teststat), 
                  results_adf_drift$splong@cval)
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
  t_values_table2[nrow(t_values_table2) + 1,] = row
}
row.names(t_values_table2) = names(results_adf_drift) # call the rows as the series
t_values_table2 = t(t_values_table2) #transpose the table, looks better
stargazer(t_values_table2,type='text') #all ok
# stargazer(t_values_table2,out="TABLES/adf_tstats_drift.tex", title="ADF test - 2nd regression t statistics",
#           notes = '\\footnotesize Notes: With N=396, critical values at 5\\%: alpha = 2.53 ; gamma= -2.88',
#           label='tab:tstat_drift')



adf_v3 = adf_v2 #series to keep testing (They are the same but oh well, I had a different pipeline with the other series)
names(adf_v3) = names(adf_v2)


### ADF 3rd regression: UR only
results_adf_none = list()
for (var_name in names(adf_v3)) {
  result = ur.df(adf_v3[[var_name]], type = "none", selectlags = "BIC")
  results_adf_none[[var_name]] = summary(result)
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
  t_values_table3[nrow(t_values_table3) + 1,] = row
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
results_adf_trend = list()
for (var_name in names(deltas)) {
  result = ur.df(deltas[[var_name]], type = "trend", selectlags = "BIC")
  results_adf_trend[[var_name]] = summary(result)
}
#export summary adf
trend_deltas =cbind(t(results_adf_trend$d_gdp@teststat), #t(results_adf_trend$d_rpi@teststat),
                    t(results_adf_trend$d_dpi@teststat), t(results_adf_trend$d_rate@teststat),
                    t(results_adf_trend$d_splong@teststat), results_adf_trend$d_rate@cval)
colnames(trend_deltas) = c(names(results_adf_trend), "CV 1pct", "CV 5pct", "CV 10pct")
stargazer(trend_deltas, type='text')
#stargazer(trend_deltas, out='TABLES/adf_deltas.tex', label= 'tab:adfdeltas_hyp',title = "ADF test - 1st regression with drift, deterministic trend and stochastic trend for series in deltas")





#################################
#2. Decompositions in levels 
#################################
# use stl function to perform the decompositions and then OLS to estimate parameters

#### Series in levels that are stationary. 
i0_levels = list(allts$infl_e, allts$deflator) #from adf!
names(i0_levels) = c('infl_e', "deflator")

decompositions = list() #store the decompositions 
dec_graphs= list() #store the decomposition graphs 
deseasonalized_ts = list() #store the deseasonalized ts, if seasonality ends up being important 
for (ts in 1:length(i0_levels)) {
  #print(i0_levels[ts])}
  decomp = stl(i0_levels[[ts]], s.window='periodic') #additive seasonality seems right from graphs
  graph = autoplot(decomp) + labs(title = names(i0_levels)[ts])
  deseason = i0_levels[[ts]] - decomp$time.series[,'seasonal']
  decompositions[[length(decompositions)+1]] = decomp
  dec_graphs[[length(dec_graphs)+1]] = graph
  deseasonalized_ts[[length(deseasonalized_ts)+1]] = deseason
}
names(deseasonalized_ts) = names(i0_levels) #name the series in deseasonalized_ts
names(decompositions) = names(i0_levels) #name the series in decompositions

rm(graph, decomp, deseason)

#Generate and export plots with the decomposition graphs 
decom_i = grid.arrange(dec_graphs[[1]],dec_graphs[[2]], dec_graphs[[3]], ncol=3)
# ggsave('IMAGES/decomposition_i.png', plot=decom_i, width = 12, height = 8)

### Run OLS regression on each component of the TS
#create a df with all the series and the monthly dummies
start_date = as.Date("1990-01-01")
end_date = as.Date("2022-12-01") # Assuming December 2022
all_dates = seq(start_date, end_date, by = "month")

df_i0level = data.frame(Date = all_dates)
for (i in seq_along(i0_levels)) {
  col_name = paste0("Series_", i)
  df_i0level[[col_name]] = i0_levels[[i]]
}
colnames(df_i0level) = c('Date', names(i0_levels))
rownames(df_i0level) = df_i0level$Date #date as index
df_i0level$MONTH=month(df_i0level$Date) #Get month dummies 
df_i0level[paste0("M", 1:12)] = as.data.frame(t(sapply(df_i0level$MONTH, tabulate, 12)))

df_i0level$Date = NULL #no more need date column
df_i0level$MONTH = NULL #no more need MONTH column

df_i0level$trend = seq_along(df_i0level$infl_e) #deterministic trend

# Run OLS regression
infl_e_ols_dec = lm(infl_e ~ trend + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = df_i0level)
deflator_ols_dec = lm(deflator ~ trend + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = df_i0level)
corp_debt_ols_dec = lm(corp_debt ~ trend + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = df_i0level)

stargazer(infl_e_ols_dec,deflator_ols_dec,corp_debt_ols_dec, type='text')
# stargazer(infl_e_ols_dec,deflator_ols_dec,corp_debt_ols_dec,
#           type='latex', out="TABLES/ols_decomp_levels", label='tab:ols_dec_levels',
#           title='OLS decomposition of I(0) series' )


## remove drifts and trend and store in new list
i0_levels_no_drift = list()

ct_infl = coef(infl_e_ols_dec)[1]  
trend_infl = coef(infl_e_ols_dec)[2] 
m5_infl = coef(infl_e_ols_dec)[7]
i0_levels_no_drift$infl_e = i0_levels$infl_e - ct_infl - (df_i0level$trend*trend_infl) - (df_i0level$M5*m5_infl)

ct_defl = coef(deflator_ols_dec)[1]  
trend_defl = coef(deflator_ols_dec)[2]  
i0_levels_no_drift$deflator = i0_levels$deflator - ct_defl- (df_i0level$trend*trend_defl)

ct_corp = coef(corp_debt_ols_dec)[1]  
trend_corp = coef(corp_debt_ols_dec)[2]  
i0_levels_no_drift$corp_debt = i0_levels$corp_debt - ct_corp - (df_i0level$trend*trend_corp)




### Find p and q with PACF and ACF                         

Acf(i0_levels_no_drift$infl_e, lag.max = 20, type = "correlation", plot = TRUE, na.action = na.contiguous, demean = TRUE, title = "ACF Inflation Expectation")
Pacf(i0_levels_no_drift$infl_e, lag.max = 20, plot = TRUE, na.action = na.contiguous, demean = TRUE)

acf_infl = ggAcf(i0_levels_no_drift$infl_e, lag.max= 40) + labs(title = 'ACF - infl_e')
acf_defl= ggAcf(i0_levels_no_drift$deflator, lag.max= 40) + labs(title = 'ACF - deflator')
acf_corp = ggAcf(i0_levels_no_drift$corp_debt, lag.max= 70) + labs(title = 'ACF - corp_debt')

ggsave('IMAGES/acf_infl.png', plot=acf_infl, width = 12, height = 8)
ggsave('IMAGES/acf_defl.png', plot=acf_defl, width = 12, height = 8)
ggsave('IMAGES/acf_corp.png', plot=acf_corp, width = 12, height = 8)

pacf_infl = ggPacf(i0_levels_no_drift$infl_e)+ labs(title = 'PACF - infl_e')
pacf_defl = ggPacf(i0_levels_no_drift$deflator) + labs(title = 'PACF - deflator')
pacf_corp = ggPacf(i0_levels_no_drift$corp_debt) + labs(title = 'PACF - corp_debt')

ggsave('IMAGES/pacf_infl.png', plot= pacf_infl, width = 12, height=8)
ggsave('IMAGES/pacf_defl.png', plot= pacf_defl, width = 12, height=8)
ggsave('IMAGES/pacf_corp.png', plot= pacf_corp, width = 12, height=8)



## create a function to get best combination of parameters 

find_best_ARMA = function(ts_data, max_ar_order, max_ma_order) {
  best_model_aic = NULL
  best_model_bic = NULL
  best_aic = Inf
  best_bic = Inf
  for (p in 0:max_ar_order) {
    for (q in 0:max_ma_order) {
      # Skip models where both AR and MA orders are 0
      if (p == 0 & q == 0) next
      # Fit ARMA model
      arma_model = arima(ts_data, order = c(p, 0, q))
      # Calculate AIC and BIC
      aic = AIC(arma_model)
      bic = BIC(arma_model)
      # Update best model based on AIC
      if (aic < best_aic) {
        best_aic = aic
        best_model_aic = arma_model
      }
      # # Update best model based on BIC
      # if (bic < best_bic) {
      #   best_bic = bic
      #   best_model_bic = arma_model
      # }
    }
  }
  
  # Return best models based on AIC and BIC
  return(list(best_model_aic = best_model_aic))#, best_model_bic = best_model_bic))
}


#use it 
best_infl = find_best_ARMA(i0_levels_no_drift$infl_e, 13, 15)
print("Best model based on AIC:")
print(best_infl$best_model_aic)

print("Best model based on BIC:")
print(best_infl$best_model_bic)








#################################
#3. Decompositions in deltas 
#################################
# Same idea for series in deltas 

decompositions_d = list() #store the decompositions 
dec_graphs_d= list() #store the decomposition graphs 
deseasonalized_ts_d = list() #store the deseasonalized ts, if seasonality ends up being important 
for (ts in 1:length(deltas)) {
  #print(deltas[ts])}
  decomp = stl(deltas[[ts]], s.window='periodic') #additive seasonality seems right from graphs
  graph = autoplot(decomp) + labs(title = names(deltas)[ts])
  deseason = deltas[[ts]] - decomp$time.series[,'seasonal']
  decompositions_d[[length(decompositions_d)+1]] = decomp
  dec_graphs_d[[length(dec_graphs_d)+1]] = graph
  deseasonalized_ts_d[[length(deseasonalized_ts_d)+1]] = deseason
}
names(deseasonalized_ts_d) = names(deltas) #name the series in deseasonalized_ts
names(decompositions_d) = names(deltas) #name the series in decompositions
rm(graph, decomp, deseason)

#Generate and export plots with the decomposition graphs 
decom_ii = grid.arrange(dec_graphs_d[[1]],dec_graphs_d[[2]], dec_graphs_d[[3]],dec_graphs_d[[4]], ncol=4)
# ggsave('IMAGES/decomposition_ii.png', plot=decom_ii, width = 12, height = 8)



### Run OLS regression on each component of the TS
#create a df with all the series and the monthly dummies
start_date = as.Date("1990-02-01")
end_date = as.Date("2022-12-01") 
all_dates = seq(start_date, end_date, by = "month")

df_deltas = data.frame(Date = all_dates)
for (i in seq_along(deltas)) {
  col_name = paste0("Series_", i)
  df_deltas[[col_name]] = deltas[[i]]
}
colnames(df_deltas) = c('Date', names(deltas))
rownames(df_deltas) = df_deltas$Date #date as index
df_deltas$MONTH=month(df_deltas$Date) #Get month dummies 
df_deltas[paste0("M", 1:12)] = as.data.frame(t(sapply(df_deltas$MONTH, tabulate, 12)))

df_deltas$Date = NULL #no more need date column
df_deltas$MONTH = NULL #no more need MONTH column

df_deltas$trend = seq_along(df_deltas$d_gdp) #deterministic trend

# Run OLS regression
d_gdp_ols_dec = lm(d_gdp ~ trend + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = df_deltas)
d_dpi_ols_dec = lm(d_dpi ~ trend + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = df_deltas)
d_rate_ols_dec = lm(d_rate ~ trend + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = df_deltas)
d_splong_ols_dec = lm(d_splong ~ trend + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10 + M11, data = df_deltas)

stargazer(d_gdp_ols_dec,d_dpi_ols_dec,d_rate_ols_dec, d_splong_ols_dec, type='text')
# stargazer(d_gdp_ols_dec,d_dpi_ols_dec,d_rate_ols_dec, d_splong_ols_dec,
#           type='latex', out="TABLES/ols_decomp_deltas", label='tab:ols_dec_deltas',
#           title='OLS decomposition of the first difference of I(1) series' )

## remove drifts and store in new list (remove trend for rate only)
deltas_no_drift_trend = list()

ct_gdp = coef(d_gdp_ols_dec)[1]  
deltas_no_drift_trend$d_gdp = deltas$d_gdp - ct_gdp
ct_dpi = coef(d_dpi_ols_dec)[1]  
deltas_no_drift_trend$d_dpi = deltas$d_dpi - ct_dpi
ct_rate = coef(d_rate_ols_dec)[1]  
trend_rate = coef(d_rate_ols_dec)[2]
deltas_no_drift_trend$d_rate = deltas$d_rate - ct_rate - (df_deltas$trend * trend_rate)
ct_sp = coef(d_splong_ols_dec)[1] 
deltas_no_drift_trend$d_splong = deltas$d_splong - ct_sp










## COINTEGRATION
levels_var = data.frame(
  splong = allts$splong,
  rate = allts$rate,
  gdp = allts$gdp,
  dpi = allts$dpi) 

#levels_var$timestamp = time(deseasonalized_ts$deflator) 
names_level = c('splong', 'rate', 'gdp', 'dpi')

lag_order = VARselect(levels_var)
res = lag_order$criteria


johansen_test = ca.jo(levels_var, type='trace', ecdet='trend', K=10)
summary(johansen_test)
# r is rank of matrix == number of cointegration relationship.
#no cointegration
johansen_table = xtable(johansensum)
print(latex_table, file=file_path)



test = data.frame(
  i= allts$dpi, 
  ii = allts$splong, 
  iii = allts$gdp)

VARselect(test)$selection
summary(ca.jo(test, type='trace', ecdet='trend', K=20))


















