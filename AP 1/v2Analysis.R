##%% FE 1 v2
##%% @ncardenasfrias

# Load necessary packages and set personal path to documents
pacman::p_load(data.table, dplyr, bootUR, urca, gridExtra, tidyverse, gplots, xts, stargazer, forecast, ggplot2, vars)

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
rm(desired_order)


#Remove outliers 
tsclean(allts$infl_e, iterate = 2, lambda =NULL)
tsclean(allts$deflator, iterate = 2, lambda =NULL)
tsclean(allts$unempl, iterate = 2, lambda =NULL)
tsclean(allts$rate, iterate = 2, lambda =NULL)
tsclean(allts$splong, iterate = 2, lambda =NULL)
tsclean(allts$corp_debt, iterate = 2, lambda =NULL)



###########################
#1. Dynamics
###########################

#####
#1.1. Series decomposition using the stl function

decompositions = list() #store the decompositions 
dec_graphs= list() #store the decomposition graphs 
deseasonalized_ts = list() #store the deseasonalized ts 
for (ts in 1:length(allts)) {
  #print(allts[ts])}
  decomp = stl(allts[[ts]], s.window='periodic') #additive seasonality seems right from graphs
  graph = autoplot(decomp) + labs(title = names[ts])
  deseason = allts[[ts]] - decomp$time.series[,'seasonal']
  decompositions[[length(decompositions)+1]] = decomp
  dec_graphs[[length(dec_graphs)+1]] = graph
  deseasonalized_ts[[length(deseasonalized_ts)+1]] = deseason
}
names(deseasonalized_ts) = names(allts) #name the series in deseasonalized_ts
names(decompositions) = names(allts) #name the series in decompositions


#Generate and export plots with the decomposition graphs 
decom_i = grid.arrange(dec_graphs[[1]],dec_graphs[[2]], dec_graphs[[3]], ncol=3)
ggsave('IMAGES/decomposition_i.png', plot=decom_i, width = 12, height = 8)

decom_ii = grid.arrange(dec_graphs[[4]],dec_graphs[[5]], dec_graphs[[6]], ncol=3)
ggsave('IMAGES/decomposition_ii.png', plot=decom_ii, width = 12, height = 8)

rm(decom_i, decom_ii, decomp, dec_graphs, df, graph, deseason, numeric_cols, col) #clean around



#####
#1.2 WORK ON DESEASONALIZED SERIES 
#they are all stored in deseasonalized_ts

#Estimate the seasonal coefficients 
seasonal_table <- data.frame(matrix(ncol = 13, nrow = length(decompositions)))
colnames(seasonal_table) <- c("Series", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                              "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
for (i in 1:length(decompositions)) {
  series_name = names(decompositions)[i]  
  seasonal_coef = decompositions[[i]]$time.series[,'seasonal']
  seasonal_coef_rounded <- round(seasonal_coef, 3)
  row_values <- c(series_name, seasonal_coef_rounded)
  seasonal_table[i, ] <- row_values
}
rownames(seasonal_table) = seasonal_table$Series #series as index, remove the Series column 
seasonal_table$Series = NULL

#stargazer(seasonal_table, out='TABLES/seasons.tex', summary=FALSE, label="tab:seasons", title="Estimation of the seasonality of each series")
#need to manually add a resize box on the latex code



#Get integration order
## Need to properly write the ADF test, but these commands summarize what I need for now, ie the order of integration
order_integration(deseasonalized_ts$infl_e, method = "adf")$order_int # I(0)
order_integration(deseasonalized_ts$deflator, method = "adf")$order_int # I(0)
order_integration(deseasonalized_ts$unempl, method = "adf")$order_int # I(1)
order_integration(deseasonalized_ts$rate, method = "adf")$order_int # I(1)
order_integration(deseasonalized_ts$splong, method = "adf")$order_int # I(1)
order_integration(deseasonalized_ts$corp_debt, method = "adf")$order_int # I(1)

#I made a list with the deseasonalized, I(0) series 
i0_deseason_series= list(deseasonalized_ts$infl_e, diff(deseasonalized_ts$deflator), 
                      diff(deseasonalized_ts$unempl), diff(deseasonalized_ts$rate),
                      diff(deseasonalized_ts$splong), diff(deseasonalized_ts$corp_debt))
names(i0_deseason_series) = c('infl_e', 'd_deflator', 'd_unempl', 'd_rate', 'd_splong', 'd_corp_debt')

#For the sake of it, add the series in deltas to the deseasonalized_ts list 
deseasonalized_ts = c(deseasonalized_ts, list(diff(deseasonalized_ts$deflator),
                      diff(deseasonalized_ts$unempl), diff(deseasonalized_ts$rate),
                      diff(deseasonalized_ts$splong), diff(deseasonalized_ts$corp_debt)))
names(deseasonalized_ts) = c(names(allts),list('d_deflator','d_unempl', 'd_rate', 'd_splong', 'd_corp_debt'))
  

#double check that deltas are I(0), we're good to go
order_integration(i0_deseason_series$d_unempl, method='adf')$order_int #I(0)
order_integration(i0_deseason_series$d_rate, method='adf')$order_int #I(0)
order_integration(i0_deseason_series$d_splong, method='adf')$order_int #I(0)
order_integration(i0_deseason_series$d_corp_debt, method='adf')$order_int #I(0)


auto.arima(i0_deseason_series$infl_e) #ARIMA(4,0,0)(1,0,0)
auto.arima(i0_deseason_series$d_deflator) #ARIMA (3,0,2)(2,0,1)
auto.arima(i0_deseason_series$d_unempl) #ARIMA(0,0,0)
auto.arima(i0_deseason_series$d_rate) #ARIMA(2,0,2)(1,0,0)
auto.arima(i0_deseason_series$d_splong) #ARIMA(1,0,0)(1,0,0)
auto.arima(i0_deseason_series$d_corp_debt) #ARIMA(1,1,1) ????????


acf_plots = list()
pacf_plots = list()
# Loop through each time series to generate its ACF plot
for (i in 1:length(i0_deseason_series)) {
  a=ggAcf(i0_deseason_series[[i]], lag.max=20)
  p=ggPacf(i0_deseason_series[[i]], lag.max=20)
  acf_plots[[i]] = plot(a) + labs(title = names(i0_deseason_series)[i])
  pacf_plots[[i]] = plot(p) + labs(title = names(i0_deseason_series)[i])}
acf = grid.arrange(grobs = acf_plots, ncol = 2) 
pacf = grid.arrange(grobs = pacf_plots, ncol=2)



#####
#1.3. WORK ON I(0) SERIES, NOT DESEASONALIZED, works less well
#they are all stored in allts

# ## Need to properly write the ADF test, but these commands summarize what I need for now, ie the order of integration
# order_integration(allts$infl_e, method = "adf")$order_int # I(0)
# order_integration(allts$deflator, method = "adf")$order_int # I(0)
# order_integration(allts$unempl, method = "adf")$order_int # I(1)
# order_integration(allts$rate, method = "adf")$order_int # I(0)
# order_integration(allts$splong, method = "adf")$order_int # I(1)
# order_integration(allts$corp_debt, method = "adf")$order_int # I(1)
# 
# i0_series= list(allts$infl_e, allts$deflator, 
#                       diff(allts$unempl), allts$rate,
#                       diff(allts$splong), diff(allts$corp_debt))
# names(i0_series) = c('infl_e', 'deflator', 'd_unempl', 'rate', 'd_splong', 'd_corp_debt')
# 
# #double check that deltas are I(0), we're good to go
# order_integration(i0_series$d_unempl, method='adf')$order_int #I(0)
# order_integration(i0_series$d_splong, method='adf')$order_int #I(0)
# order_integration(i0_series$d_corp_debt, method='adf')$order_int #I(0)
# 
# 
# auto.arima(i0_series$infl_e) #ARIMA(1,0,0)(1,0,0)
# auto.arima(i0_series$deflator) #ARIMA (2,1,1)(0,0,2)
# auto.arima(i0_series$d_unempl) #ARIMA(1,0,2)(0,0,2)
# auto.arima(i0_series$rate) #ARIMA(3,1,4)(2,0,0)
# auto.arima(i0_series$d_splong) #ARIMA(1,0,0)(1,0,0)
# auto.arima(i0_series$d_corp_debt) #ARIMA(0,1,2)
# 
# 
# # Loop through each time series to generate its ACF and PACF plot
# acf_plots = list()
# pacf_plots = list()
# for (i in 1:length(i0_series)) {
#   a=ggAcf(i0_series[[i]], lag.max=20)
#   p=ggPacf(i0_series[[i]], lag.max=20)
#   acf_plots[[i]] = plot(a) + labs(title = names(i0_series)[i])
#   pacf_plots[[i]] = plot(p) + labs(title = names(i0_series)[i])
# }
# acf_2 = grid.arrange(grobs = acf_plots, ncol = 2) 
# pacf_2 = grid.arrange(grobs = pacf_plots, ncol=2)




#####
#1.3. Check Cyclical component 
cyclical_comp=list()
for (ts in 1:length(decompositions)){
  cyclical_comp[[length(cyclical_comp)+1]] = decompositions[[ts]]$time.series[,'remainder']
} 
names(cyclical_comp) = names(decompositions)

auto.arima(cyclical_comp$infl_e)
auto.arima(cyclical_comp$deflator)
auto.arima(cyclical_comp$unempl)
auto.arima(cyclical_comp$rate)
auto.arima(cyclical_comp$splong)
auto.arima(cyclical_comp$corp_debt)



nsdiffs(cyclical_comp$infl_e,test="seas") #no more seasonality 
nsdiffs(cyclical_comp$deflator,test="seas") #no more seasonality 
nsdiffs(cyclical_comp$unempl,test="seas") #no more seasonality 
nsdiffs(cyclical_comp$rate,test="seas") #no more seasonality 
nsdiffs(cyclical_comp$splong,test="seas") #no more seasonality 
nsdiffs(cyclical_comp$corp_debt,test="seas") #no more seasonality 



#################
#2. VAR
#################

#Let us focus on deflator, unempl, and splong only to avoid having a VAR too big 
levels_var = data.frame(
  deflator = deseasonalized_ts$deflator,
  unepl = deseasonalized_ts$unempl,
  splong = deseasonalized_ts$splong)
#levels_var$timestamp <- time(deseasonalized_ts$deflator) 

deltas_var = data.frame(
  d_deflator = deseasonalized_ts$d_deflator,
  d_unepl = deseasonalized_ts$d_unempl,
  d_splong = deseasonalized_ts$d_splong)



#####
#2.1 Work in levels 

###Select the order of the VAR using VARselect
lag_order = VARselect(levels_var)
res = lag_order$criteria
crit_level = as.data.frame(res)
rownames(crit_level) = c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")

latex_varorder_lev= xtable(crit_level, caption = "Canonical VAR in levels - Identify order")
print(latex_varorder_lev, caption.placement = "top", include.rownames = TRUE, file = "TABLES/varorder_level.tex")

order_selec_level = lag_order$selection # let's go with 8 from AIC, HQ, FPE
print(order_selec_level)

###VAR model estimation
var_level = VAR(levels_var, p = 5)
sum_varlevel = summary(var_level)

#export
table_var_level = xtable(table_var_level$varresult)
print(latex_table, file = "TABLES/estim_var_level.tex", caption ="Level VAR - Estimation", floating = FALSE)


#####
#2.2 Work in deltas






