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

























