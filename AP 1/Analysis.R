#%% AP 1 
#%% ncardenasfrias

pacman::p_load(data.table, urca, tidyverse, gplots, xts, stargazer, forecast, plm, ggplot2, tidyr)
library(dplyr)
setwd('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/AP 1')

df <- fread("DATA_month.csv")
as.data.table(df)

############################
## Identify the risk factors
############################

#Need to remove the predicatable part to the endogeneous and exogeneous series 

  #upload the monthly data with the yearly factorsand transform into a list of TS
data = read.csv('Monthly_series.csv')
data$Date <- as.Date(data$Date)

filtered_data <- data %>% #Filter rows between 2005 and 2022
  filter(Date >= as.Date("2005-01-01") & Date <= as.Date("2022-12-31"))
time_series_cols <- filtered_data %>%
  select(-Date)
time_series_list <- lapply(time_series_cols, function(col) {
  ts_values <- ts(col, start = c(year(min(filtered_data$Date)), month(min(filtered_data$Date))), frequency = 12)
  return(ts_values)
})

names(time_series_list) <- names(time_series_cols)

  #plot series
time_series_df <- as.data.frame(time_series_list)
time_series_df$Date <- time(time_series_list[[1]])
time_series_long <- pivot_longer(time_series_df, cols = -Date, names_to = "Series", values_to = "Value")
ggplot(time_series_long, aes(x = Date, y = Value, color = Series)) +
  geom_line() +
  facet_wrap(~ Series, scales = "free_y") +
  labs(x = "Time", y = "Value")

  ##check stationarity 

lv = filtered_data[,c("EuroUSD","CAC40", "infl","pib","Breakeven","rkfreeAAA", 'oat')]

lv.adf.ln.trend = list(
  XR = ur.df(lv$EuroUSD, type='trend', selectlags = c('BIC')),
  cac = ur.df(lv$CAC40, type='trend', selectlags = c('BIC')),
  infl = ur.df(lv$infl, type='trend', selectlags = c('BIC')),
  pib = ur.df(lv$pib, type='trend', selectlags = c('BIC')),
  #breakeven = ur.df(lv$Breakeven_no_na, type='trend', selectlags = c('BIC')),
  rfAAA = ur.df(lv$rkfreeAAA, type='trend', selectlags = c('BIC')),
  oat = ur.df(lv$oat, type='trend', selectlags = c('BIC'))
)
 
summary(lv.adf.ln.drift$XR)
print("levelVariable with drift and trend")
test =cbind(t(lv.adf.ln.trend$XR@teststat),t(lv.adf.ln.trend$cac@teststat), 
            t(lv.adf.ln.trend$infl@teststat),t(lv.adf.ln.trend$pib@teststat),
            t(lv.adf.ln.trend$rfAAA@teststat), t(lv.adf.ln.trend$oat@teststat),
            lv.adf.ln.trend$XR@cval)
#stargazer(test, out ='Tables/trend_macro.tex')

lv.adf.ln.drift = list(
  XR = ur.df(lv$EuroUSD, type='drift', selectlags = c('BIC')),
  cac = ur.df(lv$CAC40, type='drift', selectlags = c('BIC')),
  infl = ur.df(lv$infl, type='drift', selectlags = c('BIC')),
  pib = ur.df(lv$pib, type='drift', selectlags = c('BIC')),
  #breakeven = ur.df(lv$Breakeven_no_na, type='drift', selectlags = c('BIC')),
  rfAAA = ur.df(lv$rkfreeAAA, type='drift', selectlags = c('BIC')),
  oat = ur.df(lv$oat, type='drift', selectlags = c('BIC'))
)
print("levelVariable with drift ")
stat_macro_drift =cbind(t(lv.adf.ln.drift$XR@teststat),t(lv.adf.ln.drift$cac@teststat), 
            t(lv.adf.ln.drift$infl@teststat),t(lv.adf.ln.drift$pib@teststat),
            t(lv.adf.ln.drift$rfAAA@teststat), t(lv.adf.ln.drift$oat@teststat),
            lv.adf.ln.drift$XR@cval)
#stargazer(stat_macro_drift, out='Tables/drift_macro.tex')

lv.adf.ln.none = list(
  XR = ur.df(lv$EuroUSD, type='none', selectlags = c('BIC')),
  cac = ur.df(lv$CAC40, type='none', selectlags = c('BIC')),
  infl = ur.df(lv$infl, type='none', selectlags = c('BIC')),
  pib = ur.df(lv$pib, type='none', selectlags = c('BIC')),
  #breakeven = ur.df(lv$Breakeven_no_na, type='none', selectlags = c('BIC')),
  rfAAA = ur.df(lv$rkfreeAAA, type='none', selectlags = c('BIC')),
  oat = ur.df(lv$oat, type='none', selectlags = c('BIC'))
)
print("levelVariable with none ")
stat_macro_none =cbind(t(lv.adf.ln.none$XR@teststat),t(lv.adf.ln.none$cac@teststat), 
                        t(lv.adf.ln.none$infl@teststat),t(lv.adf.ln.none$pib@teststat),
                        t(lv.adf.ln.none$rfAAA@teststat), t(lv.adf.ln.none$oat@teststat),
                        lv.adf.ln.none$XR@cval)
#stargazer(stat_macro_none, out='Tables/drift_none.tex')




#differenciate 
filtered_data$logGDP = log(filtered_data$pib)

filtered_data$DlogGDP <- c(NA, diff(filtered_data$logGDP))
filtered_data$DCAC40 <- c(NA, diff(filtered_data$CAC40))
filtered_data$DEuroUSD <- c(NA, diff(filtered_data$EuroUSD))
filtered_data$Dinfl <- c(NA, diff(filtered_data$infl))
filtered_data$Doat <- c(NA, diff(filtered_data$oat))
filtered_data$DrfAAA <- c(NA, diff(filtered_data$rkfreeAAA))

time_series_cols <- filtered_data %>%
  select(-Date)
time_series_list <- lapply(time_series_cols, function(col) {
  ts_values <- ts(col, start = c(year(min(filtered_data$Date)), month(min(filtered_data$Date))), frequency = 12)
  return(ts_values)
})

names(time_series_list) <- names(time_series_cols)

#plot series
time_series_df <- as.data.frame(time_series_list)
time_series_df$Date <- time(time_series_list[[1]])
time_series_long <- pivot_longer(time_series_df, cols = -Date, names_to = "Series", values_to = "Value")
ggplot(time_series_long, aes(x = Date, y = Value, color = Series)) +
  geom_line() +
  facet_wrap(~ Series, scales = "free_y") +
  labs(x = "Time", y = "Value")

# Auto arima differenciates as much as needed to get stationary variables 

logGDP_arima = auto.arima(filtered_data$logGDP) #ARIMA 0,1,0
logGDP_arima
EuroUSD_arima = auto.arima(filtered_data$EuroUSD) #ARIMA 0,1,1
EuroUSD_arima
infl_arima = auto.arima(filtered_data$infl) #ARIMA 1,1,1
infl_arima
breakeven_arima = auto.arima(filtered_data$Breakeven) #ARIMA 2,1,2
breakeven_arima 

timestamps <- filtered_data$Date
res_pib = resid(logGDP_arima)
res_xr = resid(EuroUSD_arima)
res_infl = resid(infl_arima)
res_bkeven = resid(breakeven_arima)
min_length <- min(length(res_pib), length(res_xr), length(res_infl))
# Create a dataframe with aligned timestamps and residuals padded with NA for breakeven
residuals_df <- data.frame(
  Date = timestamps[1:min_length],
  res_pib = c(res_pib[1:min_length], rep(NA, times = max(0, length(timestamps) - min_length))),
  res_xr = c(res_xr[1:min_length], rep(NA, times = max(0, length(timestamps) - min_length))),
  res_infl = c(res_infl[1:min_length], rep(NA, times = max(0, length(timestamps) - min_length))),
  res_bkeven = c(res_bkeven[1:min_length], rep(NA, times = max(0, length(timestamps) - min_length)))
)



#################################
## Estimate the beta coefficients 
#################################

data_firm = read.csv('Firm_monthly.csv')
data_firm$Date <- as.Date(data_firm$Date)

filtered_data_firm <- data_firm %>% #Filter rows between 2005 and 2022
  filter(Date >= as.Date("2005-01-01") & Date <= as.Date("2022-12-31"))
time_series_cols_firm <- filtered_data_firm %>%
  select(-Date)
time_series_list_firm <- lapply(time_series_cols_firm, function(col) {
  ts_values_firm <- ts(col, start = c(year(min(filtered_data_firm$Date)), month(min(filtered_data_firm$Date))), frequency = 12)
  return(ts_values_firm)
})

names(time_series_list_firm) <- names(time_series_cols_firm)

## to get return, differenciate value of the stock (within each company)
filtered_data_firm <- filtered_data_firm[order(filtered_data_firm$Company, filtered_data_firm$Date), ]
filtered_data_firm$Return <- with(filtered_data_firm, ave(value, Company, FUN = function(x) c(NA, diff(x))))

finalmonthly =merge(filtered_data_firm, filtered_data, by = "Date")
finalmonthly =merge(finalmonthly, residuals_df, by = "Date")


dta_bystock = split(finalmonthly, finalmonthly$Company)

fit_lm <- function(data) {
  lm(Return ~ res_pib + res_xr + res_infl, data = data)
}
regs_beta <- lapply(dta_bystock, function(subset) fit_lm(subset))

summary(regs_beta)
stargazer(regs_beta,
          title = "Estimate the beta coefficients for each exogeneous factor",
          column.labels = names(regs_beta),
          out="Tables/betas_exo.tex")


#Include endofactor -> start in 2013 
fit_lm_endo <- function(data) {
  lm(Return ~ res_pib + res_xr + res_infl+ res_bkeven, data = data)
}
regs_beta_endo <- lapply(dta_bystock, function(subset) fit_lm_endo(subset))
stargazer(regs_beta_endo,
          title = "Estimate the beta coefficients for exogeneous and endogeneous factors (2013-2022)",
          column.labels = names(regs_beta),
          out="Tables/betas_exo_endo.tex")

#Include FF factors
fit_lm_exoff <- function(data) {
  lm(Return ~ res_pib + res_xr + res_infl+ HML+SMB+Mkt.RF, data = data)
}
regs_beta_exoff <- lapply(dta_bystock, function(subset) fit_lm_exoff(subset))

summary(regs_beta_exoff)
stargazer(regs_beta_exoff,
          title = "Estimate the beta coefficients for each exogeneous factor and French and Fama factors",
          column.labels = names(regs_beta),
          out="Tables/betas_exo_ff.tex")


#### FF model with annual data and our estimation for this sample 
Firm_yearly <- read_csv("Firm_yearly.csv")
Firm_yearly = merge(Firm_yearly, 
                    Firm_yearly%>%
                      filter(!is.na(MarketCap))%>%
                      group_by(Date)%>%
                      dplyr::summarise(MedianMCt=median(MarketCap)),
                    by="Date")
DiffMC = merge(
  Firm_yearly%>%
    filter(!is.na(MarketCap) & MarketCap>MedianMCt & !is.na(value))%>%
    group_by(Date)%>%
    dplyr::summarise(AvgTop = mean(value)),
  Firm_yearly%>%
    filter(!is.na(MarketCap) & MarketCap<MedianMCt & !is.na(value))%>%
    group_by(Date)%>%
    dplyr::summarise(AvgBot = mean(value)),
  by = "Date")
DiffMC$SMBest = DiffMC$AvgTop-DiffMC$AvgBot

Firm_yearly = merge(Firm_yearly, 
                    Firm_yearly%>%
                      filter(!is.na(BookMarket))%>%
                      group_by(Date)%>%
                      dplyr::summarise(MedianBMt=median(BookMarket)),
                    by="Date")
DiffBM = merge(
  Firm_yearly%>%
    filter(!is.na(BookMarket) & BookMarket>MedianBMt & !is.na(value))%>%
    group_by(Date)%>%
    dplyr::summarise(AvgTop = mean(value)),
  Firm_yearly%>%
    filter(!is.na(BookMarket) & BookMarket<MedianBMt & !is.na(value))%>%
    group_by(Date)%>%
    dplyr::summarise(AvgBot = mean(value)),
  by = "Date")
DiffBM$HMLest = DiffBM$AvgTop-DiffBM$AvgBot

Firm_yearly =merge(Firm_yearly, DiffBM, by = "Date")
Firm_yearly =merge(Firm_yearly, DiffMC, by = "Date")

Data_year = read_csv("DATA_yearly.csv")
Firm_yearly =merge(Firm_yearly, Data_year, by = "Date")
Firm_yearly$Date = as.Date(Firm_yearly$Date)

Firm_yearly <- Firm_yearly[order(Firm_yearly$Company.x, Firm_yearly$Date), ]
Firm_yearly$Return <- with(Firm_yearly, ave(value.x, Company.x, FUN = function(x) c(NA, diff(x))))

Firm_yearly$DCAC40 = c(NA, diff(Firm_yearly$CAC40))
Firm_yearly$Doat = c(NA, diff(Firm_yearly$oat))
Firm_yearly$mkt_free = Firm_yearly$DCAC40 - Firm_yearly$Doat

filtered_Firm_yearly <- Firm_yearly %>% #Filter rows between 2010 and 2022 (no missing values)
  filter(Date >= as.Date("2010-01-01") & Date <= as.Date("2022-12-31"))

fit_lm_myFFonly <- function(data) {
  lm(Return ~ mkt_free+HMLest+SMBest, data = data)
}
year_by_stock_FF = split(filtered_Firm_yearly, filtered_Firm_yearly$Company.x)
regs_beta_myFF <- lapply(year_by_stock_FF, function(subset) fit_lm_myFFonly(subset))

stargazer(regs_beta_myFF,
          title = "Estimate the beta coefficients for computed French and Fama factors (2010-2022)",
          column.labels = names(regs_beta_myFF),
          out="Tables/betas_myff.tex")


#######################
## Estimate the lambdas
#######################


#Extarct the beta coefficients of reg with exo and FF factors 
# Initialize an empty dataframe to store coefficients and model names
coefficients_df <- data.frame(Model = character(), Intercept = numeric(),
                              res_pib = numeric(), res_xr = numeric(),
                              res_infl = numeric(), HML = numeric(),
                              SMB = numeric(), Mkt.RF = numeric(),
                              stringsAsFactors = FALSE)

# Iterate through subsets of data and fit models
for (i in seq_along(dta_bystock)) {
  subset <- dta_bystock[[i]]
  model <- fit_lm_exoff(subset)
  coefficients <- c(model$coefficients[1], model$coefficients[-1])
  row <- data.frame(Model = paste0("Model_", i),
                    Intercept = coefficients[1],
                    res_pib = coefficients[2],
                    res_xr = coefficients[3],
                    res_infl = coefficients[4],
                    HML = coefficients[5],
                    SMB = coefficients[6],
                    Mkt.RF = coefficients[7])
  coefficients_df <- rbind(coefficients_df, row)
}

coefficients_df
coefficients_df$Company = names(regs_beta_exoff) #added column with company name 



#Get the historical mean return of every stock

historical_mean_returns <- finalmonthly %>%
  group_by(Company) %>%
  summarise(mean_return = mean(Return, na.rm = TRUE))

# View the resulting dataframe with mean returns by company
print(historical_mean_returns)


#merge two data sets 
multibeta = merge(coefficients_df, historical_mean_returns, by="Company")








#################################################
#Test the validity of the multi-beta relationship
#################################################