#%% AP 1 
#%% ncardenasfrias

pacman::p_load(data.table, tidyverse, gplots, xts, stargazer, plm)

setwd('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/AP 1')

df <- fread("DATA_month.csv")
as.data.table(df)


## Identify the risk factors
#Need to remove the predicatable part to the endogeneous and exogeneous series 

  #upload the monthly data with the yearly factors  and transform into a list of TS
data = read.csv('Monthly_series.csv')
data$Date <- as.Date(data$Date)

# Filter rows between 2005 and 2022
filtered_data <- data %>%
  filter(Date >= as.Date("2005-01-01") & Date <= as.Date("2022-12-31"))

time_series_cols <- filtered_data %>%
  select(-Date)

time_series_list <- lapply(time_series_cols, function(col) {
  ts_values <- ts(col, start = c(year(min(filtered_data$Date)), month(min(filtered_data$Date))), frequency = 12)
  return(ts_values)
})

names(time_series_list) <- names(time_series_cols)




## Estimate the beta coefficients 


## Estimate the lamdas 


#Test the validity of the multi-beta relationship