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

#Plot the decomposed series in 2 groups, save the figures
decomp_i = grid.arrange(dec1, dec2, dec3, ncol=3)
ggsave("IMAGES/decomposition_i.png", plot = decomp_i, width =12, height=8)

decomp_ii = grid.arrange(dec4, dec5, dec6, ncol=3)
ggsave("IMAGES/decomposition_ii.png", plot = decomp_ii, width = 12, height = 8)



#####
#1.2. UR test

#MWE
#tinfl_e = ur.df(allts$infl_e, type='trend',selectlags=c('BIC')) 
#t = summary(tinfl_e)

##ChatGPT 1
results_adf_trend <- list()

# Perform ur.df tests for each variable and store the summaries
for (var_name in names(allts)) {
  result <- ur.df(allts[[var_name]], type = "trend", selectlags = "BIC")
  results_adf_trend[[var_name]] <- summary(result)
}


##try to get stargazer table
regression_tables <- lapply(names(results_adf_trend), function(var_name) {
  summary_obj <- results_adf_trend[[var_name]]
  # Extract necessary information (modify this part based on what you need)
  coef <- summary_obj@testreg$coefficients
  Fstat <- summary_obj@testreg$fstatistic
  combined_info <- cbind(Series = rep(var_name, nrow(coef)), coef, Fstat)  # Adjust as needed
  return(combined_info)
})

combined_table <- do.call(rbind, regression_tables)

combined_table <- as.data.frame(combined_table)

combined_table$Estimate = round(as.numeric(combined_table$Estimate),2)
combined_table$`Std. Error` = round(as.numeric(combined_table$`Std. Error`), 2)
combined_table$`t value`= round(as.numeric(combined_table$`t value`),2)
combined_table$`Pr(>|t|)` = round(as.numeric(combined_table$`Pr(>|t|)`),2)
combined_table$Fstat = round(as.numeric(combined_table$Fstat),2)




# Export the table using stargazer
stargazer(combined_table, type = 'html', out = 'test2.html', summary = FALSE)
  
  

# Use stargazer with the combined information
stargazer::stargazer(
  regression_tables,
  type = "text",  # Change to "latex" for LaTeX output
  title = "Unit Root Test Results",
  align = TRUE
)





















#####
#1.3. Seasonal variations


### 1.3. Cyclical component



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


##### 
#3.1.1. Estimate cointegration relationship 

