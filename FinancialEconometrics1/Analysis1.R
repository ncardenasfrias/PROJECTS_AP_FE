##%% FE 1 
##%% @ncardenasfrias

# Load necessary packages and set personal path to documents
pacman::p_load(data.table, dplyr, urca, tidyverse, gplots, xts, stargazer, forecast, plm, ggplot2, tidyr)

setwd('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/FinancialEconometrics1')

#Load the dataset, data gathering and cleaning done in Python
df <- fread("DATA/data.csv")
as.data.table(df)


###########################
#1. Dynamics
###########################

### 1.1. Series decomposition
### 1.2. UR test
### 1.3. Seasonal variations
### 1.3. Cyclical component



###########################
#2. VAR
###########################

### 2.1. Canonical VAR: levels 
### 2.2. Canonical VAR: deltas




###########################
#2. Cointegration
###########################

### 3.1. Cointegration rank/Johansen test 
### 3.1.1. Estimate cointegration relationship 

