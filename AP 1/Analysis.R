#%% AP 1 
#%% ncardenasfrias

pacman::p_load(data.table, tidyverse, gplots, xts, stargazer)

setwd('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/AP 1')

df <- fread("DATA_month.csv")
as.data.table(df)

typeof(df)
colnames(df)

## Identify the risk factors
#Need to remove the predicatable part to the endogeneous and exogeneous series 


## Estimate the beta coefficients 


## Estimate the lamdas 


#Test the validity of the multi-beta relationship