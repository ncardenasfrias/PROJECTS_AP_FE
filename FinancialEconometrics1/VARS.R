##%% FE VARS

# Load necessary packages and set personal path to documents
pacman::p_load(data.table, xtable, tseries, reshape2, smoots, dplyr, bootUR, urca, gridExtra, tidyverse, gplots, xts, stargazer, forecast, ggplot2, vars)

setwd('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/FinancialEconometrics1')

############ EMPIRICAL ANALYSIS 2

# Selecting the three series for the VAR model

diff_var_data = data.frame(splong = deltas$d_splong, gdp =deltas$d_gdp, rate = deltas$d_rate)

# Identify the order of the VAR model
var_order <- VARselect(diff_var_data, lag.max = 10, type = "both")#$selection
res = var_order$criteria
crit_level = as.data.frame(res)
rownames(crit_level) = c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")
latex_varorder_lev= xtable(crit_level, caption = "Canonical VAR in levels - Identify order")
# print(latex_varorder_lev, caption.placement = "top", include.rownames = TRUE, file = "TABLES/varorder_level.tex")

print(var_order$selection)

# Estimate the VAR model
var_model1 <- VAR(diff_var_data, p = 7, type = "both")
sum_varlevel = summary(var_model1)$varresult

stargazer(var_model1[['varresult']], type='text')
# stargazer(var_model1[['varresult']], type='latex', column.labels = names_level,
#           out="TABLES/estim_var_level.tex", title="Level VAR - Estimation",
#           label='tab:est_var_level', no.space=TRUE, model.numbers=F, font.size='small',
#           keep.stat=c("adj.rsq","ser","f"))




######## EMPIRICAL APPLICATION 3 (after johansen no coint) 
#### same as before
# # Selecting the same I(1) components as in Empirical Analysis 2
# var_data2 <- data.frame(
#     splong = allts$splong, 
#     gdp = allts$gdp, 
#     rate = allts$rate
# )
# 
# # Differencing the data to achieve stationarity
# diff_var_data3 <- diff(var_data2, differences = 1)
# 
# # Identify the order of the VAR model
# var_order2 <- VARselect(diff_var_data2, lag.max = 10, type = "both")$selection
# 
# # Estimate the VAR model
# var_model2 <- VAR(diff_var_data2, p = var_order2, type = "both")
# summary(var_model3)

# Causality Tests for the modified model

# Causality test between GDP and Federal Reserve Rate
causality_gdp <- causality(var_model1, cause = "gdp")
print(causality_gdp$Granger)

# Causality test between SPLong and GDP
causality_splong <- causality(var_model1, cause = "splong")
print(causality_splong$Granger)

# Causality test between SPLong and Federal Reserve Rate
causality_rate <- causality(var_model1, cause = "rate")
print(causality_rate$Granger)







###### EMPIRICAL APPLICATION 4

# Impulse Response Analysis for var_model1 from Empirical Analysis 2 + its graph
irf_var_model1 <- irf(var_model1, n.ahead = 10, boot = TRUE, ci = 0.95, cumulative=T)
plot(irf_var_model1)


# Impulse Response Analysis for var_model2 from Empirical Application 3 + its graph
# irf_var_model2 <- irf(var_model2, n.ahead = 10, boot = TRUE, ci = 0.95)
# plot(irf_var_model2)
####statistical analysis

# Setting up the plotting area for side-by-side comparison
par(mfrow=c(2,1))

# Plotting IRF for VAR Model 1
plot(irf_var_model1, main = "Impulse Responses for VAR Model 1")

# Plotting IRF for VAR Model 2
plot(irf_var_model2, main = "Impulse Responses for VAR Model 2")

# Resetting the plotting layout
par(mfrow=c(1,1))
