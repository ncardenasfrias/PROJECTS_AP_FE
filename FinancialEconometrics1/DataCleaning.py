#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Financial Econometrics - Empirical Applications d
Data Gathering and Data Cleaning 

@author: nataliacardenasf
"""

import pandas as pd
import os 
import datetime

from fredapi import Fred 

os.chdir('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/FinancialEconometrics1')

### Initialize FRED API 
fred = Fred(api_key='23edc2b1b61e17c07b83a97e7abfc02b')

### Import all the data

# S&P 500 
sp500 = pd.DataFrame(fred.get_series('SP500')) #daily close, NSA, Index
sp500.columns= ['sp500']

#Inflation expectations from survey UMich
infl_e = pd.DataFrame(fred.get_series('MICH')) #monthly, NSA, median expected in % over next 12 mo
infl_e.columns= ['infl_e']


#ICE BofA US Corporate Index Total Return Index
corp_debt = pd.DataFrame(fred.get_series('BAMLCC0A0CMTRIV')) #daily, close, NSA, Index  
corp_debt.columns= ['corp_debt']


#MP rate 
rate = pd.DataFrame(fred.get_series('DFF')) #daily, 7-Day, NSA, %
rate.columns = ['rate']

#Deflator
deflator = pd.DataFrame(fred.get_series('A191RI1Q225SBEA')) #Q, SA Annual Rate
deflator.columns = ['deflator']

#Unemployment 
unempl = pd.DataFrame(fred.get_series('UNRATENSA')) #monthly, NSA, %
unempl.columns=['unempl']

# GDP
gdp = pd.DataFrame(fred.get_series('GDP'))  # quarterly, Billions of Dollars, SA Annual Rate
gdp.columns = ['gdp']

# RPI (Real Personal Income)
rpi = pd.DataFrame(fred.get_series('RPI'))  # monthly, SA rate, deflated
rpi.columns = ['rpi']

# Real Personal Disposable Income
dpi = pd.DataFrame(fred.get_series('DSPIC96'))  # monthly, SA annaul rate, chained 2017 USD
dpi.columns = ['dpi']

# Manufacturing Sector
manufacturing = pd.DataFrame(fred.get_series('MPU9900063'))  # annual, NSA => avoid this 
manufacturing.columns = ['manufacturing']

fred.search("MPU9900063").T #this function gives of the info on every series


### Resample into monthly data 
sp500 = sp500.resample('1M').mean(numeric_only=True)
infl_e = infl_e.resample('1M').mean(numeric_only=True)
corp_debt = corp_debt.resample('1M').mean(numeric_only=True)
rate = rate.resample('1M').mean(numeric_only=True)
deflator = deflator.resample('1M').mean(numeric_only=True)
unempl = unempl.resample('1M').mean(numeric_only=True)
gdp = gdp.resample('1M').mean(numeric_only=True)
rpi = rpi.resample('1M').mean(numeric_only=True)
dpi = dpi.resample('1M').mean(numeric_only=True)
manufacturing = manufacturing.resample('1M').mean(numeric_only=True)

dta = [infl_e, rate, sp500, corp_debt, deflator, unempl, gdp, rpi, dpi, manufacturing]

### Slice the df to relevant period 
#Find common time span
min_date = max([min(i.index) for i in dta])
max_date = min([max(i.index) for i in dta])
print(min_date, max_date)

#Let us work on monthly data for the 1990-2022 period
start = datetime.datetime(1990,1,1)
end= datetime.datetime(2022,12,31)

## SP500 series is too short, I am taking it from Yahoo Finance 
import yfinance as yf
splong = yf.download('^GSPC', start=start,end=end)['Adj Close'].resample('M').mean(numeric_only=True)
splong = pd.DataFrame(splong)
type(splong)
splong.rename(columns={"Adj Close":'splong'}, inplace=True)

##Get a single DF
dta.append(splong)
for i in range(len(dta)): #we had some indexes at end of month, others at 1st of month: harmonize to 1st each month
    df = dta[i]
    df.index = [pd.datetime(x.year, x.month, 1) for x in df.index.tolist()]
    dta[i] = df.loc[start:end,:]
dta# we're good now
#merge into 1 df, 1 series per column
monthly = pd.concat(dta, axis=1)
#interpolate missing months for deflatior data (Q): uses midpoints ie assumes that each month in the quarter contributes in the same fashion to the increase QoQ
m1 = monthly.interpolate(method ='linear', limit_direction ='forward')


m1.to_csv("DATA/data.csv")
