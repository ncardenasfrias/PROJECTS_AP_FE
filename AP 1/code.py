#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  2 21:18:53 2023

@author: nataliacardenasf
"""

import pandas_datareader.data as web
import yfinance as yf
import datetime
import pandas as pd
import os 

os.chdir('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/AP 1')

tickers = ['DTE.DE']
data = yf.download(tickers, start='2013-01-01',
                end='2023-11-01')
adjclose=data['Adj Close']


pd.core.common.is_list_like = pd.api.types.is_list_like
start = datetime.datetime(2014, 9, 17)
end = datetime.datetime(2023, 10, 31)

SP500 = web.DataReader(['sp500'], 'fred', start, end)
SP500 = SP500.asfreq('W-FRI', method='pad')
print(SP500)

BTC_week = pd.read_csv('BTC-USD.csv')
BTC_day = pd.read_csv('BTC-USD (1).csv')

