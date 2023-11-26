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
from eurostatapiclient import EurostatAPIClient

os.chdir('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/AP 1')

#Time frame of the study
start = datetime.datetime(2000, 1, 1)
end = datetime.datetime(2023, 11, 1)

#tickers 
tickers = ['DTE.DE', 'AIR.DE', 'DHL.DE', 'SIE.DE', 'DB1.DE', '1COV.DE', 'EOAN.DE', 'ADS.DE', 'DTG.DE', 'DBK.DE', 'HEI.DE', 'RWE.DE', 'BAS.DE', 'CON.DE', 'MTX.DE', 'ALV.DE', 'SHL.DE', 'BEI.DE', 'SY1.DE', 'FRE.DE', 'BAYN.DE', 'VOW3.DE', 'MRK.DE', 'BMW.DE', 'HNR1.DE', 'IFX.DE', 'P911.DE', 'VNA.DE', 'ZAL.DE', 'ENR.DE']

data = yf.download(tickers, start=start,
                end=end)
adjclose=data['Adj Close']


pd.core.common.is_list_like = pd.api.types.is_list_like

SP500 = web.DataReader(['sp500'], 'fred', start, end)
SP500 = SP500.asfreq('W-FRI', method='pad')
print(SP500)

BTC_week = pd.read_csv('BTC-USD.csv')
BTC_day = pd.read_csv('BTC-USD (1).csv')

