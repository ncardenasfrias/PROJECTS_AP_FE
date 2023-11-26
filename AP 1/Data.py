#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
AP1 - Data gathering and Data Cleaning

Scrapping - Firm level data for French and Fama factors
Use Yahoo Finance API to get the financial data for all the stocks 
Use Eurostat to get the macro data 

Merge and clean the dataset

@author: nataliacardenasf
"""

import pandas as pd
import os

import requests
from bs4 import BeautifulSoup

import pandas_datareader.data as web
import yfinance as yf
import datetime


os.chdir('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/AP 1')

company_names_lower = [
    'air-liquide', 'airbus', 'bouygues', 'capgemini', 'carrefour', 'casino-guichard-perrachon', 'vivendi', 
    'kering', 'l-oreal', 'lvmh', 'michelin', 'orange', 'renault', 'sanofi', 'thales', 
    'totalenergies', 'vinci', 'compagnie-de-saint-gobain', 'ubisoft', 'tf1', 'danone', 
    'dassault-aviation', 'air-france-klm', 'accor', 'bic', 'hermes-international', 
    'jcdecaux', 'nexans', 'sodexo', 'biomerieux']

tickers = [
    'AI.PA', 'AIR.PA', 'EN.PA', 'CAP.PA', 'CA.PA', 'CO.PA', 'VIV.PA', 'KER.PA', 'OR.PA', 'MC.PA',
    'ML.PA', 'ORA.PA', 'RNO.PA', 'SAN.PA', 'HO.PA', 'TTE.PA', 'DG.PA', 'SGO.PA', 'UBI.PA', 'TFI.PA',
    'BN.PA', 'AM.PA', 'AF.PA', 'AC.PA', 'BB.PA', 'RMS.PA', 'DEC.PA', 'NEX.PA', 'SW.PA', 'BIM.PA']



#%% Scrap firm level data for French and Fama factors 
# List of companies' URLs
mktcap_urls = ['https://companiesmarketcap.com/'+ x+'/marketcap/' for x in company_names_lower]
pricebook_urls = ['https://companiesmarketcap.com/'+ x+'/pb-ratio/' for x in company_names_lower]


# Scrapping functions
def scrape_market_cap(url, company_name):
    response = requests.get(url)
    if response.status_code == 200:
        soup = BeautifulSoup(response.content, 'html.parser')
        table_body = soup.find('table', class_='table').find('tbody')
        if table_body:
            data = []
            rows = table_body.find_all('tr')
            for row in rows:
                cols = row.find_all('td')
                if len(cols) >= 2:
                    year = cols[0].text.strip()
                    market_cap = cols[1].text.strip()
                    #variation = cols[2].text.strip()
                    data.append({'Year': year, 'MarketCap': market_cap, 'Company': company_name})
            return pd.DataFrame(data)
    return None

def scrape_price_book(url, company_name):
    response = requests.get(url)
    if response.status_code == 200:
        soup = BeautifulSoup(response.content, 'html.parser')
        table_body = soup.find('table', class_='table').find('tbody')
        if table_body:
            data = []
            rows = table_body.find_all('tr')
            for row in rows:
                cols = row.find_all('td')
                if len(cols) >= 2:
                    year = cols[0].text.strip()
                    pricebook = cols[1].text.strip()
                    #variation = cols[2].text.strip()  
                    data.append({'Year': year, 'PriceBook': pricebook, 'Company': company_name})
            return pd.DataFrame(data)
    return None


# Scraping market cap data
dfmktcap = pd.DataFrame()
for url, company in zip(mktcap_urls, company_names_lower):
    data = scrape_market_cap(url, company)
    if data is not None:
        dfmktcap = pd.concat([dfmktcap, data])
        
#Scap price book 
dfpricebook = pd.DataFrame()
for url, company in zip(pricebook_urls, company_names_lower):
    data = scrape_price_book(url, company)
    if data is not None:
        dfpricebook = pd.concat([dfpricebook, data])
        
#indexes 
dfmktcap['Year'] = pd.to_datetime(dfmktcap['Year'])
dfmktcap['Year'] = pd.DatetimeIndex(dfmktcap['Year']).year

dfpricebook['Year'] = pd.to_datetime(dfpricebook['Year'])
dfpricebook['Year'] = pd.DatetimeIndex(dfpricebook['Year']).year


##Merge datasets 
final_firm = dfmktcap.copy()
final_firm = final_firm.merge(dfpricebook, how='outer', on=['Year', 'Company'])


del dfmktcap, dfpricebook, mktcap_urls, pricebook_urls, url, data, company


missing = final_firm[final_firm.isna().any(axis=1)]
missing = missing.sort_values(by=['Year'])
missing = missing.reset_index()
#have both data points for all firms for 2010-2022
# in 09 only missing data is from BIC, Carrefour, Ubisolft, AirFrance


#%%Get return data with Yahoo finance

start = datetime.datetime(2009, 1, 1)
end = datetime.datetime(2022, 12, 31)

#Get all data
data = yf.download(tickers, start=start,
                end=end)

#Focus on adjusted closed values only 
adjclose=data['Adj Close']
adjclose = adjclose.set_axis(company_names_lower, axis=1)

#Use monthly data: mean of the months value
adjclose = adjclose.resample('1M').mean()

#Reshape
prices_monthly = pd.melt(adjclose, value_vars=company_names_lower, ignore_index=False)

del data, missing, adjclose


#%% Endogeneous factor: long term inflation expectation 

