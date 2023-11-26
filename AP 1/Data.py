#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
AP1 - Data gathering and Data Cleaning

Scrapping - Firm level data for French and Fama factors
Use Yahoo Finance API to get the financial data for all the stocks 
Use Eurostat API to get the macro data 
Two data sets (French-Fama factors and long term inflation expectation) are found online and have been downloaded in CSV file beforehang

Merge and clean the dataset

@author: nataliacardenasf
"""

import pandas as pd
import numpy as np
import os

import requests
from bs4 import BeautifulSoup

import pandas_datareader.data as web
import yfinance as yf
#from eurostatapiclient import EurostatAPIClient
import datetime


os.chdir('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/AP 1')

company_names_lower = [
    'air-liquide', 'airbus', 'bouygues', 'capgemini', 'carrefour', 'casino-guichard-perrachon', 'vivendi', 
    'kering', 'l-oreal', 'lvmh', 'michelin', 'orange', 'renault', 'sanofi', 'thales', 
    'totalenergies', 'vinci', 'compagnie-de-saint-gobain', 'ubisoft', 'tf1', 'danone', 
    'dassault-aviation', 'air-france-klm', 'accor', 'bic', 'hermes-international', 
    'jcdecaux', 'nexans', 'sodexo', 'biomerieux', "CAC40"]

tickers = [
    'AI.PA', 'AIR.PA', 'EN.PA', 'CAP.PA', 'CA.PA', 'CO.PA', 'VIV.PA', 'KER.PA', 'OR.PA', 'MC.PA',
    'ML.PA', 'ORA.PA', 'RNO.PA', 'SAN.PA', 'HO.PA', 'TTE.PA', 'DG.PA', 'SGO.PA', 'UBI.PA', 'TFI.PA',
    'BN.PA', 'AM.PA', 'AF.PA', 'AC.PA', 'BB.PA', 'RMS.PA', 'DEC.PA', 'NEX.PA', 'SW.PA', 'BIM.PA', "^FCHI"]



#%% Scrap firm level data for French and Fama factors 
# List of companies' URLs
mktcap_urls = ['https://companiesmarketcap.com/'+ x+'/marketcap/' for x in company_names_lower[:-1]]
pricebook_urls = ['https://companiesmarketcap.com/'+ x+'/pb-ratio/' for x in company_names_lower[:-1]]


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
for url, company in zip(mktcap_urls, company_names_lower[:-1]):
    data = scrape_market_cap(url, company)
    if data is not None:
        dfmktcap = pd.concat([dfmktcap, data])
        
#Scap price book 
dfpricebook = pd.DataFrame()
for url, company in zip(pricebook_urls, company_names_lower[:-1]):
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

#remove 2023
final_firm = final_firm[final_firm.Year != 2023]



#Get book to market ratio = inverse of price-book ratio 
final_firm['PriceBook'] = pd.to_numeric(final_firm['PriceBook'], errors='coerce')
final_firm['PriceBook'].replace('nan', np.nan, inplace=True)
final_firm['BookMarket']  = final_firm['PriceBook'].apply(lambda x: x ** -1 if not pd.isnull(x) else np.nan)

#Clean MarketCap
final_firm['MarketCap'] = (final_firm['MarketCap'].replace({'\$': '', ' B': ''}, regex=True).astype(float) * 1_000)  # Clear the letters, convert to float and scale to millions
            
#Date format
final_firm['Year'] = pd.to_datetime(final_firm['Year'], format='%Y')

#====Get monthly data 
monthly_data = pd.DataFrame()
# Repeat the yearly data for each month and each firm
for index, row in final_firm.iterrows():
    firm_data = pd.DataFrame()
    monthly_year = pd.date_range(start=row['Year'], periods=12, freq='MS')
    firm_data['Date'] = monthly_year
    firm_data['Company'] = row['Company']
    firm_data['MarketCap'] = row['MarketCap']
    firm_data['BookMarket'] = row['BookMarket']
    firm_data['PriceBook'] = row['PriceBook']
    monthly_data = pd.concat([monthly_data, firm_data])

del index, monthly_year, row



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
adjclose = adjclose.resample('1M').mean(numeric_only=True)
adjclose_y = adjclose.resample('1Y').mean(numeric_only=True)

#Reshape
prices_monthly = pd.melt(adjclose, value_vars=company_names_lower, ignore_index=False)
prices_yearly = pd.melt(adjclose_y, value_vars=company_names_lower, ignore_index=False)


del data, missing, adjclose, adjclose_y


#%% Endogeneous factor: long term inflation expectation from external file 

#upload the Agence France Trésor data
pi_endo= pd.read_excel('2023_11_01_rend_tit_ref_oatei.xls', skiprows=[0,1,2,3,4], usecols=[0,3])
pi_endo.columns = ['Date', "Breakeven"]

pi_endo["Date"] = pd.to_datetime(pi_endo["Date"])
pi_endo= pi_endo.set_index(pi_endo["Date"])
pi_endo.drop(columns=['Date'])

#get monthly data 
pi_endo = pi_endo.resample('1M').mean(numeric_only=True)

#get yearly data
pi_endo = pi_endo.resample('1Y').mean(numeric_only=True)


#%% French and Fama - their data 

df = pd.read_csv('Europe_3_Factors.csv',skiprows=[0,1,2])

#montly data, need to fix dates
frenchfama_month = df.iloc[:399,:]

frenchfama_month['Unnamed: 0'] = frenchfama_month['Unnamed: 0'].astype(str)  # Convert to string for manipulation
frenchfama_month['Year'] = frenchfama_month['Unnamed: 0'].str[:4]  # Extract year from the encoded date
frenchfama_month['Month'] = frenchfama_month['Unnamed: 0'].str[4:]  # Extract month from the encoded date
frenchfama_month['Date'] = pd.to_datetime(dict(year=frenchfama_month['Year'], month=frenchfama_month['Month'], day=1))
frenchfama_month.drop(['Year', 'Month', 'Unnamed: 0'], axis=1, inplace=True)

#yearly data
frenchfama_year = df.iloc[402:,:]
frenchfama_year["Unnamed: 0"] = pd.to_datetime(frenchfama_year['Unnamed: 0'])
frenchfama_year.rename(columns={"Unnamed: 0":'Date'}, inplace=True)

del df 


















#%% Macro data - Eurostat 

pd.core.common.is_list_like = pd.api.types.is_list_like

SP500 = web.DataReader(['sp500'], 'fred', start, end)
SP500 = SP500.asfreq('W-FRI', method='pad')
print(SP500)
