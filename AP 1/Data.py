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

#import pandas_datareader.data as web
import yfinance as yf
#from eurostatapiclient import EurostatAPIClient
import datetime


os.chdir('/Users/nataliacardenasf/Documents/GitHub/PROJECTS_AP_FE/AP 1')

company_names_lower = [
    'air-liquide', 'airbus', 'bouygues', 'capgemini', 'carrefour', 'casino-guichard-perrachon', 'vivendi', 
    'kering', 'l-oreal', 'lvmh', 'michelin', 'orange', 'renault', 'sanofi', 'thales', 
    'totalenergies', 'vinci', 'compagnie-de-saint-gobain', 'ubisoft', 'tf1', 'danone', 
    'dassault-aviation', 'air-france-klm', 'accor', 'bic', 'hermes-international', 
    'jcdecaux', 'nexans', 'sodexo', 'biomerieux', "CAC40", "EuroUSD"]

tickers = [
    'AI.PA', 'AIR.PA', 'EN.PA', 'CAP.PA', 'CA.PA', 'CO.PA', 'VIV.PA', 'KER.PA', 'OR.PA', 'MC.PA',
    'ML.PA', 'ORA.PA', 'RNO.PA', 'SAN.PA', 'HO.PA', 'TTE.PA', 'DG.PA', 'SGO.PA', 'UBI.PA', 'TFI.PA',
    'BN.PA', 'AM.PA', 'AF.PA', 'AC.PA', 'BB.PA', 'RMS.PA', 'DEC.PA', 'NEX.PA', 'SW.PA', 'BIM.PA', "^FCHI", 'EURUSD=X']



#%% Scrap firm level data for French and Fama factors 
# List of companies' URLs
mktcap_urls = ['https://companiesmarketcap.com/'+ x+'/marketcap/' for x in company_names_lower[:-2]]
pricebook_urls = ['https://companiesmarketcap.com/'+ x+'/pb-ratio/' for x in company_names_lower[:-2]]


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
for url, company in zip(mktcap_urls, company_names_lower[:-2]):
    data = scrape_market_cap(url, company)
    if data is not None:
        dfmktcap = pd.concat([dfmktcap, data])
        
#Scap price book 
dfpricebook = pd.DataFrame()
for url, company in zip(pricebook_urls, company_names_lower[:-2]):
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

del index, monthly_year, row, firm_data

firms_year = final_firm.copy()
firms_month = monthly_data.copy()

del final_firm, monthly_data, missing

#%%Get return data with Yahoo finance

start = datetime.datetime(2002, 1, 1)
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

## extract CAC40 and exchange rate 
cac_xrate_month = adjclose.loc[:, ["CAC40", "EuroUSD"]]
cac_xrate_year = adjclose_y.loc[:, ["CAC40", "EuroUSD"]]
adjclose = adjclose.drop(columns=["CAC40", "EuroUSD"])
adjclose_y = adjclose_y.drop(columns=["CAC40", "EuroUSD"])

#Reshape
prices_monthly = pd.melt(adjclose, value_vars=company_names_lower[0:-2], ignore_index=False)
prices_yearly = pd.melt(adjclose_y, value_vars=company_names_lower[0:-2], ignore_index=False)


del data, adjclose, adjclose_y

#I'm not getting right values for xrate when dowloading in bulk
data = yf.download(['EURUSD=X', '^FCHI'], start=start, end=end)
adjclose=data['Adj Close']
adjclose = adjclose.set_axis(['EuroUSD','CAC40'], axis=1)
#adjclose = pd.DataFrame(adjclose, columns=['Date', 'EuroUSD'])

cac_xrate_month = adjclose.resample('1M').mean(numeric_only=True)
cac_xrate_year = adjclose.resample('1Y').mean(numeric_only=True)

del data, adjclose, start, end


#%% Endogeneous factor: long term inflation expectation from external file 

#upload the Agence France Tresor data
pi_endo= pd.read_excel('2023_11_01_rend_tit_ref_oatei.xls', skiprows=[0,1,2,3,4], usecols=[0,3])
pi_endo.columns = ['Date', "Breakeven"]

pi_endo["Date"] = pd.to_datetime(pi_endo["Date"])
pi_endo= pi_endo.set_index(pi_endo["Date"])
pi_endo.drop(columns=['Date'])

#get monthly data 
piendo_month = pi_endo.resample('1M').mean(numeric_only=True)

#get yearly data
piendo_year = pi_endo.resample('1Y').mean(numeric_only=True)

del pi_endo

#%% French and Fama - their data 

df = pd.read_csv('Europe_3_Factors.csv',skiprows=[0,1,2])

#montly data, need to fix dates
frenchfama_month = df.iloc[:399,:]

frenchfama_month['Unnamed: 0'] = frenchfama_month['Unnamed: 0'].astype(str)  # Convert to string for manipulation
frenchfama_month['Year'] = frenchfama_month['Unnamed: 0'].str[:4]  # Extract year from the encoded date
frenchfama_month['Month'] = frenchfama_month['Unnamed: 0'].str[4:]  # Extract month from the encoded date
frenchfama_month['Date'] = pd.to_datetime(dict(year=frenchfama_month['Year'], month=frenchfama_month['Month'], day=1))
frenchfama_month.drop(['Year', 'Month', 'Unnamed: 0'], axis=1, inplace=True)
frenchfama_month = frenchfama_month.set_index(frenchfama_month['Date'])
frenchfama_month = frenchfama_month.drop(columns=["Date"])
frenchfama_month = frenchfama_month.loc['2002-01-01':]
frenchfama_month = frenchfama_month.astype(float)


#yearly data
frenchfama_year = df.iloc[402:,:]
frenchfama_year["Unnamed: 0"] = pd.to_datetime(frenchfama_year['Unnamed: 0'])
frenchfama_year.rename(columns={"Unnamed: 0":'Date'}, inplace=True)
frenchfama_year = frenchfama_year.set_index(frenchfama_year['Date'])
frenchfama_year = frenchfama_year.drop(columns=["Date"])
frenchfama_year = frenchfama_year.loc['2002-01-01':]
frenchfama_year = frenchfama_year.astype(float)

del df 


#%% Macro data 
#APIs didn't work as planned

## PIB Q
pib = pd.read_csv("ECB_PIB.csv")
pib.columns = ['Date', 'Q', 'pib']
pib['Date'] = pd.to_datetime(pib['Date'])
pib = pib.drop(columns=['Q'])
pib = pib.set_index(pib['Date'])
pib = pib.loc['2002-01-01':]
pib = pib.drop(columns=['Date'])

#monthly
pib_monthly = pib.resample('MS').ffill()
#yearly 
pib_year = pib.resample('1Y').last()

del pib

##risk free AAA
rkfreeAAA = pd.read_csv('ECB_yield.csv')
rkfreeAAA.columns=['Date', "time" ,"rkfreeAAA"]
rkfreeAAA['Date'] = pd.to_datetime(rkfreeAAA['Date'])
rkfreeAAA = rkfreeAAA.set_index(rkfreeAAA['Date'])
rkfreeAAA = rkfreeAAA.drop(columns=['time', "Date"])

rkfreeAAA_monthly = rkfreeAAA.resample("1M").mean(numeric_only=True)
rkfreeAAA_year = rkfreeAAA.resample("1Y").mean(numeric_only=True)

del rkfreeAAA

##HICP 
pi_month = pd.read_csv("HICP.csv", sep=';', encoding = 'latin1',skiprows=[0,1,2,3], usecols=[0,1], header=None)
pi_month.columns= ['Date', "infl"]
pi_month["Date"] = pd.to_datetime(pi_month['Date'], format = "%Y-%m")
pi_month=pi_month.set_index(pi_month["Date"])
pi_month = pi_month.drop(columns=['Date'])

#yearly 
pi_year = pi_month.resample("1Y").mean(numeric_only=True)


##OAT 
oat = pd.read_csv('OAT.csv', sep=';', skiprows=[0,1,2,3,4,5], usecols=[0,5], header=None)
oat.columns = ['Date', 'oat']
oat['Date'] = pd.to_datetime(oat["Date"])
oat = oat.set_index(oat["Date"])
oat = oat.drop(columns=["Date"])
oat = oat.loc["2002-01-01":]

oat['oat'] = oat['oat'].replace("-", np.nan)
oat['oat'] = oat['oat'].str.replace(',', '.').astype(float)

oat_month = oat.resample('M').mean(numeric_only=True)
oat_year = oat.resample('1Y').mean(numeric_only=True)

del oat


#%% Merge the dataframes 

#===MONTHLY 
#macro stuff, only date index matter 
monthly = pd.concat([cac_xrate_month, frenchfama_month, oat_month, pi_month, pib_monthly, piendo_month, rkfreeAAA_monthly], axis=1)
monthly= monthly.resample('M').last() #some tables encoded end of month, others on the 1st

monthly.to_csv('Monthly_series.csv')


#firm specific data 
firms_month['Date'] = firms_month['Date'] + pd.offsets.MonthEnd(0) #all other df have eomonth date
firms_month = firms_month.set_index(['Date'])
firms_month = firms_month.set_index('Company', append=True)

prices_monthly.columns = ['Company', 'value']
prices_monthly.set_index('Company', append=True)

monthly_stock = pd.merge(prices_monthly.reset_index(), firms_month.reset_index(), on =["Date", "Company"], how='outer').set_index(["Date", "Company"])
monthly_stock.to_csv("Firm_monthly.csv")

#merge the two
merged_monthly = pd.merge(monthly, monthly_stock, left_index=True, right_index=True, how='right')

#Export df in csv
merged_monthly.to_csv("DATA_month.csv")



#=== YEARLY
yearly = pd.concat([cac_xrate_year, frenchfama_year, oat_year, pi_year, pib_year, piendo_year, rkfreeAAA_year], axis=1)
yearly = yearly.resample('Y').last()

yearly.to_csv('Yearly_series.csv')

# ============================================================================= 
# #firm specific
# firms_year.rename(columns={"Year":'Date'}, inplace=True)
# firms_year['Date'] = firms_year['Date'] + pd.offsets.MonthEnd(0)
# firms_year = firms_year.set_index(['Date'])
# firms_year = firms_year.set_index('Company', append=True)
# 
# prices_yearly.columns = ['Company', 'value']
# prices_yearly = prices_yearly.set_index('Company', append=True)
# 
# yearly_stock = pd.merge(prices_yearly.reset_index(), firms_year.reset_index(), on =["Date", "Company"], how='outer').set_index(["Date", "Company"])
# =============================================================================

#yearly_stock = monthly_stock.groupby('Company').resample('Y').mean()

monthly_data_reset = monthly_stock.reset_index()
yearly_stock = monthly_data_reset.groupby('Company').resample('Y', on='Date').mean()
#yearly_stock = yearly_stock.set_index(["Date", "Company"])

yearly_stock.to_csv("Firm_yearly.csv")


#merge the two 
merged_year = pd.merge(yearly, yearly_stock, left_index=True, right_index=True, how='right')

merged_year.to_csv("DATA_yearly.csv")


