#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar  8 21:40:07 2024

@author: home
"""



import pandas as pd


# read in csv

df = pd.read_csv('/Volumes/NO NAME/centene_interview/OMT_MDCD_R23_P11_V10_YTD21_GEO.csv')


# # Display the first few rows of the DataFrame
# print(df.head())

# # Get basic information about the DataFrame
# print(df.info())

# # Get summary statistics of the numerical columns
# print(df.describe())
#%%
df = df.rename(columns={

    'Tot_Opioid_Clms': 'Opioid Claims',

    'Tot_Clms': 'Overall Claims',

    'Opioid_Prscrbng_Rate': 'Opioid Prescribing Rate',

    'Opioid_Prscrbng_Rate_5Y_Chg': 'Five Year Change in Opioid Prescribing Rate',

    'Opioid_Prscrbng_Rate_1Y_Chg': 'One Year Change in Opioid Prescribing Rate',

    'LA_Tot_Opioid_Clms': 'Long-Acting Opioid Claims',

    'LA_Opioid_Prscrbng_Rate': 'Long-Acting Opioid Prescribing Rate',

    'LA_Opioid_Prscrbng_Rate_5Y_Chg': 'Five Year Change in Long-Acting Opioid Prescribing Rate',

    'LA_Opioid_Prscrbng_Rate_1Y_Chg': 'One Year Change in Long-Acting Opioid Prescribing Rate'

})




#%%

import matplotlib.pyplot as plt
import seaborn as sns

# Calculate the correlation matrix
correlation_matrix = df.corr()

# Plot heatmap of the correlation matrix
plt.figure(figsize=(10, 8))
sns.heatmap(correlation_matrix, annot=True, cmap='coolwarm', fmt=".2f", linewidths=.5)
plt.title('Correlation Matrix')
plt.show()

# Investigate correlation between 'Opioid_Prscrbng_Rate' and other variables
opioid_rate_correlation = correlation_matrix['Opioid_Prscrbng_Rate'].drop('Opioid_Prscrbng_Rate')
print(opioid_rate_correlation)
#%%

import matplotlib.pyplot as plt
import seaborn as sns

# Filter out rows where Geo_Lvl is State
state_df = df[df['Geo_Lvl'] == 'State']


# Sort states by their opioid prescribing rates
sorted_states = state_df.sort_values(by='Opioid_Prscrbng_Rate', ascending=False)

# Plot opioid prescribing rates for each state
plt.figure(figsize=(12, 8))
sns.barplot(x='Opioid_Prscrbng_Rate', y='Geo_Desc', data=sorted_states, palette='viridis')
plt.xlabel('Opioid Prescribing Rate (%)', fontsize=24)  # Increase font size for x-axis label
plt.ylabel('State', fontsize=24)  # Increase font size for y-axis label
plt.title('Opioid Prescribing Rates by State')

# Increase font size of x and y tick labels
plt.xticks(fontsize=18)
plt.yticks(fontsize=14)


plt.show()



#%%
# Assuming 'df' contains multiple years of data

# Filter national level data for trend analysis
national_trend_df = df[df['Geo_Lvl'] == 'National']

# Pivot data to have years on the x-axis and opioid prescribing rate on the y-axis
trend_data = national_trend_df.pivot_table(values='Opioid_Prscrbng_Rate', index='Year', columns='Geo_Desc')

# Plotting the trend over years
trend_data.plot(figsize=(12, 8), marker='o')
plt.title('National Opioid Prescribing Rate Over Time')
plt.xlabel('Year')
plt.ylabel('Opioid Prescribing Rate (%)')
plt.legend(title='Geo_Desc').remove()
plt.show()

# For a specific state trend, replace 'National' with the state name
# And you may also want to compare a few states, so instead of filtering by 'National',
# You would filter by those state names



#%%




