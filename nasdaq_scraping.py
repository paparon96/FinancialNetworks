# Import packages
from splinter import Browser
import bs4
from bs4 import BeautifulSoup
import selenium
from selenium import webdriver
import time
from selenium.webdriver.common.keys import Keys
import pandas as pd
from selenium.webdriver.chrome.options import Options
from datetime import datetime
import copy
from datetime import date

# Get current data and transform it into another format
today = date.today()
current_date = today.strftime("%d_%m_%Y")
print("Current date reformatted:")
print(current_date)


# Important trick to access all elements of the list: https://stackoverflow.com/questions/48162795/iterating-through-elements-get-repeating-result-on-selenium-on-python

# Parameters
chrome_options = Options()
chrome_options.add_argument("--headless")
#driver = webdriver.Chrome()  # options=chrome_options # or empty if we want to see it in action

# Initialize list of tuples to hold general infos
general_info_list = []


# Geolocation user-input
#firm = 'jpm' # 'jpm' , 'tsla', 'ms'
firms = ['jpm' , 'ms','bac','c','wfc','gs','usb','tfc','bk','td']
print(len(firms))

for firm in firms:

    print("Current stock: {}".format(firm))
    driver = webdriver.Chrome()  # options=chrome_options # or empty if we want to see it in action

    driver.get('https://www.nasdaq.com/market-activity/stocks/{}/institutional-holdings'.format(firm))

    # Get info about the land
    time.sleep(10)

    # Sort investors by size
    for j in range(0,2):
        sorting_buttons = driver.find_elements_by_xpath(".//span[@class='institutional-holdings__columnheader-sort']")
        sorting_button = sorting_buttons[2]
        driver.execute_script("arguments[0].click();", sorting_button)
        time.sleep(10)

    time.sleep(10)

    table_info_list = driver.find_elements_by_xpath("//table[@role='table']")
    tbl_general_info = table_info_list[0].get_attribute('outerHTML')
    df_general_info  = pd.read_html(tbl_general_info)[0]
    #print(df_general_info)

    # Append it to the list
    inst_share = df_general_info['Value'].iloc[0]
    #print(type(inst_share))
    shares_outstanding = df_general_info['Value'].iloc[1]
    #print(type(shares_outstanding))
    value_holdings = df_general_info['Value'].iloc[2]
    value_holdings = value_holdings.replace("$","")
    value_holdings = value_holdings.replace(",","")
    value_holdings = value_holdings.replace(" ","")
    #print(type(value_holdings))

    general_info_list.append((float(inst_share[0:6])/100,int(shares_outstanding)*1000000,
                                float(value_holdings)*1000,firm))

    time.sleep(10)

    # Loop over the different pages (60 largest institutional investors)
    for i in range(0,4):

        #print("Round: {}".format(i))

        table_info_list = driver.find_elements_by_xpath("//table[@role='table']")
        #print(len(table_info_list))

        tbl_interest = table_info_list[-1].get_attribute('outerHTML')

        df  = pd.read_html(tbl_interest)[0]

        # Add new column with the name of the currently considered stock/firm
        df['stock_symbol'] = firm

        if (i==0) & (firm == firms[0]):

            #print(df.head())
            #print("Hello")
            final_df = copy.deepcopy(df)
            #print(final_df.head())

            time.sleep(10)

        else:

            #print(df.head())
            final_df = final_df.append(copy.deepcopy(df))

        # Go to the next page
        next_page_button = driver.find_element_by_xpath(".//button[@class='pagination__next']")
        driver.execute_script("arguments[0].click();", next_page_button)
        time.sleep(10)
            #next_page_button.click()

        #time.sleep(2)


    # Close the webdriver
    driver.close()


# Add new column with the name of the currently considered stock/firm
#final_df['stock_symbol'] = firm

print(final_df.head())


# Export data
# Save out results to csv file
final_df.to_excel('./Data/NASDAQ_INST_holdings_{}.xlsx'.format(current_date), index=True)

# Convert list of tuples to Pandas df and export to Excel
general_info_df = pd.DataFrame(general_info_list, columns =['Institutional_ownership_share', 'Shares_outstanding',
                                                            'Holdings_value_in_th_dollar','stock_symbol'])
print(general_info_df.head())

# Save out results to csv file
general_info_df.to_excel('./Data/NASDAQ_stock_metadata_{}.xlsx'.format(current_date), index=True)
