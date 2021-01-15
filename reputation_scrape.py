# Collect reputation change, time, description
# -*- coding: utf-8 -*-
import scrapy
import re
import numpy as np
import pandas as pd
import os
import logging
import psutil
import time
import datetime
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC

# Create a new logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
file_handler = logging.FileHandler("C:/Projects/Stack_Exchange/motivation_feedback/Answers/"+ 
                                    "reputation.log", mode='a', encoding='utf-8')
formatter = logging.Formatter('%(asctime)s,%(levelname)s,%(message)s')
file_handler.setFormatter(formatter)
logger.addHandler(file_handler)

# Define a Spider class that Scrapy use to scrape web information.
# Spider class must subclass Spider
class ReputationSpider(scrapy.Spider):
    # Define attributes
    name = 'UX_reputation' # name identifies the Spider, must be unique

    custom_settings = {
        'ROBOTSTXT_OBEY': 'False', 
        'CONCURRENT_REQUESTS': '1',
        'DOWNLOAD_DELAY': 0.30, # requirement: 30 requests a second
        'AUTOTHROTTLE_ENABLED': 'True',
        'USER_AGENT': 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:66.0) Gecko/20100101 Firefox/66.0',
        'LOG_FILE': 'C:/Projects/Stack_Exchange/motivation_feedback/Answers/errors_scrape.log',
        'LOG_LEVEL': 'ERROR',
        'LOG_FORMAT': '%(asctime)s,%(levelname)s,%(message)s',
    }

    allowed_domains = ['ux.stackexchange.com']

    # import reputation urls
    reputation_url = pd.read_csv('C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Reputation/reputation.csv')
    start_urls = ['https://ux.stackexchange.com/users/21884/johntyb?tab=reputation&sort=post&page=1', 
                  'https://ux.stackexchange.com/users/31508/user31508?tab=reputation&sort=post&page=1', 
                  'https://ux.stackexchange.com/users/31514?tab=reputation&sort=post&page=1', 
                  'https://ux.stackexchange.com/users/31516?tab=reputation&sort=post&page=1', 
                  'https://ux.stackexchange.com/users/31519?tab=reputation&sort=post&page=1', 
                  'https://ux.stackexchange.com/users/31525?tab=reputation&sort=post&page=1', 
                  'https://ux.stackexchange.com/users/31536?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31537?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31540?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31567?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31570?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31572?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31574?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31577?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31588?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31605?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31606?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31613?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31635?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31657?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31658?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31661?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31667?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31681?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31685?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31697?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31699?tab=reputation&sort=post&page=1', 
                  'https://ux.stackexchange.com/users/31701?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31711?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31724?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31730?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31734?tab=reputation&sort=post&page=1', 
                  'https://ux.stackexchange.com/users/31738?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31759?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31793?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31795?tab=reputation&sort=post&page=1',
                  'https://ux.stackexchange.com/users/31803?tab=reputation&sort=post&page=1']

    def parse(self, response):
        logger.info("Starting Url: {}".format(response.url))
        logger.info("Starting Response: {}".format(response.status))

        # Check if there are other pages
        page =  response.css('.s-pagination--item::text').extract() 
        
        # if page is not empty we have more page to load
        if len(page) > 0:
            # initiate an empty list
            page_urls = []
            # create calls to each of the page
            page = [int(s) if s.isdigit() else -1 for s in page]
            for i in range(1, max(page)+1):
                page_urls.append(response.url[0:-1]+str(i))

            logger.info("Number of pages: {}".format(len(page_urls)))
            for i in page_urls:
                yield scrapy.Request(i, callback=self.parsePage)
        
        else:
            yield scrapy.Request(response.url, callback=self.parsePage)

    # Scrape what´s in the page
    def parsePage(self, response):
        logger.info("Now Scraping: {}".format(response.url))

        start_time = datetime.datetime.now()

        # Expand all the first tiers
        driver = webdriver.Chrome()
        driver.get(response.url)

        expanded_count = 0
        while True:
            try:
                run = WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CSS_SELECTOR,
                                                                                "a[class='load-body expander-arrow-small-hide']")))
                run.click()
                time.sleep(0.5)
                expanded_count += 1
            except:
                break

        logger.info("Expended row count: {}".format(expanded_count))

        #Create the dataframe to save the info
        #reputation = response.css(".rep-left span::text").extract()

        reputation = [x.text for x in driver.find_elements_by_css_selector("tr[class='rep-breakdown-row'] .rep-left")]
        time_UTC = [x.get_attribute('title') for x in driver.find_elements_by_css_selector("tr[class='rep-breakdown-row'] .rep-time")]
        description = [x.get_attribute('title') for x in driver.find_elements_by_css_selector("tr[class='rep-breakdown-row'] .rep-desc")] 

        # close the process
        driver.close()
        
        work_time = datetime.datetime.now() - start_time
        logger.info("Scraping Time: {}".format(work_time))

        user_search = re.search(r'users/(\d+)/', response.url) 
        user = user_search.group(1)

        Reputation = pd.DataFrame({'OwnerUserId': [user]*len(time_UTC),
                                    'reputation': reputation, 
                                    'time_UTC': time_UTC, 
                                    'description': description})

        # Check if there is a file named Reputation.csv in the working directory
        os.chdir(r'C:\Projects\Stack_Exchange\motivation_feedback\Answers\data\raw')

        if os.path.isfile('Reputation.csv'):
            # Load the existing csv file
            prior_df = pd.read_csv('Reputation.csv')
            # Concatenate the two dataframes
            Reputation = pd.concat([prior_df, Reputation])

            # Write down the updated Reputation.csv file
            Reputation.to_csv('Reputation.csv', index=False)
        else:
            # Create a csv file with the Reputations just extracted
            Reputation.to_csv('Reputation.csv', index=False)
        

# free the process killing any of the dangling WebDriver instances
if __name__ == '__main__':
    PROCNAME = "geckodriver"
    for proc in psutil.process_iter():
        # check whether the process name matches
        if proc.name() == PROCNAME:
            proc.kill() 

# scrapy shell -s USER_AGENT='Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:66.0) Gecko/20100101 Firefox/66.0' 'https://ux.stackexchange.com/ajax/users/73/rep/day/1281312000?sort=post'

