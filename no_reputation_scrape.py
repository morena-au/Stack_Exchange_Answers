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
                                    "NOreputation.log", mode='a', encoding='utf-8')
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
        'DOWNLOAD_DELAY': 1, # requirement: 30 requests a second
        'AUTOTHROTTLE_ENABLED': 'True',
        'USER_AGENT': 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:66.0) Gecko/20100101 Firefox/66.0',
        'LOG_FILE': 'C:/Projects/Stack_Exchange/motivation_feedback/Answers/errors_scrape.log',
        'LOG_LEVEL': 'ERROR',
        'LOG_FORMAT': '%(asctime)s,%(levelname)s,%(message)s',
    }

    allowed_domains = ['ux.stackexchange.com']

    # import reputation urls
    reputation_url = pd.read_csv('C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Reputation/NOreputation_urls.csv')
    start_urls = reputation_url['url'].tolist()

    def parse(self, response):
        logger.info("Starting Url: {}".format(response.url))
        logger.info("Starting Response: {}".format(response.status))

        user_search = re.search(r'users/(\d+)/', response.url) 
        user = user_search.group(1)
        
        reputation = response.css('#rep-page-container .empty::text').extract() 
        Reputation = pd.DataFrame({'OwnerUserId': [user]*len(reputation),
                                    'reputation': reputation})

        # Check if there is a file named Reputation.csv in the working directory
        os.chdir(r'C:\Projects\Stack_Exchange\motivation_feedback\Answers\data\raw')

        if os.path.isfile('NOReputation.csv'):
            # Load the existing csv file
            prior_df = pd.read_csv('NOReputation.csv')
            # Concatenate the two dataframes
            Reputation = pd.concat([prior_df, Reputation])

            # Write down the updated Reputation.csv file
            Reputation.to_csv('NOReputation.csv', index=False)
        else:
            # Create a csv file with the Reputations just extracted
            Reputation.to_csv('NOReputation.csv', index=False)
        