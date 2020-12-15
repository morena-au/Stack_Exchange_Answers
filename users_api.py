# COLLECT INFORMATION REGARDING THE USER USING THE API

import pandas as pd
import numpy as np
import scrapy
import json
import time
import os
import re

os.chdir("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
urls = pd.read_csv("users_urls_api.csv")

class UsersSpider(scrapy.Spider):
    name = "users_scrape"
    allowed_domains = ["api.stackexchange.com"]
    start_urls = list(urls.url) # requirement: 10000 requests per day
    custom_settings = {
        #'ROBOTSTXT_OBEY': 'True', 
        # 'CONCURRENT_REQUESTS': '1',
        'DOWNLOAD_DELAY': 0.30, # requirement: 30 requests a second
        #'AUTOTHROTTLE_ENABLED': 'True'
    }


    def parse(self, response):
        results = json.loads(response.body)

        users_info = pd.DataFrame.from_dict(results["items"]) 

        # Check if there is a file named users_info.csv in the working directory
        os.chdir("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")

        if os.path.isfile('users_info.csv'):
            # Load the existing csv file
            prior = pd.read_csv('users_info.csv')
            # Concatenate the two dataframes
            users_info = pd.concat([prior, users_info])
            # Write down the updated urls.csv file
            users_info.to_csv('users_info.csv', index=False)

        else:
            # Create a csv file with the URLs just extracted
            users_info.to_csv('users_info.csv', index=False)
