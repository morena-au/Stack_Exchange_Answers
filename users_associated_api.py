# Run a spider self-contained in a Python file
# scrapy runspider users_associated_api.py

import pandas as pd
import numpy as np
import scrapy
import json
import time
import os
import re

os.chdir("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
urls = pd.read_csv("urls_associated_api.csv")

class UsersAssociatedSpider(scrapy.Spider):
    name = "users_associated_scrape"
    allowed_domains = ["api.stackexchange.com"]
    start_urls = list(urls.url) # requirement: 10000 requests per day
    custom_settings = {
        #'ROBOTSTXT_OBEY': 'True', 
        # 'CONCURRENT_REQUESTS': '1',
        'DOWNLOAD_DELAY': 0.30, # requirement: 30 requests a second
        #'AUTOTHROTTLE_ENABLED': 'True'
    }

    def parse(self, response):
        
        pattern = re.compile(r'page=(\d+)&')
        page_n = int(pattern.findall(response.url)[0])

        results = json.loads(response.body)

        users_associated_info = pd.DataFrame.from_dict(results["items"]) 

        # Check if there is a file named users_associated_info.csv in the working directory
        os.chdir("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")

        if os.path.isfile('users_associated_info.csv'):
            # Load the existing csv file
            prior = pd.read_csv('users_associated_info.csv')
            # Concatenate the two dataframes
            users_associated_info = pd.concat([prior, users_associated_info])
            # Write down the updated urls.csv file
            users_associated_info.to_csv('users_associated_info.csv', index=False)

        else:
            # Create a csv file with the URLs just extracted
            users_associated_info.to_csv('users_associated_info.csv', index=False)

        # if the api call has more pages load the pages and get all the data
        if results['has_more'] == True:
            page_n += 1 
            yield scrapy.Request(re.sub(r'page=(\d+)&', 'page='+str(page_n)+'&', response.url), 
                                        callback=self.parse)
             

