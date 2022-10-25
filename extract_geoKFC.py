#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Apr 16 12:13:15 2022

@author: carmenarbaizar
"""

## First part

import pandas as pd
dtafile = ''
df = pd.read_stata(dtafile)
list_of_urls=df["link"].to_list()

import csv
from urllib.request import urlopen
from bs4 import BeautifulSoup
from pathlib import Path
import time
import re

file_csv = open('KFC_lon_lat.csv', 'w+')
print("Link,","Latitude,","Longitude", file = file_csv)
for item in list_of_urls:
    url = 'https://locations.kfc.com/' + item
    conn = urlopen(url)
    html = conn.read()
    
    soup = BeautifulSoup(html)
    links = soup.find_all('a')
    
    
    list1 = []
    list2 = []
    
    lat=soup.find("meta", itemprop="latitude") 
    lon=soup.find("meta", itemprop="longitude") 
    non_decimal = re.compile(r'[^\d.-]+') 
    latitude=non_decimal.sub('', str(lat)) 
    longitude=non_decimal.sub('', str(lon))
    list1.append(latitude)
    list2.append(longitude)
            #print(link)
    print(item,",",list1[0],",",list2[0], file = file_csv)


## NOTE: ((ANACONDA ISSUE))

## Second part

import csv
from urllib.request import urlopen
from bs4 import BeautifulSoup
import pandas as pd
from pathlib import Path
import time

state_id = ["al", "ak", "az","ar",
            "ca","co","ct","de",
            "fl","ga","hi","id",
           "il","in","ia","ks",
          "ky","la","me","md",
          "ma","mi","mn","ms",
          "mo","mt","ne","nv",
          "nh","nj","nm","ny",
          "nc","nd","oh","ok",
          "or", "pa","pr","ri",
          "sc","sd","tn","tx",
          "ut","vt","vi","va",
          "wa","wv","wi","wy"]

file_csv = open('urls.csv', 'w+')
print("State,", "Link", file = file_csv)
for state in state_id:
    url = 'https://locations.kfc.com/' + state
    conn = urlopen(url)
    html = conn.read()
    
    soup = BeautifulSoup(html)
    links = soup.find_all('a')
    
    list_link = []
    
    for tag in links:
        link = tag.get('href',None)
        if link is not None:
            list_link.append(link)
            #print(link)
    
    for i in range(len(list_link)):
        if list_link[i][0:2] == state:
            print(state, ", ", list_link[i], file = file_csv)
        #print(list_link[i][0:2])
    



