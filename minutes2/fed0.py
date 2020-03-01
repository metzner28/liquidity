# scrapes HTML minutes, 1993-2019, from FOMC and writes to text files for downstream analysis

from bs4 import BeautifulSoup
from urllib import request
import pandas as pd

#### make sure it's only pulling 1993 and later here, the csv was updated to include all dates ###
minutes = pd.read_csv("fed0.csv")
##########

for date, url in zip(minutes.date, minutes.url):

        with open(f"{date}.txt", 'w', encoding = 'utf-8') as file:

            html = request.urlopen(url).read()
            soup = BeautifulSoup(html, 'html.parser')
            paragraphs = soup.find_all("p")

            for text in paragraphs:
                minutes_parsed = text.get_text()
                file.write(minutes_parsed)
