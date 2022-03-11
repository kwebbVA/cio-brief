# -*- coding: utf-8 -*-
"""
This file is used for the setup of the CIO Brief presentation
- folder created by user input of the CIO Brief date
- files moved from github repo CIO_Brief
- R project file created
- data, ad_hoc_scripts, and slides folders created
"""

import os
import datetime
import shutil

cioBdate = input('Date of CIO Briefing Presentation (YYYY_MM_DD)? ')


# function to check if date is in correct format
def checkDate(date):
    date = date.strip()

    if len(date) == 10:
        numUnderscores = date.count('_')
        if numUnderscores == 2:
            year, month, day = date.split('_')
            try:
                datetime.datetime(int(year), int(month), int(day))
                return True
            except ValueError:
                return False

    return False


# Continue to ask for correct input if not actual date
while True:
    if checkDate(cioBdate):
        break
    else:
        print("Date must be of the format YYYY_MM_DD")
        cioBdate = input('Date of CIO Briefing Presentation (YYYY_MM_DD)? ')

# Create folder using user entered date, subfolders, and populate with necessary files
os.chdir("C:/Users/" + os.getlogin() + "/Department of Veterans Affairs/Operations Triage Group (OTG) - Analytics - Documents/Publication/CIO Brief/CIOBriefs_OTG_2.0")
os.mkdir(cioBdate)
os.mkdir(cioBdate + "/data")
os.mkdir(cioBdate + "/slides")
os.mkdir(cioBdate + "/ad_hoc_scripts")

sources = ["cio-brief/background.jpg",
           "cio-brief/weekly_CIO_Brief.R",
           "cio-brief/titlePage.jpg",
           "cio-brief/blank_title.pptx"]

destinations = [cioBdate + "/background.jpg",
                cioBdate + "/weekly_CIO_Brief.R",
                cioBdate + "/titlePage.jpg",
                cioBdate + "/slides/blank_title.pptx"]

f_copy = lambda x_from, x_to: shutil.copy(x_from, x_to)

for i in range(len(sources)):
    f_copy(sources[i], destinations[i])
