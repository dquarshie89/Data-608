library(XML)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(lubridate)

##Zip Code Data##
nyc_zips <- read.csv("https://raw.githubusercontent.com/dquarshie89/Data-608/master/NYC%20Zip%20Codes.csv",
                header= TRUE)
#List each zip code for each boro and neighborhood
nyc_zips <- nyc_zips %>% mutate(ZIP.Codes = strsplit(as.character(ZIP.Codes), ',')) %>% unnest(ZIP.Codes)
#Rename and trim zip code column so it can be used with other data sources
names(nyc_zips)[names(nyc_zips) == 'ZIP.Codes'] <- 'ZipCode' 
nyc_zips$ZipCode <- as.character(nyc_zips$ZipCode)
nyc_zips$ZipCode <- str_trim(nyc_zips$ZipCode)

##Real Estate Data##
real_estate <- read.csv("https://raw.githubusercontent.com/dquarshie89/Data-608/master/NY_Price_Data.csv",
                     header= TRUE)
#Get median prices for zip codes in the NY zip code data
real_estate$RegionName <- as.character(real_estate$RegionName)
real_estate <- real_estate %>% filter(RegionName %in% nyc_zips$ZipCode)
#Rename Region to be zip code and tranform data to be narrow
names(real_estate)[names(real_estate) == 'RegionName'] <- 'ZipCode' 
real_estate <- real_estate %>% gather(Period, MedianPrice, -ZipCode)
#Get the month and year for each record
real_estate$Period <- as.Date(as.yearmon(real_estate$Period, 'X%Y.%m'))
real_estate$Year <- year(real_estate$Period)
real_estate$Month <- month(real_estate$Period)
#Get % price change from the first month on record
real_estate <- real_estate %>% 
  arrange(ZipCode, Period) %>% group_by(ZipCode) %>% drop_na(MedianPrice) %>%
  mutate(PriceChange = MedianPrice / first(MedianPrice) - 1)

##Business Data##
biz <- read.csv("https://raw.githubusercontent.com/dquarshie89/Data-608/master/NY_Biz_From_2000.csv",
                        header= TRUE)
# select five new york counties and include only needed variables
biz <- biz %>% 
  select(Period = Initial.DOS.Filing.Date, ZipCode = DOS.Process.Zip, Name=Current.Entity.Name) %>%
  filter(ZipCode %in% nyc_zips$ZipCode)
# get month and year for filings, exclude day
biz$Period <- as.Date(as.yearmon(biz$Period, '%m/%d/%Y'))
biz$Year <- year(biz$Period)
biz$Month <- month(biz$Period)