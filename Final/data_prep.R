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
#Add VA and DC zip codes
va_zips <- read.csv("https://raw.githubusercontent.com/dquarshie89/Data-608/master/DC%20Zip%20Codes.csv",
                    header=TRUE)
nyc_zips <- rbind(nyc_zips, va_zips)

##Real Estate Data##
real_estate <- read.csv("https://raw.githubusercontent.com/dquarshie89/Data-608/master/NY_Price_Data.csv",
                        header= TRUE)
#Get median prices for zip codes in the NY zip code data
real_estate$RegionName <- as.character(real_estate$RegionName)
real_estate <- real_estate %>% filter(RegionName %in% nyc_zips$ZipCode)
#Rename Region to be zip code and tranform data to be narrow
names(real_estate)[names(real_estate) == 'RegionName'] <- 'ZipCode' 
real_estate <- real_estate %>% gather(Period, MedianPrice, -ZipCode,-State)
#Get the month and year for each record
real_estate$Period <- as.Date(as.yearmon(real_estate$Period, 'X%Y.%m'))
real_estate$Year <- year(real_estate$Period)
real_estate$Month <- month(real_estate$Period)
#Get % price change from the first month on record
real_estate <- real_estate %>% 
  arrange(ZipCode, Period) %>% group_by(ZipCode) %>% drop_na(MedianPrice) %>%
  mutate(PriceChange = MedianPrice / first(MedianPrice) - 1)
#Get the December line, which has the final price change for the year for each zip code
real_estate_lastmonth <- real_estate %>% filter(Month==12)




##Business Data##
biz <- read.csv("https://raw.githubusercontent.com/dquarshie89/Data-608/master/NY_Biz_From_2008.csv",
                header= TRUE)
# Filter for the zip codes that are in the zip code table
biz <- biz %>% 
  select(Period = Initial.DOS.Filing.Date, ZipCode = DOS.Process.Zip) %>%
  filter(ZipCode %in% nyc_zips$ZipCode)
#Extract month and year
biz$Period <- as.Date(as.yearmon(biz$Period, '%m/%d/%Y'))
biz$Year <- year(biz$Period)
biz$Month <- month(biz$Period)
#Get a running sum of new business per month for each zip code
newbiz <- biz %>%
  group_by(ZipCode, Year, Month) %>% summarize(NewBusinesses = n()) %>% 
  group_by(ZipCode, Year) %>% mutate(TotalNewBusinesses = cumsum(NewBusinesses)) %>%
  ungroup() %>% arrange(ZipCode, Year, Month)
#Replace year with full four digit year
newbiz$Year <- replace(as.character(newbiz$Year), newbiz$Year == "8", "2008")
newbiz$Year <- replace(as.character(newbiz$Year), newbiz$Year == "9", "2009")
newbiz$Year <- replace(as.character(newbiz$Year), newbiz$Year == "10", "2010")
newbiz$Year <- replace(as.character(newbiz$Year), newbiz$Year == "11", "2011")
newbiz$Year <- replace(as.character(newbiz$Year), newbiz$Year == "12", "2012")
newbiz$Year <- replace(as.character(newbiz$Year), newbiz$Year == "13", "2013")
newbiz$Year <- replace(as.character(newbiz$Year), newbiz$Year == "14", "2014")
newbiz$Year <- replace(as.character(newbiz$Year), newbiz$Year == "15", "2015")
newbiz$Year <- replace(as.character(newbiz$Year), newbiz$Year == "16", "2016")
newbiz$Year <- replace(as.character(newbiz$Year), newbiz$Year == "17", "2017")
newbiz$Year <- replace(as.character(newbiz$Year), newbiz$Year == "18", "2018")
#Get the December line, which has the total of new businesses made for each zip code
newbiz_lastmonth <- newbiz %>% filter(Month==12)
#newbiz_lastmonth$Year <- as.numeric(newbiz_lastmonth$Year)


##Combine Data##
final <- inner_join(nyc_zips, newbiz_lastmonth %>%
                         select(ZipCode, Year, NewBusinesses, TotalNewBusinesses),
                       by = 'ZipCode')
final$Year <- as.numeric(final$Year)
final <- inner_join(final, real_estate_lastmonth %>% 
                         select(ZipCode, State, Year, MedianPrice, PriceChange),
                       by = c('ZipCode', 'Year'))

# convert borough and neighborhoods to factors
final$Neighborhood <- factor(final$Neighborhood)
final$Borough <- factor(final$Borough)
final$State <- factor(final$State)

# convert to tbl_df for easier investigation
final <- tbl_df(final)

save(final, file = 'final.RData')
