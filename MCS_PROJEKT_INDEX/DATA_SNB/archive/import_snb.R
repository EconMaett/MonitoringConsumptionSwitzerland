#############################################################################################################
# R file: Import SNB Data
# Author: Matthias Spichiger
# Email:  matthias.spichiger@student.unisg.ch
# Last Updated: 23.07.2020
#############################################################################################################
# Set system to English, so that month abbreviations %b are read correctly:
Sys.setlocale("LC_TIME", "English")
# Remove all Objects from the Global Environment
rm(list = ls(all.names = TRUE))
# Set option stringsasfactors to FALSE:
options(stringsAsFactors = FALSE)

# Set working directory
setwd(dir = "/Users/jones/switchdrive/MCS_SHARE/DATA_SNB")
# Get working directory
getwd()

install.packages(c("data.table", "tidyverse", "tidyr", "dplyr"))
library(data.table) # Fast aggregation of large data 
library(tidyverse)  # Install and load multiple 'tidyverse' packages in a single step
library(tidyr)      # Easily Tidy Data with 'spread()' and 'gather()' Functions
library(dplyr)      # Data handling


##################################
### Import Zahlungsverkehrsdaten
# ********************************

# Read in the data
data <- read.csv(file = "https://data.snb.ch/api/cube/zavezaluba/data/csv/en", 
                 sep = ";", dec = ".", skip = 3, header = TRUE)
# Make a data.table object for faster processing
data <- as.data.table(data)

# drop in 1, drop in 1 ??

data <- data %>%
  # rename v1 datum, rename v2 transaction, rename v3 instrument, rename v4 holder, rename v5 location, rename v6 measure, rename v7 value
  rename(datum = Date, transaction = D0, instrument = D1, holder = D2, location = D3, measure = D4, value = Value) %>%
  # gen edate = date(datum, "YM")
  mutate(edate = as.Date(paste0(datum, "-01"), format = "%Y-%m-%d"),
         # gen year = year(edate)
         year = lubridate::year(edate),
         # gen month = month(edate)
         month = lubridate::month(edate),
         # gen day = day(edate)
         day = lubridate::day(edate))

# drop in 1, destring value, replace ??
  
# save test, replace
save(data, file = "test.RData")

data <- data %>%
  # keep if measure=="BMF"
  filter(measure == "BMF" ) %>%
  # drop if transaction=="PGT", drop if transaction=="PGKL", drop if transaction=="DG", drop if transaction=="BE", drop if instrument=="EG"
  filter(transaction != "PGT" & transaction != "PGKL" & transaction != "DG" & transaction != "BE" & transaction != "EG")

# table year, outsheet using snb_zavezaluba_2005.csv , comma  replace
data <- arrange(.data = data, year)
write.csv(data, file = "snb_zavezaluba_2005_test.csv", sep = ",")

# use test, clear
load(file = "test.RData")

# keep if year>2014
data <- filter(.data = data, year > 2014)

# table year, outsheet using snb_zavezaluba_2015.csv , comma  replace
write.csv(data, file = "snb_zavezaluba_2015_test.csv", sep = ",")


# #############################
# # ** Import Konsumindikatoren
# # ***************************
# # import delimited "https://data.snb.ch/api/cube/conretail/data/csv/en", delimiter(";") encoding(UTF-8) clear 
# data <- read.csv(file = "https://data.snb.ch/api/cube/conretail/data/csv/en", 
#                  skip = 3, header = TRUE, sep = ";")
# 
# # drop in 1, drop in 1 ??
# 
# data <- data %>%
#   # rename v1 datum, rename v2 indicator, rename v3 value
#   rename(datum = Date, indicator = D0, value = Value) %>%
#   # drop in 1, estring value, replace ?? 
#   # keep if indicator=="I1"
#   filter(indicator == "I1") %>%
#   # replace indicator = "DHU"
#   mutate(indicator = "DHU",
#          # gen edate = date(datum, "YM")
#          edate = as.Date(paste0(datum, "-01"), format = "%Y-%m-%d"),
#          # gen year= year(edate)
#          year = lubridate::year(edate),
#          # gen month=month(edate)
#          month = lubridate::month(edate),
#          # gen day= day(edate)
#          day = lubridate::day(edate)
#          ) 
# 
# # table year, outsheet using snb_conretail_2002.csv , comma  replace
# write.csv(data, file = "snb_conretail_2002_test.csv", sep = ",")
# 
# ######################################
# # *** merge konsum & zahlungsverkehr"
# # ***********************************
# 
# data <- data %>%
#   # rename value DHU
#   mutate(value = "DHU") %>%
#   # drop indicator
#   select(-indicator) %>%
#   # sort edate
#   arrange(edate)
# # save test1, replace
# save(data, file = "test1.RData")
# 
# 
# # use test, clear
# load(file = "test.RData")
# 
# data <- data %>%
#   # keep if measure=="BMF", keep if transaction=="ZT", keep if instrument=="D", keep if holder=="IZ", keep if location=="II"
#   filter(measure == "BMF" & transaction == "ZT" & instrument == "D" & holder == "IZ" & location == "II") %>%
#   # rename value ZT_D_IZ_II_BMF
#   rename(ZT_D_IZ_II_BMF = value) %>%
#   # sort edate
#   arrange(edate)
# 
# # save test2, replace
# save(data, file = "test2.RData")
# 
# # use test, clear
# load(file = "test.RData")
# 
# data <- data %>%
#   # keep if measure=="BMF", keep if transaction=="ZT", keep if instrument=="K", keep if holder=="IZ", keep if location=="II"
#   filter(measure == "BMF" & transaction == "ZT" & instrument == "K" & holder == "IZ" & location == "II") %>%
#   # rename value ZT_K_IZ_II_BMF
#   rename(ZT_K_IZ_II_BMF = value) %>%
#   # sort edate
#   arrange(edate)
# 
# # save test3, replace
# save(data, file = "test3.RData")


# use test1, replace
# load(file = "test1.RData")

# merge 1:1 edate using test2
# data2 <- merge.data.frame(x = data, y = d)

# drop if _merge <3
# drop _merge
# merge 1:1 edate using test3
# drop if _merge <3
# drop _merge
# drop measure location holder instrument transaction
# label var ZT_D_IZ_II_BMF "Debit card payments"
# label var ZT_K_IZ_II_BMF "Credit card payments"
# gen ZT_IZ_II_BMF = ZT_D_IZ_II_BMF + ZT_K_IZ_II_BMF
# label var ZT_IZ_II_BMF "Total card payments"
# label var DHU "Retail trade index"
# order datum edate year month day
# outsheet using payment_retailtrade.csv , comma  replace
# save payment_retail, replace



