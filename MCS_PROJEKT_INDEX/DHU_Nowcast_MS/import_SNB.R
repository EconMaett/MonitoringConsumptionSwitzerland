#########################################################################################################
# Filename:       import_SNB.R
# Author:         Matthias Spichiger
# Email:          matthias.spichiger@student.unisg.ch
# Latest Update:  2021.05.12
#########################################################################################################
# Purpose: 
# Download monthly retail trade turnover index (Detailhandelsumsatz, DHU) from Swiss National Bank (SNB)
# We use the nominal total index (including fuel sales), calendar adjusted.

# We download the monthly retail turnover index from 2015:01-2019:12
# And we download the monthly retail turnover index for 2020.

# We download the monthly debit card payment turnover from the Swiss National Bank (SNB)
# From this series we use the indicator ZT_D_IZ_II_BMF which covers the debit card payments in Switzerland by cards issued in Switzerland
# It does not include cash withdrawals by debit card.


# From the project Monitoring Consumption Switzerland we download weekly data:
# Debit card transaction volume for 2020 (point of sale transactions in Switzerland by domestic card holders only):
# DEBIT_canton.csv
# ATM transactions by debit card and bank card 2020 (cash deposits and cash withdrawals in Switzerland by domestic card holders only):
# ATM_canton.csv

#########################################################################################################
# Remove all Objects from the Global Environment
rm(list = ls(all.names = TRUE))
# Set working directory
setwd(dir = "C:/Users/matth/switchdrive/MCS_PROJEKT_INDEX/")
# Get working directory
getwd()

#########################################################################################################
# install.packages(c("data.table", "tidyverse", "tidyr", "dplyr", "ggplot2", "scales", "plotly"))
library(data.table) # Fast aggregation of large data 
library(tidyverse)  # Install and load multiple 'tidyverse' packages in a single step
library(tidyr)      # Easily Tidy Data with 'spread()' and 'gather()' Functions
library(dplyr)      # Data handling
library(ggplot2)    # Plotting
library(scales)     # Scale functions for visualizations
library(plotly)     # Plotting
library(lubridate)  # For easy handling of dates

# Set option stringsasfactors to FALSE or TRUE:
options(stringsAsFactors = TRUE)

# Clear console window:
cat("\f")

#########################################################################################################
# Import Zahlungsverkehrsdaten (zavezaluba)
# URL: https://data.snb.ch/de/topics/finma#!/cube/zavezaluba
zavenzaluba <- read.csv(file = "https://data.snb.ch/api/cube/zavezaluba/data/csv/en", 
                        header = TRUE, sep = ";", skip = 3)

# Date (datum): YYYY-MM, from 2005-01 to latest update

# D0 (transaction):  Zahlungen/Bargeldbezüge 
# ZT:   Zahlungen - Total
# PGT:  Zahlungen - Präsenzgeschäft - Total
# PGKL: Zahlungen - Präsenzgeschäft - davon kontaktlos ausgelöst
# DG:   Zahlungen - Distanzgeschäft
# BT:   Bargeldbezüge - Total
# BE:   Bargeldbezüge - davon zunehmende Erfassung bankeigene Bezüge

# D1 (instrument): Zahlungskarten
# K:    Kreditkarten
# EG:   E-Geld
# D:    Debitkarten

# D2 (holder): Herkunft der Karte
# IZ:   Inländische Zahlungskarten
# AZ:   Ausländische Zahlungskarten

# D3 (location): Ort der Transaktion
# II:   Im Inland
# IA:   Im Ausland

# D4 (measure): Transatktionen und Betrag
# TT:   Transaktionen in Tausend
# BMF:  Betrag in Millionen Franken
# BTF:  Betrag pro Transaktion in Franken

# Value (value): Wert

#########################################################################################################
# Create a file called "test" that contains only:
# Zahlungen Total, Bargeldbezüge Total and Kreditkarten und Debitkarten, Betrag in Millionen Franken

# Rename variable names
zavenzaluba <- rename(.data = zavenzaluba, datum = Date, transaction = D0, instrument = D1, 
                      holder = D2, location = D3, measure = D4, value = Value)

# Generate a date format as well as year, month and day variables:
zavenzaluba$edate <- as.Date(paste(zavenzaluba$datum, "-01", sep=""))
zavenzaluba$year  <- year(zavenzaluba$edate)
zavenzaluba$month <- month(zavenzaluba$edate)
zavenzaluba$day   <- day(zavenzaluba$edate)
# save zavenzaluba as test
test <- zavenzaluba

# Generate CSV file starting at 2005-01-01
write.csv(x = test, file = "DHU_Nowcast_MS/snb_zavezaluba_2005.csv", row.names = FALSE, fileEncoding = "UTF-8")


# Generate CSV file with years starting in 2015-01-01 until latest update
snb_zavezaluba_2015 <- test[test$year > 2014, ]

# Write snb_zavezaluba_2015.csv
write.csv(x = snb_zavezaluba_2015, file = "DHU_Nowcast_MS/snb_zavezaluba_2015.csv", row.names = FALSE, fileEncoding = "UTF-8")

#########################################################################################################
# Generate a file called "test1" that contains the DHU nominal total index including fuel

# Import Konsumindikatoren from SNB
# SNB: https://data.snb.ch/de/topics/uvo#!/cube/conretail
# BFS: www.dhu.bfs.admin.ch

# import  "https://data.snb.ch/api/cube/conretail/data/csv/en"
conretail <- read.csv(file = "https://data.snb.ch/api/cube/conretail/data/csv/en", 
                        header = TRUE, sep = ";", skip = 3)

# Date (datum): YYYY-MM, from 2002-01 to latest update

# D0:   Übersicht (indicator) -> indicator

# I0:   Nominal - Total ohne Treibstoffe - Index
# I1:   Nominal - Total - Index
# I2:   Real - Total ohne Treibstoffe - Index
# I3:   Real - Total - Index

# VVP0: Nominal - Total ohne Treibstoffe - Veränderung gegenüber dem Vorjahr in %
# VVP1: Nominal - Total - Veränderung gegenüber dem Vorjahr in %
# VVP2: Real - Total ohne Treibstoffe - Veränderung gegenüber dem Vorjahr in %
# VVP3: Real - Total - Veränderung gegenüber dem Vorjahr in %

# Value (value): Wert

# Rename variable names
conretail <- rename(.data = conretail, datum = Date, indicator = D0, value = Value) 

# Keep only I1: Nominal - Total - Index
conretail <- conretail[conretail$indicator == "I1", ]

# Basicalle replace "I0" with "DHU"
conretail$indicator = "DHU"

# Generate a date format called edate and year, month and day variables
conretail$edate <- as.Date(paste(conretail$datum, "-01", sep="")) 
conretail$year <- year(conretail$edate)
conretail$month <- month(conretail$edate)
conretail$day <- day(conretail$edate)

# Write snb_conretail_2002.csv
write.csv(x = conretail, file = "DHU_Nowcast_MS/snb_conretail_2002.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Rename the variable "value" in the file "conretail" to "DHU"?????
conretail <- rename(.data = conretail, DHU = value)
# Drop the variable "indicator" which only contained the word "DHU"
conretail$indicator <- NULL

# arrange by edate
conretail <- arrange(.data = conretail, edate)

# save conretail as test1
test1 <- conretail

#########################################################################################################
# Generate a file called "test2" that contains Debit Card Data from zavezaluba (test) 
test2 <- test
# Keep only Betrag in Millionen Franken
test2 <- test2[test2$measure == "BMF", ]
# Keep only Zahlungen Total
test2 <- test2[test2$transaction == "ZT", ]
# Keep only Debitkarten
test2 <- test2[test2$instrument == "D", ]
# Keep only Inländische Zahlungskarten
test2 <- test2[test2$holder == "IZ", ]
# Keep only Transaktionen im Inland
test2 <- test2[test2$location == "II", ]
# Rename value to ZT_D_IZ_II_BMF which stands for:
# Zahlungen Total, Debitkarten, Inländische Zahlungskarten, Transaktionen im Inland, Betrag in Millionen Franken
test2 <- rename(.data = test2, ZT_D_IZ_II_BMF = value)

# arrange by edate
test2 <- arrange(.data = test2, edate)

#########################################################################################################
# Generate a file called "test3" that contains Credit Card Data from zavezaluba (test) 
test3 <- test
# Keep only Betrag in Millionen Franken
test3 <- test3[test3$measure == "BMF", ]
# Keep only Zahlungen Total
test3 <- test3[test3$transaction == "ZT", ]
# Keep only Kreditkarten
test3 <- test3[test3$instrument == "K", ]
# Keep only Inländische Zahlungskarten
test3 <- test3[test3$holder == "IZ", ]
# Keep only Transaktionen im Inland
test3 <- test3[test3$location == "II", ]
# Rename value to ZT_K_IZ_II_BMF which stands for:
# zahlungen Total, Kreditkarten, Inländische Zahlungskarten, Transaktionen im Inland, Betrag in Millionen Franken
test3 <- rename(.data = test3, ZT_K_IZ_II_BMF = value)

# arrange by edate
test3 <- arrange(.data = test3, edate)

#########################################################################################################
# Merge DHU Data (test1), Debit Card Data (test2), and Credit Card Data (test3)

# merge test1 and test2 using the variable "edate" as key 
test4 <- merge(x = test1, y = test2[, c("edate", "ZT_D_IZ_II_BMF")], by = "edate")

# merge test1 and test3 using the variable "edate" as key
test4 <- merge(x = test4, y = test3[, c("edate", "ZT_K_IZ_II_BMF")], by = "edate")

# Generate a new variable called "ZT_IZ_II_BMF" by adding "ZT_D_IZ_II_BMF" and "ZT_K_IZ_II_BMF" together
test4$ZT_IZ_II_BMF = test4$ZT_D_IZ_II_BMF + test4$ZT_K_IZ_II_BMF

# Rearrange columns
col_order <- c("datum", "edate", "year", "month", "day", "DHU", "ZT_D_IZ_II_BMF", "ZT_K_IZ_II_BMF", "ZT_IZ_II_BMF")
test4 <- test4[, col_order]

# Save test1 as payment_retailtrade.csv
write.csv(x = test4, file = "DHU_Nowcast_MS/payment_retailtrade.csv", row.names = FALSE, fileEncoding = "UTF-8")

