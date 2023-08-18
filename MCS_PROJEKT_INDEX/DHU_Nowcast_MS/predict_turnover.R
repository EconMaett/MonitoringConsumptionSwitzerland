#########################################################################################################
# Filename:       predict_turnover.R
# Author:         Matthias Spichiger
# Email:          matthias.spichiger@student.unisg.ch
# Latest Update:  2021.05.12
#########################################################################################################
# Purpose: Forecast DHU based on payment card data. 

#########################################################################################################
# Remove all Objects from the Global Environment
rm(list = ls(all.names = TRUE))
# Set working directory
setwd(dir = "C:/Users/matth/switchdrive/MCS_PROJEKT_INDEX/")
# Get working directory
getwd()
# Change the language to English
Sys.setlocale("LC_TIME", "English")

#########################################################################################################
# install.packages(c("data.table", "tidyverse", "tidyr", "dplyr", "ggplot2", "scales", "plotly", "ggfortify", "xts", "zoo", "xtable", "lmtest", "forecast"))
library(data.table) # Fast aggregation of large data 
library(tidyverse)  # Install and load multiple 'tidyverse' packages in a single step
library(tidyr)      # Easily Tidy Data with 'spread()' and 'gather()' Functions
library(dplyr)      # Data handling
library(ggplot2)    # Plotting
library(scales)     # Scale functions for visualizations
library(plotly)     # Plotting
library(lubridate)  # For easy handling of dates
library(ggfortify)  # for time series plots in R
library(xts)        # for time series handling (xtends the popular zoo class)
library(zoo)        # for time series handling
library(xtable)     # to export tables to latex
library(lmtest)     # to infer std. errors
library(forecast)   # to plot inverse ARMA roots

# Set option stringsasfactors to FALSE or TRUE:
options(stringsAsFactors = TRUE)

# Clear console window:
cat("\f")

#########################################################################################################

# import delimited "d:\VadeMecum\projects\SIX\MonitoringConsumption\data\SIX.csv"
# *import delimited  "https://raw.githubusercontent.com/statistikZH/covid19monitoring/master/covid19socialmonitoring.csv", encoding(UTF-8)

# Load Overview Dataset:
mcsdat <- read.csv(file = "DHU_Nowcast_MS/MCS_data/MCS_Overview_Data.csv")
# DATE: Date of transactions, YYYY-MM-DD, from 2019-01-01 to latest update

# TRANSACTIONS:
# ATM_WITHDRAWAL:       Cash withdrawals by Swiss debit card holders at ATMs (ATMfutura) in Switzerland
# CREDIT_DOMESTIC_ECOM: Credit card transactions by Swiss cardholders to e-commerce in Switzerland
# CREDIT_DOMESTIC_POS:  Credit card transactions by Swiss cardholders at point-of-sale (POS) in Switzerland
# CREDIT_FOREIGN_ECOM:  Credit card transactions by foreign cardholders to e-commerce in Switzerland
# CREDIT_FOREIGN_POS:   Credit card transactions by foreign cardholders at point-of-sale (POS) n Switzerland
# DEBIT_DOMESTIC:       Debit card transactions by Swiss cardholders in Switzerland      
# DEBIT_FOREIGN:        Debit card transactions by foreign cardholders in Switzerland
# MOBILE:               Transactions via mobile phone by Swiss residents and foreign nationals at point-of-sale (POS) and to e-commerce in Switzerland

# (Mobile payments include payments executed on a mobile device and debited directly to the consumer bank account, e.g. TWINT,
# Alipay, WeChat Pay. They do not include payments executed via mobile device which are debited via credit or debit card, e.g.
# Google Pay, Samsung Pay or Apple Pay)

# AMOUNTCHF: Amount of transactions of specific type on a given day, in CHF.

# NUMBERTRX: Number of transactions reported on a given day.

#########################################################################################################

# First change the variable "DATE" into a date format:
mcsdat$date2 <- as.Date(mcsdat$DATE, "%Y-%m-%d")
# Check with is.Date(mcsdat$date2) 

# Keep only observations before 5th of April, 2020
mcsdat <- mcsdat[mcsdat$date2 <= as.Date("2020-04-05", "%Y-%m-%d"), ]

#########################################################################################################
# CHOOSE HERE WHICH SERIES TO MODEL: total, cash, pos

# PLOTTYPE <- "TOTAL" 
# PLOTTYPE <- "CASH" 
PLOTTYPE <- "POS" 

if (PLOTTYPE == "TOTAL") {
  # collapse (sum) value, by (date2)
  mcsdat <- mcsdat %>%
    group_by(date2) %>%
    summarise(value = sum(AMOUNTCHF))
  
} else if (PLOTTYPE == "CASH") {
  # keep if variable_short == "bezug_bargeld"
  # keep only ATM withdrawals
  mcsdat <- mcsdat[mcsdat$TRANSACTIONS == ATM_WITHDRAWAL, ]
  mcsdat <- mcsdat %>%
    group_by(date2) %>%
    summarise(value = sum(AMOUNTCHF))

} else if (PLOTTYPE == "POS") {
  # keep if variable_short == "stat_einkauf"
  # keep only POS transactions:
  # Question: Should "MOBILE" be included in POS?
  mcsdat <- mcsdat[mcsdat$TRANSACTIONS %in% c("CREDIT_DOMESTIC_POS", "CREDIT_FOREIGN_POS", "MOBILE"), ]
  mcsdat <- mcsdat %>%
    group_by(date2) %>%
    summarise(value = sum(AMOUNTCHF))

} else {
  break # We break the loop if neither "TOTAL", "CASH", or "POS" has been chosen.
  
}

#########################################################################################################
# rename "value" to "total"
mcsdat <- rename(.data = mcsdat, total = value)

# Generate a new variable "actual" equal to the existing variable "total" 
mcsdat$actual <- mcsdat$total

# replace total =. if  date2>=td(16mar2020)
# make sure that prediction does not pick up something not it should not know
# Assuming =. is setting equal to zero (or NA or "")?
mcsdat$total[mcsdat$date2 >= as.Date("2020-03-16", "%Y-%m-%d")] <- 0

# tsset date2
# Create an xts (time series) object using date2 as the time index:
# overview_ts <- xts(x = as.matrix(mcsdat), order.by = mcsdat$date2)

# Generate a variable day of week with levels "Tue" "Wed" "Thu" "Fri" "Sat" "Sun" "Mon"
mcsdat$dow <- weekdays(x = mcsdat$date2, abbreviate = TRUE)

# ** make the weekday dummies
mcsdat$mo <- 0
mcsdat$mo[mcsdat$dow == "Mon"] <- 1

mcsdat$tu <- 0
mcsdat$tu[mcsdat$dow == "Tue"] <- 1

mcsdat$we <- 0
mcsdat$we[mcsdat$dow == "Wed"] <- 1

mcsdat$th <- 0
mcsdat$th[mcsdat$dow == "Thu"] <- 1

mcsdat$fr <- 0
mcsdat$fr[mcsdat$dow == "Fri"] <- 1

mcsdat$sa <- 0
mcsdat$sa[mcsdat$dow == "Sat"] <- 1

# We do not build a variable for Sunday to avoid perfect multicollinearity (Dummy variable trap).
# This means that Sunday is the baseline here.


# *make the paydate dummy (not very smart implementation)
# gen payday = (date2==td(24jan2020)|date2==td(25jan2020)|date2==td(27jan2020)|date2==td(25feb2020)|
# date2==td(26feb2020)|date2==td(27feb2020)|date2==td(25mar2020)|date2==td(26mar2020)|date2==td(27mar2020))

# Suggestion: Make a day variable and define them as month-day pairs?
# Or check for packages?
# Or create your own package?

mcsdat$payday <- 0
mcsdat$payday[mcsdat$date2 == as.Date("2020-01-24") | mcsdat$date2 == as.Date("2020-01-25") | mcsdat$date2 == as.Date("2020-01-27") |
              mcsdat$date2 == as.Date("2020-02-25") | mcsdat$date2 == as.Date("2020-02-26") | mcsdat$date2 == as.Date("2020-02-27") |
              mcsdat$date2 == as.Date("2020-03-25") | mcsdat$date2 == as.Date("2020-03-26") | mcsdat$date2 == as.Date("2020-03-27")] <- 1

# * estimate model up to and inclusive 15mar2020
# arima total mo tu we th fr sa payday if tin(, 15mar2020), ar(1)  vce(r)

# not sure how to specifiy robust standard errors? (There are packages for that tough)
fit <- arima(x = mcsdat[mcsdat$date2 <= as.Date("2020-03-16", "%Y-%m-%d"), 2], # Estimate model upp to and inclusive 15 march 2020
               order = c(1, 0, 0), # AR(1) Model
               xreg = as.matrix(mcsdat[mcsdat$date2 <= as.Date("2020-03-16", "%Y-%m-%d"), 5:11])) # Include regressors mo tu we th fr sa payday
summary(fit)

# To find out how to extract resulting variabls from the model, use str(fit)
str(fit)

fit$coef # or coefficients(fit)
fit$sigma2
fit$residuals

# * extract coefficients to get the standarderror of predictions right
# matrix def est_coeff  = e(b)



# global phi  = est_coeff[1,9]

phi <- fit$model$phi[1] # That would be the coefficient of the AR(1) term?

# dis $phi

# global sd_ehat = est_coeff[1,10]

# Assuming we want the standard error of the AR(1) coefficient?
sd_ehat <- sqrt(fit$var.coef[1,1])

# dis $sd_ehat



# *newey total mo tu we th fr sa payday l.total  if date2<td(16mar2020), lag(1) 
# *predict ehat, res

# ** use newey west because errors are serially correlated 
# *newey total mo tu we th fr sa payday if date2<td(16mar2020), lag(1) 
# * an arima model on the errors suggests one lag is good. 
# * predict ehat1, res
# * arima ehat1 if date2<td(16mar2020), arima(1,0,0) nocon


# predict ehat, res
# ac ehat
# predict iis if date2< td(16mar2020)
# predict oos, dynamic(td(16mar2020)) y

# gen losses    = actual-oos  if date2>=td(16mar2020)
# gen cumlosses = sum(losses) if date2>=td(16mar2020)

# ** helper variables for standard deviation computaion 
# * assumes AR(1) in error term 
# gen fhorizon = date2-td(15mar2020) if date2>=td(16mar2020)

# gen pow = 2*fhorizon 
# gen pow2 = sqrt(sum($phi^pow) + 1)

# *gen   oos1=oos+$sd_ehat*sqrt($nenner*($phi^fhorizon))
# *gen   oos2=oos-$sd_ehat*sqrt($nenner*($phi^fhorizon))

# *gen   oos12=oos+2*$sd_ehat*sqrt($nenner*($phi^fhorizon))
# *gen   oos22=oos-2*$sd_ehat*sqrt($nenner*($phi^fhorizon))

# gen   oos1=oos  +$sd_ehat*pow2
# gen   oos2=oos  -$sd_ehat*pow2
# gen   oos12=oos +2*$sd_ehat*pow2
# gen   oos22=oos -2*$sd_ehat*pow2

# * make pics

# format date2 %tdddMonYY
# twoway line actual date2 if  date2>=td(16mar2020), color(black) lwidth(thick)|| ///
#   line oos date2 if  date2>=td(16mar2020), color(red)   lwidth(thick) || ///
#   rarea   oos1    oos2  date2 if  date2>=td(16mar2020),  fcolor(red%50)  lcolor(white) || ///
#   rarea   oos12   oos22 date2 if  date2>=td(16mar2020), fcolor(red%10)  lcolor(white) ///
#   xtitle("Time since Lockdown")   ///
#   legend(order(1 "Actual" 2 "Forecast" 3 "Forecast +/- SE"  4 "Forecast +/- 2SE") pos(6) row(1)) ///
#   name(graph1) title($PLOTTYPE)



# format date2 %tdddMonYY
# twoway line losses date2 if  date2>=td(16mar2020), color(black) lwidth(thick) || ///
#   line cumlosses date2 if  date2>=td(16mar2020), color(red)   lwidth(thick)  ///
#   xtitle("Time since Lockdown")   ///
#   legend(order(1 "Actual-Forecast" 2 "Cum. Losses") pos(6) row(1)) ///
#   ytitle("Mio. CHF", size(small))  name(graph3) title($PLOTTYPE)


# format date2 %tdddMonYY
# tsline  actual  oos iis ,   legend(label(1 "Total Turnover") /// 
#                                      label(3 "In-sample Predicted Turnover") label(2 "Predicted Turnover")) /// 
#   lpattern(dash) xtitle("Time",size(small)) ytitle("Mio. CHF", size(small)) name(graph2) title($PLOTTYPE)


# format date2 %td 

# exit


# keep date2 losses cumlosses 
# keep if date2>=td(16mar2020)
# cd d:\VadeMecum\projects\SIX\MonitoringConsumption\data\
# export delimited  $PLOTTYPE.losses.csv
