#########################################################################################################
# Filename:       DHU_DCTCV analysis.R
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
# use payment_retail, clear
payment_retail <- read.csv(file = "DHU_Nowcast_MS/payment_retailtrade.csv")
# datum: YYYY-MM, from 2005-01 to latest update
# edate: YYYY-MM-01, from 2005-01-01 to latest update
# year: YYYY, from 2005 to latest year
# month: MM, from 01 to 12
# day: D, all equal to 1
# DHU: Nominal - Total - Index (I1)
# ZT_D_IZ_II_BMF: Zahlungen Total, Debitkarten, Inländische Zahlungskarten, Transaktionen im Inland, Betrag in Millionen Franken
# ZT_K_IZ_II_BMF: # Zahlungen Total, Kreditkarten, Inländische Zahlungskarten, Transaktionen im Inland, Betrag in Millionen Franken
# ZT_IZ_II_BMF: Zahlungen Total, (Kredit- und Debitkarten,) Inländische Zahlungskarten, Transaktionen im Inland, Betrag in Millionen Franken

# rename ZT_D_IZ_II_BMF to DCTV (Debit card transaction volume)
payment_retail <- rename(.data = payment_retail, DCTV = ZT_D_IZ_II_BMF)

# keep if year < 2020
payment_retail <- payment_retail[payment_retail$year < 2020, ]

# Next we want to add a a column of means by group to the original table:

# We want to generate a variable called "DHU_meanyear" which is the mean of the variable "DHU" in each "year":
# We can solve this using the "dplyr" package
payment_retail <- payment_retail %>% 
  group_by(year) %>% 
  mutate(DHU_meanyear = mean(DHU))

# We also want to generate a variable called "DCTV_meanyear"which is the mean of the variable "DCTV" in each "year":
# We achieve this with the "data.table" package:
setDT(payment_retail)[, DCTV_meanyear := mean(DCTV), by = year]

# We want to detrend the data by dividing by the yearly averages and multiplying by 100:

# Next we generate a variable "DHU_detrend" = DHU / DHU_meanyear*100
# Here, we can use R's vectorization properties:
payment_retail <- mutate(.data = payment_retail, DHU_detrend = DHU/DHU_meanyear*100)

# Next, we generate "DCTV_detrend" = DCTV / DCTV_meanyear*100
payment_retail <- mutate(.data = payment_retail, DCTV_detrend = DCTV/DCTV_meanyear*100)

# Set ggplot base theme
theme_set(theme_bw())

# Generate scatter plot of "DHU_detrend" vs "DCTV_detrend"
ggplot(data = payment_retail, mapping = aes(x = DCTV_detrend , y = DHU_detrend)) + 
  geom_abline(slope = 1, intercept = 0, color = "red", size = 1.5) +
  geom_point(shape = 21, color = "darkblue", size = 2, stroke = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(xlim = c(80, 160), ylim = c(80, 160)) +
  ggtitle("Figure 1: Correlation between de-trended DHU and de-trended DCTV") +
  xlab("Detrended DCTV") + 
  ylab("Detrended DHU")

ggsave("DHU_Nowcast_MS/Fig_1_DHU_DCTV_detrend.png", width = 20, height = 20, units = "cm")
# Delete with unlink("DHU_Nowcast_MS/Fig_1_DHU_DCTV_detrend.png")

# Generate scatter plot of "DHU_detrend" vs "month":
ggplot(data = payment_retail, mapping = aes(x = month , y = DHU_detrend)) + 
  geom_point(shape = 21, color = "darkgrey", size = 2, stroke = 1) +
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1), labels = month.abb) +
  scale_y_continuous(limits = c(40, 160), breaks = seq(40, 160, by = 40)) +
  ggtitle("Figure 2: Retail trade turnover index (DHU) vs Month, de-trended") +
  xlab("Month") + 
  ylab("Detrended DHU")

ggsave("DHU_Nowcast_MS/Fig_2_DHU_detrend_month.png", width = 20, height = 10, units = "cm")
# Delete with unlink("DHU_Nowcast_MS/Fig_2_DHU_detrend_month.png")


# Generate scatter plot of "DCTV_detrend" vs "month":
ggplot(data = payment_retail, mapping = aes(x = month , y = DCTV_detrend)) + 
  geom_point(shape = 21, color = "darkblue", size = 2, stroke = 1) +
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1), labels = month.abb) +
  scale_y_continuous(limits = c(40, 160), breaks = seq(40, 160, by = 40)) +
  ggtitle("Figure 3: Debit card transaction volume (DCTV) vs Month, de-trended") +
  xlab("Month") + 
  ylab("Detrended DCTV")

ggsave("DHU_Nowcast_MS/Fig_3_DCTV_detrend_month.png", width = 20, height = 10, units = "cm")
# Delete with unlink("DDHU_Nowcast_MS/Fig_3_DCTV_detrend_month.png")

# Next we generate "log_DHU_detrend" = ln(DHU_detrend)
payment_retail <- mutate(.data = payment_retail, log_DHU_detrend = log(DHU_detrend))

# Then we generate "log_DCTV_detrend" = ln(DCTV_detrend)
payment_retail <- mutate(.data = payment_retail, log_DCTV_detrend = log(DCTV_detrend))

# scatter log_DHU_detrend log_DCTV_detrend
ggplot(data = payment_retail, mapping = aes(x = log_DCTV_detrend , y = log_DHU_detrend)) + 
  geom_abline(slope = 1, intercept = 0, color = "red", size = 1.5) +
  geom_point(shape = 21, color = "darkblue", size = 2, stroke = 1) +
  coord_cartesian(xlim = c(4, 6), ylim = c(4, 6)) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Figure 4: Correlation between log(de-trended DHU) and log(de-trended DCTV)") +
  xlab("log(Detrended DCTV)") + 
  ylab("log(Detrended DHU)")

ggsave("DHU_Nowcast_MS/Fig_4_DHU_DCTV_logdetrend.png", width = 20, height = 20, units = "cm")
# Delete with unlink("DHU_Nowcast_MS/Fig_1_DHU_DCTV_detrend.png")

# outsheet using payment_retail_detrend.csv , comma  replace
write.csv(x = test4, file = "DHU_Nowcast_MS/payment_retailtrade.csv", row.names = FALSE, fileEncoding = "UTF-8")


# reg log_DHU_detrend log_DCTV_detrend
# Use forecast::tslm() instead of lm if you have time series data
lm <- lm(formula = log_DHU_detrend ~ log_DCTV_detrend, data = payment_retail)
summary(lm)
# Coefficient is 0.71737 and highly significant
lm$coefficients[c(2)]

# reg log_DHU_detrend log_DCTV_detrend if year > 2009
lm_2009 <- lm(formula = log_DHU_detrend ~ log_DCTV_detrend, data = payment_retail[payment_retail$year > 2009, ])
summary(lm_2009)
# Coefficient is 0.72803 and highly significant
lm_2009$coefficients[c(2)]

# reg log_DHU_detrend log_DCTV_detrend if year > 2014
lm_2014 <- lm(formula = log_DHU_detrend ~ log_DCTV_detrend, data = payment_retail[payment_retail$year > 2014, ])
summary(lm_2014)
# Coefficient is 0.8530084 and highly significant
lm_2014$coefficients[c(2)]
