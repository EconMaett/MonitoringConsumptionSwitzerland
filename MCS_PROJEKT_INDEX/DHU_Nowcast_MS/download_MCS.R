#########################################################################################################
# Filename:       download_MCS.R
# Author:         Matthias Spichiger
# Email:          matthias.spichiger@student.unisg.ch
# Latest Update:  2021.05.28
#########################################################################################################
# Purpose: Download all the MCS files

#########################################################################################################
# Remove all Objects from the Global Environment
rm(list = ls(all.names = TRUE))
# Set working directory
setwd(dir = "C:/Users/matth/switchdrive/MCS_PROJEKT_INDEX/")
# Get working directory
getwd()

#########################################################################################################
# install.packages(c("data.table", "tidyverse", "tidyr", "dplyr", "ggplot2", "scales", "plotly", "rvest", "httr"))
library(data.table) # Fast aggregation of large data 
library(tidyverse)  # Install and load multiple 'tidyverse' packages in a single step
library(tidyr)      # Easily Tidy Data with 'spread()' and 'gather()' Functions
library(dplyr)      # Data handling
library(ggplot2)    # Plotting
library(scales)     # Scale functions for visualizations
library(plotly)     # Plotting
library(lubridate)  # For easy handling of dates
library(rvest)      # To access URLs
library(httr)       # To access URLs

# Set option stringsasfactors to FALSE or TRUE:
options(stringsAsFactors = TRUE)

# Clear console window:
cat("\f")

#########################################################################################################

# Generate an output path to save all the files:
OUTPUT_PATH <- "DHU_Nowcast_MS/MCS_Data/"

# Download all the files from MCS:
ov_base_url <- "https://drive.switch.ch/index.php/s/4JmvjqxKnlmrSVn/download?path=%2F&files="
ov_names <- c("MCS_Overview_Data")
ov_urls <- paste0(ov_base_url, ov_names, ".csv")

aq_base_url <- "https://drive.switch.ch/index.php/s/EbFbKsKRBfSH6LC/download?path=%2F&files="
aq_names <- c("ACQ_CountryOrigin", "ACQ_NOGA_CardholderOrigin", "ACQ_NOGA_Channel", "ACQ_NOGA_PaymentMethod",
              "ACQ_POS_Agglomeration_NOGA", "ACQ_POS_Canton", "ACQ_POS_Grossregion_NOGA", "ACQ_Transaction_Type")
aq_urls <- paste0(aq_base_url, aq_names, ".csv")

is_base_url <- "https://drive.switch.ch/index.php/s/iQp8MNDwX8TBjF2/download?path=%2F&files="
is_names <- c("ATM_Agglomeration", "ATM_Canton", "DEBIT_Canton", 
              "DEBIT_Merchanttype_Agglomeration", "DEBIT_Merchanttype_Grossregion")
is_urls <- paste0(is_base_url, is_names, ".csv")

urls <- c(ov_urls, aq_urls, is_urls)
names <- c(ov_names, aq_names, is_names)
dest_files <- paste0(OUTPUT_PATH, names, ".csv")

# Download all the files

for (i in 1:length(urls)) {
  download.file(url = urls[i], destfile = dest_files[i]) 
}