# Access MCS acquiring data ----
library(tidyverse)
## Data description ----

# Acquiring data by merchant type and location includes payment transactions of debit cards, 
# credit cards and mobile payments at merchants in Switzerland
# Data Provider: Worldline
# Period / Frequency: Daily from 2019-01-01

### Variables ----

# - Date: Date of transaction

# - Cardholder origin: 
#   - Domestic: Cards issued to Swiss residents
#   - Foreign: Cards issued to foreign residents

# - Cardholder origin detail: 
# - CHE, AUT, CHN, DEU, FRA, GBR, ITA, LIE, NLD, RUS, USA, RoW

# - Merchant category: 
#   - Retail: Food, beverage, tobacco
#   - Retail: Other goods
#   - Accommodation
#   - Entertainment and sports
#   - Food and Beverage services
#   - Human health services
#   - Other
#   - Personal services
#   - Professional services
#   - Retail: Fuel stations
#   - Transport services

#   Merchant Categories are defined according to the standard NOGA industry code classification. (See https://www.kubb-tool.bfs.admin.ch/en)
#   To preserve anonymity of the data we hereby cluster NOGA codes to summary categories.

# - Channel:
#   - E-comm: Transactions in e-commerce
#   - POS: Transactions at point-of-sale (POS), i.e., directly in shop
#   - All: Transactions in e-commerce and at point-of-sale (POS)

# - Payment method:
#   - Credit: Transactions with credit cards
#   - Debit: Transactions with debit cards
#   - Mobile: Mobile payments include payments executed by a mobile device and debited directly to the consumers bank account 
#     (e.g., TWINT, Alipay, WeChat Pay). They do not include payments executed by a mobile device which are debited via a credit 
#    or debit card (Google Pay, Samsung Pay, Apple Pay)”

# NOTE: Due to challenges with the classification of transactions as debit or credit-card transactions, 
# we advise to work only with the series of consolidated debit and credit card transactions.

# - Agglomeration type:
#   - 9999: Transactions in the Principality of Liechtenstein and Campione d'Italia
#   - Commuting zone
#   - Municipality: core
#   - Municipality: multiorientated
#   - Municipality: rural
#   - Principle core: city
#   - Principle core: other
#   - Secondary core

#   (See: https://www.bfs.admin.ch/bfs/de/home/grundlagen/raumgliederungen.assetdetail.349566.html)


# - Region:
#   - 9999: Transactions in the Principality of Liechtenstein and Campione d'Italia
#   - Central: LU, OW, NW, SZ, UR, ZG
#   - Eastern. AI, AR, GL, GR, SG, SH, TG
#   - Espace Mittelland: BE, FR, JU, NE, SO
#   - Lake Geneva: GE, VD, VS
#   - Northwestern: AG, BL, BS
#   - Ticino: TI
#   - Zurich: ZH

# - Canton: AG, AI, AR, BE, BL, BS, FR, GE, GL, GR, JU, LU, NE, NW, OW, UR, SG, SH, SO, SZ, TG, TI, VD, VS, ZG, ZH, 9999

#   The raw data includes an identifier for the municipality (PLZ) in which a merchant is located.
#   To preserve anonymity of the data we the location of Merchants by two separate methods:
#   1) We classify merchants by Canton / Grossregion which corresponds to the standard international NUTS-2/ NUTS-3 regional classification.
#   2) We classify merchant location by agglomeration type according to SFSO.
#   (See: https://en.wikipedia.org/wiki/NUTS_statistical_regions_of_Switzerland)

# The mapping from NOGA codes to summary merchant categories and 
# the mapping of PLZ to Grossregion / Canton and Agglomeration type are available at:
# https://drive.switch.ch/index.php/s/yLISs3KVE7ASE68?path=%2F4_MAPPING%20FILES


# - Scaled Value: Scaled transaction amount of a specific category/region on a given day
# - Scaled Number of Transactions: Scaled number of transactions reported on a given day

# To scale the data, we use the average daily turnover (in CHF or #TRX) with all means of payment methods 
# (incl. ATM withdrawals) in January 2020. 
# All data points are divided by the same number (CHF or #TRX). 
# The data thus still provide information on the **relative importance** of turnover 
# in different regions / merchant categories / agglomeration types.


### Download ----
# URL: https://monitoringconsumption.com/acquiring_data
# Switch Drive does not allow automated download of ZIP files for security reasons,
# therefore we download the files individually.

base_url <- "https://drive.switch.ch/index.php/s/yLISs3KVE7ASE68/download?path=%2F2_ACQUIRING%20DATA"

names <- c("ACQ_CountryOrigin", "ACQ_NOGA_CardholderOrigin", "ACQ_NOGA_CardholderOriginDetail", 
           "ACQ_NOGA_Channel", "ACQ_NOGA_PaymentMethod", "ACQ_POS_Agglomeration_NOGA",
           "ACQ_POS_Canton", "ACQ_POS_Grossregion_NOGA", "ACQ_Transaction_Type")

urls <- c(paste0(base_url, "&files=", names, ".csv"), paste0(base_url, "&files=", names, "_README.pdf"))

destfiles <- c(paste0("DATA/", names, ".csv"), paste0("DATA/", names, "_README.pdf"))

for (i in seq_along(urls)) {
  download.file(url = urls[i], destfile = destfiles[i], method = "curl", quiet = FALSE)
}

### Load the files ----
ACQ_CountryOrigin               <- read_csv(file = "DATA/ACQ_CountryOrigin.csv")
ACQ_NOGA_CardholderOrigin       <- read_csv(file = "DATA/ACQ_NOGA_CardholderOrigin.csv")
ACQ_NOGA_CardholderOriginDetail <- read_csv(file = "DATA/ACQ_NOGA_CardholderOriginDetail.csv")
ACQ_NOGA_Channel                <- read_csv(file = "DATA/ACQ_NOGA_Channel.csv")
ACQ_NOGA_PaymentMethod          <- read_csv(file = "DATA/ACQ_NOGA_PaymentMethod.csv")
ACQ_POS_Agglomeration_NOGA      <- read_csv(file = "DATA/ACQ_POS_Agglomeration_NOGA.csv")
ACQ_POS_Canton                  <- read_csv(file = "DATA/ACQ_POS_Canton.csv")
ACQ_POS_Grossregion_NOGA        <- read_csv(file = "DATA/ACQ_POS_Grossregion_NOGA.csv")
ACQ_Transaction_Type            <- read_csv(file = "DATA/ACQ_Transaction_Type.csv")

# The values are scaled. Does that mean you have to re-index them after aggregation?
# No, because they have been divided by the total amount of a typical day in January 2020
# You can re-scale them to your liking tho.
ACQ_CountryOrigin |> 
  summarise(Value = sum(`Scaled Value`), .by = Date) |> 
  mutate(
    Month = month(Date),
    Year = year(Date)
  ) |> 
  summarise(Value = sum(Value), .by = c(Month, Year)) |> 
  mutate(Date = date("2019-01-01") + years(Year - 2019) + months(Month - 1) + days(14)) |> 
  group_by(Year) |> 
  mutate(Mean = mean(Value)) |> 
  ungroup() |> 
  mutate(Value = Value / Mean[Year == 2019] * 100) |> 
  ggplot(mapping = aes(y = Value, x = Date)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Payments by credit card, debit card and mobile payment app at POS ins Switzerland processed by Worldline",
    subtitle = "2019 = 100",
    caption = "@econmaett. MCS, Worldline",
    x = "", y = ""
  ) +
  geom_hline(yintercept = 100, show.legend = NA, linetype = 2) +
  geom_hline(yintercept = 0, show.legend = NA) +
  geom_rect(aes(xmin = date("2020-03-16"), xmax = date("2020-05-11"), ymin = -Inf, ymax = Inf), fill = "#ce4631", alpha = 0.01, show.legend = FALSE) +
  geom_rect(aes(xmin = date("2021-01-18"), xmax = date("2021-02-28"), ymin = -Inf, ymax = Inf), fill = "#ce4631", alpha = 0.01, show.legend = FALSE) +
  geom_vline(xintercept = date("2020-03-16"), color = "black", show.legend = FALSE) +
  geom_vline(xintercept = date("2020-05-11"), color = "black", show.legend = FALSE) +
  geom_vline(xintercept = date("2021-01-18"), color = "black", show.legend = FALSE) +
  geom_vline(xintercept = date("2021-02-28"), color = "black", show.legend = FALSE) +
  geom_rect(aes(xmin = date("2019-01-01"), xmax = date("2019-12-31"), ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.01, show.legend = FALSE) +
  geom_vline(xintercept = date("2019-01-01"), color = "black", show.legend = FALSE) +
  geom_vline(xintercept = date("2019-12-31"), color = "black", show.legend = FALSE) +
  geom_step() +
  ylim(0, 200) +
  theme_bw()
  