# MCS Acquiring Data ----

## Load packages ----
library(tidyverse)
library(readxl)
library(ggtext)

### ISO week dates -----
# Note to weekly aggregates:
# We aggregate the data by ISO-week to account for year-end / year-start effects:
# ISO-week 01 2019: 2018-12-31 - 2019-01-06
# ISO-week 01 2020: 2019-12-30 - 2020-01-05
# ISO-week 01 2021: 2021-01-04 - 2021-01-10
# ISO-week 01 2022: 2022-01-03 - 2022-01-09
# ISO-week 01 2023: 2023-01-02 - 2023-01-08
# ISO-week 01 2024: 2024-01-01 - 2024-01-07

# ISO week date is a leap week calendar that is part of ISO 8601.
# An ISO-week-numbering year has 52 or 53 full weeks, that is,
# 364 or 371 days instead of the usual 365 or 366 days
# The 53 week year occurs on all years that have Thursday as the 1st of January
# and on leap years that start on Wednesday the 1st.
# Weeks start with Monday and end on Sunday.
# Each week's year is the Gregorian year in which the Thursday falls.
# The first week of the year, hence, always contains the 4th of January.
# ISO week year numbering therefore usually deviates by 1 from the Gregorian for
# days close to the 1st of January.

# The ISO 8601 definition for week 01 is the week with the first Thursday
# of the Gregorian year (i.e. of January) in it.

# - It is the first week with a majority (4 or more) of its days in January.
# - Its first day is the MOnday nearest to 1 January.
# - It has 4 January in it.

# The earleist possible first week extends from 29 December in the previous
# Gregorian year to Sunday 4 January

# The latest possible first week extends fro Monday 4 January to Sunday 10 January.

# It has the year's first working day in it if Saturdays, Sundays, and 1 January
# are holidays.

## Mapping MCS ACQ to DHU ----

# 47: Total Detailhandel = Retail: Food, beverage, tobacco + Retail: Fuel stations + Retail: Other goods
# 47 (ohne 473): Total Detailhandel ohne Tankstellen = Retail: Food, beverage, tobacco + Retail: Other goods
# 4711, 472: Detailhandel mit Nahrungsmitteln, Getränken, Tabakwaren  = Retail: Food, beverage, tobacco
# 4719, 474-479: Detailhandel mit Nicht-Nahrungsmitteln (ohne Tankstellen), davon = Retail: Other goods
# 473: Detailhandel mit Tankstellen = Retail: Fuel stations
# Total Detailhandel ohne Treibstoffe = Retail: Food, beverage, tobacco + Retail: Other goods
# Nahrungsmittel, Getränke, Tabak = Retail: Food, beverage, tobacco
# Übrige Warengruppen (ohne Treibstoffe) = Retail: Other goods
# Treibstoffe  = Retail: Fuel stations

DHU2 <- DHU |> 
  pivot_longer(cols = Raw:Trend, names_to = "Index", values_to = "Value") |> 
  filter(
    `Index / Change` == "Indizes", 
    `Nominal / Real` == "Nominal", 
    `Merchant Category` == "47 (ohne 473): Total Detailhandel ohne Tankstellen",
    Index == "Raw",
    Date >= date("2015-01-01")
  ) |> select(Date, Value)

DHU2 |> 
  select(Date, Value) |> 
  ggplot(mapping = aes(x = Date, y = Value)) +
  geom_step() +
  ylim(0, 200) +
  labs(
    title = "DHU Index Nominal 47 (ohne 473): Total Detailhandel ohne Tankstellen",
    subtitle = "2015 = 100",
    caption = "Bundesamt für Statistik",
    x = "", y = ""
    ) +
  theme_bw()

ZAVX2 <-  ZAVX |> 
  filter(
    `Payments/Cash withdrawals` == "Payments Total", 
    `Place of transaction` == "Domestic",
    `Payment cards` != "E-money",
    `Transactions and amount` == "Amount in CHF millions"
  ) |> 
  summarise(Value = sum(Value), .by = Date) |> 
  mutate(Year = year(Date)) |> 
  group_by(Year) |> 
  mutate(Mean = mean(Value)) |> 
  ungroup() |> 
  mutate(Value = Value / Mean[Year == 2015] * 100) |> 
  filter(Date >= date("2015-01-01")) |> 
  select(Date, Value)

ZAVX2 |> 
  ggplot(mapping = aes(x = Date, y = Value)) +
  geom_step() +
  labs(
    title = "Payments with credit and debit cards in Switzerland",
    subtitle = "2015 = 100", 
    caption = "Source: Swiss National Bank",
    x = "", y = ""
  ) +
  ylim(0, 200) +
  theme_bw()


DHU2
ZAVX2

dim(DHU2)   # 102 months 
dim(ZAVX2)  # 101 months
tail(DHU2)  # DHU includes 2023-06 already
tail(ZAVX2) # ZAVX only includes 2023-05

# Note: Payment card transactions by SNB are reported AFTER DHU!
# To NOWCAST the DHU, we NEED the MCS acquiring data from Worldline!

# To get the correlation, we left-join ZAVX and DHU so they have the same length.
DF <- left_join(x = ZAVX2, y = DHU2, by = "Date") |> 
  rename(ZAVX = Value.x, DHU = Value.y)

round(cor(DF$DHU, DF$ZAVX), 2) # 0.71

DF |> 
  pivot_longer(cols = DHU:ZAVX, names_to = "Index", values_to = "Value") |> 
  ggplot(mapping = aes(x = Date, y = Value, group = Index, color = Index)) +
  labs(
    title = "<span style = 'color: #478c5b;'>DHU</span> vs <span style = 'color: #8aabfd;'>credit and debit card payments in Switzerland</span> (both unadjusted)",
    subtitle = "2015 = 100",
    caption = "@econmaett. DHU: BFS. ZAVX: SNB",
    x = "", y = ""
  ) +
  scale_color_manual(values = c("#478c5b", "#8aabfd")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ylim(0, 200) +
  geom_hline(yintercept = 100, show.legend = NA, linetype = 2) +
  geom_hline(yintercept = 0, show.legend = NA) +
  geom_rect(aes(xmin = date("2020-03-16"), xmax = date("2020-05-11"), ymin = -Inf, ymax = Inf), fill = "#df7c18", alpha = 0.01, show.legend = FALSE) +
  geom_rect(aes(xmin = date("2021-01-18"), xmax = date("2021-02-28"), ymin = -Inf, ymax = Inf), fill = "#df7c18", alpha = 0.01, show.legend = FALSE) +
  geom_vline(xintercept = date("2020-03-16"), color = "black", show.legend = FALSE) +
  geom_vline(xintercept = date("2020-05-11"), color = "black", show.legend = FALSE) +
  geom_vline(xintercept = date("2021-01-18"), color = "black", show.legend = FALSE) +
  geom_vline(xintercept = date("2021-02-28"), color = "black", show.legend = FALSE) +
  geom_rect(aes(xmin = date("2015-01-01"), xmax = date("2015-12-31"), ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.01, show.legend = FALSE) +
  geom_vline(xintercept = date("2015-01-01"), color = "black", show.legend = FALSE) +
  geom_vline(xintercept = date("2015-12-31"), color = "black", show.legend = FALSE) +
  geom_step(size = 1.1) +
  theme_bw() +
  theme(plot.title = ggtext::element_markdown(), legend.position = "none")

# 1) Erster Lockdown in der Schweiz (16. März bis 11. Mai 2020)
# 2) Zweite Ladenschliessungen in der Schweiz (18. Januar bis 28. Februar 2021)

