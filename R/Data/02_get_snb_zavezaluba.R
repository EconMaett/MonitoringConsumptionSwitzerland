# Access SNB payment transaction data ----
library(tidyverse)
library(jsonlite)

### SNB - Zahlungen und Bargeldbezüge ----

# Erläuterung: https://emi.snb.ch/de/emi/ZAVX.
# URL: https://data.snb.ch/de/topics/finma/cube/zavezaluba
# Dimensions: https://data.snb.ch/api/cube/zavezaluba/dimensions/en

# 2014-12	Between November and December 2014, the reporting population and the survey approach were adjusted, so that it is difficult to compare some of the data.
# 2017-05	Between May 2017 and September 2020, ATMs were migrated to standardised software (project name: ATMfutura). Over this period, the series ‘Domestic cash withdrawals with domestic debit cards’ therefore gradually incorporated cash withdrawals at ATMs of the card-issuing bank with Maestro, V Pay, Debit Mastercard or Visa Debit that had not been included previously. For the period of the system migration, these additional transactions are reported separately in the series ‘Of which enhanced coverage of cash withdrawals at card-issuing bank’.
# 2022-01	From January 2022, the published data include additional cash withdrawals with domestic debit cards at domestic points of sale which had erroneously not been reported earlier.
# 1) Payments of goods and services (including cash withdrawals at points of sale that cannot be reported separately from payments for goods and services).
# 2) Card-present payments cover all payments in which the transaction is initiated by the physical presence of the payment card or storage device at the attended or unattended point of sale.
# 3) All contactless payments at the point of sale are covered.
# 4) Card-not-present payments cover all payments in which the payment card is not physically present at the point of sale when the transaction is initiated. This includes, in particular, online payments and transactions arising from e-mail or telephone orders.
# 5) Cash withdrawals at ATMs and points of sale.
# 6) Due to an ATM system migration, cash withdrawals at ATMs of the card-issuing bank that had not been included previously were gradually incorporated (see break in series 2017-05 for more information).
# 7) Credit cards include both charge cards and credit cards with installment or partial payment options. Charge cards, also known as delayed-debit cards, offer the cardholder interest-free credit until the due date specified in the invoice, but do not offer the option of paying in instalments or making partial payments.
# 8) Debit cards are linked to a bank account and allow the cardholder to debit payments and cash withdrawals directly from their bank account. In addition to debit cards issued by internationally accepted payment card organisations (e.g. Maestro, V-Pay), this also includes cards which are accepted or used at national level (e.g. PostFinance Card, M-Card).
# 9) E-money describes any electronically stored monetary value constituting a claim on the issuer, which is issued against payment of funds for the purpose of carrying out payment transactions. It includes prepaid cards with a wide range of uses, but not cards whose applications are limited (such as voucher cards). In the survey, the main group obliged to report data are the providers of prepaid card products.
# 10) Domestic payment cards are deemed to be cards issued by a domestic institution, while foreign payment cards are those issued by a foreign institution.
# 11) In some cases, payments and cash withdrawals using foreign prepaid cards (e-money) issued by international payment card organisations cannot be reported separately from credit cards. Consequently, in the case of credit cards, data on foreign payment cards may include data on transactions with prepaid cards.
# 12) Domestic payment cards are deemed to be cards issued by a domestic institution, while foreign payment cards are those issued by a foreign institution. 

url_zavx_csv <- "https://data.snb.ch/api/cube/zavezaluba/data/csv/en"
url_zavx_dim <- "https://data.snb.ch/api/cube/zavezaluba/dimensions/en"

download.file(url = url_zavx_csv, destfile = "DATA/ZAVX.csv", method = "curl")

ZAVX <- readr::read_delim(file = url_zavx_csv, delim = ";", skip = 2)
ZAVX

df <- fromJSON(txt = url_zavx_dim, simplifyDataFrame = TRUE)

pattern1 <- df |> 
  as_tibble() |> 
  unnest_wider(dimensions) |> 
  select(dimensionItems) |> 
  unnest(dimensionItems) |> 
  unnest(dimensionItems) |> 
  select(id, name)

pattern2 <- df |> 
  as_tibble() |> 
  unnest_wider(dimensions) |> 
  select(dimensionItems) |> 
  unnest(dimensionItems) |> 
  unnest(dimensionItems) |> 
  unnest(dimensionItems, names_repair = "unique") |> 
  unnest_wider(dimensionItems, names_repair = "unique") |> 
  mutate(name = paste(`name...2`, `name...4`)) |> 
  select(`id...3`, name) |> 
  rename(id = `id...3`)

pattern3 <- df |> 
  as_tibble() |> 
  unnest_wider(dimensions) |> 
  select(dimensionItems) |> 
  unnest(dimensionItems) |> 
  unnest(dimensionItems) |> 
  unnest(dimensionItems, names_repair = "unique") |> 
  unnest_wider(dimensionItems, names_repair = "unique") |> 
  unnest(cols = `id...5`:`name...6`) |> 
  mutate(name = paste(`name...4`, `name...6`)) |> 
  select(`id...5`, name) |> 
  rename(id = `id...5`)

# Combine the three patterns, apply reg-ex
pattern <- rbind(pattern1, pattern2, pattern3) |> 
  mutate(id = paste0("^", id, "$"))

# Create a named vector for the pattern:
pattern_vec <- as.character(pattern$name)
names(pattern_vec) <- pattern$id

#  Use across() to apply the matching to all columns
ZAVX <- ZAVX |> 
  mutate(across(D0:D4, ~ str_replace_all(string = .x, pattern = pattern_vec)))

# Lastly change the names
names(ZAVX) <- c("Date", df$dimensions$name, "Value")

# The monthly dates are equal to the 15th of each month
ZAVX <- ZAVX |> 
  rename("Payments/Cash withdrawals" = `Payments/Cash withdrawals `) |> # Fix the typo
  mutate(Date = date(paste0(Date, "-15")))
ZAVX

# Plot the ZAVX
ZAVX |> 
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
  # filter(Date >= date("2015-01-01")) |> 
  ggplot(mapping = aes(x = Date, y = Value)) +
  geom_step() +
  ylim(0, 200) +
  theme_bw()
