# Inflation adjustment ----
library(tidyverse)
library(readxl)
library(jsonlite)
library(seasonal)
library(tsbox)

# Get CPI either form BFS
# Use overall inflation to adjust the raw SNB payment transactions

# Use the single price indices to adjust the product categories in the MCS ACQ data

# BFS ----

# Detailed monthly results
# Warenkorbstruktur 2020
# Basis Dezember 2020 = 100

# sheet INDEX_m: Index, Monatswert
# Code, PosNo, PosType, Level, COICOP, Position_D, Gewicht 2023, Mai 22, Jun 22

# sheet VAR_m-1: Veränderungsrate gegenüber Vormonat in %
# sheet VAR_m-12: Veränderungsrate gegenüber Vorjahresmonat (%)
# sheet CONTR_m: Beitrag zur Monatsteuerung
# sheet INDEX_y: Index, Jahresdurchschnitte
# sheet VAR_y-1: Jahresdurchschnittliche Teuerung (%)
# sheet DE_FR_IT_EN: Translations
bfs_cpi_url <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/27085446/master"
download.file(url = bfs_cpi_url, destfile = "DATA/CPI.xlsx", method = "curl")

CPI <- readxl::read_excel(path = "DATA/CPI.xlsx", sheet = "INDEX_m", skip = 3, na = c("..."))

CPI <- CPI |> 
  select(-c("Position_F", "PosTxt_F", "Posizione_I", "PosTxt_I", "PosTxt_D", "Position_D")) |> 
  slice(1:(n() - 62))

CPI <- CPI |> 
  pivot_longer(
    cols = -c("Code", "PosNo", "PosType", "Level", "COICOP",  "Item_E", "PosTxt_E",  "2023"), 
    names_to = "Date",
    values_to = "Value")

# Get the correct date
CPI <- CPI |> 
  mutate(Date = as.numeric(Date)) |> 
  mutate(Date = Date - 30286) |> 
  mutate(Date = date("1982-12-1") + days(Date) + days(14)) |> 
  select(Item_E, Date, Value)

ACQ_NOGA_CardholderOrigin |> 
  filter(`Merchant category` == "Retail: Fuel stations") |> 
  summarise(Value = sum(`Scaled Value`), .by = Date) |> 
  mutate(Month = month(Date), Year = year(Date)) |> 
  summarise(Value = sum(Value), .by = c(Month, Year)) |> 
  mutate(Date = date("2019-01-01") + years(Year - 2019) + months(Month - 1) + days(14)) |> 
  group_by(Year) |> 
  mutate(Mean = mean(Value)) |> 
  ungroup() |> 
  mutate(Value = Value / Mean[Year == 2019] * 100) |> 
  select(Date, Value) |> 
  left_join(y = CPI |> 
              filter(Item_E == "Fuel") |> 
              rename(CPI = Value) |> 
              select(Date, CPI), by = "Date") |> 
  select(Date, CPI, Value) |> 
  mutate(CPI = CPI / CPI[Date == "2019-01-15"] * 100) |>  # Re-index CPI so that January 2019 = 100
  mutate(Decimal_form = CPI / 100) |> # Bring CPI in decimal form
  mutate(ValueReal = Value / Decimal_form) |> 
  select(Date, Value, ValueReal) |> 
  rename(ValueNom = Value) |> 
  pivot_longer(cols = ValueNom:ValueReal, names_to = "Index", values_to = "Value") |> 
  ggplot(mapping = aes(x = Date, y = Value, color = Index)) +
  geom_step() +
  ylim(0, 200) +
  theme_bw()

CPI <- CPI |> 
  filter(Item_E  %in% c("Food and non-alcoholic beverages", "Alcoholic beverages and tobacco",
                         "Fuel", "Total", "Transport services", "Other services", "Personal care", 
                        "Accommodation", "Recreation and culture", "Restaurants and hotels", "Healthcare")) |> 
  pivot_wider(names_from = Item_E, values_from = Value) |> 
  mutate(`Food, beverage, tobacco` = (10.991 * `Food and non-alcoholic beverages` + 2.892 * `Alcoholic beverages and tobacco`) / (10.991 + 2.892)) |> 
  pivot_longer(cols = Total:`Food, beverage, tobacco`, names_to = "Category", values_to = "Value") |> 
  group_by(Category) |> 
  mutate(Value = Value / Value[Date == "2019-01-15"] * 100) |>  # Re-index CPI so that January 2019 = 100
  ungroup() |> 
  mutate(CPI = Value / 100) |>  # Bring CPI in decimal form
  select(Date, Category, CPI) |> 
  mutate(Category = paste0(Category, "_CPI")) |> 
  pivot_wider(names_from = Category, values_from = CPI)

CPI

ACQ <- ACQ_NOGA_CardholderOrigin |> 
  group_by(Date, `Merchant category`) |> 
  summarise(Value = sum(`Scaled Value`)) |> 
  ungroup() |> 
  mutate(Month = month(Date), Year = year(Date)) |> 
  group_by(Month, Year, `Merchant category`) |> 
  summarise(Value = sum(Value)) |> 
  ungroup() |> 
  mutate(Date = date("2019-01-01") + years(Year - 2019) + months(Month - 1) + days(14)) |> 
  select(Date, `Merchant category`, Value) |> 
  group_by(`Merchant category`) |> 
  mutate(Value = Value / Value[Date == "2019-01-15"] * 100) |> # Re-index ACQ too? Not clear
  ungroup() |> 
  mutate(`Merchant category` = paste0(`Merchant category`, "_N")) |> 
  pivot_wider(names_from = `Merchant category`, values_from = Value)


# Retail: Food, beverage, tobacco -> Food, beverage, tobacco = (10.991 * Food and non-alcoholic beverages + 2.892 * Alcoholic beverages and tobacco) / (10.991 + 2.892)
# Retail: Fuel stations           -> Fuel
# Retail: Other goods             -> Total
# Transport services              -> Transport services
# Personal services               -> Other Services
# Professional services           -> Personal care
# Accommodation                   -> Accommodation      
# Entertainment and sports        -> Recreation and culture
# Food and beverage services      -> Restaurants and hotels
# Human health services           -> Health care
# Other                           -> Total

# We have to create two additional groups:
# - Retail: Total = Retail: Food, beverage, tobacco + Retail: Fuel stations + Retail: Other goods
# - Retail: Total without fuel stations = Retail: Food, beverage, tobacco + Retail: Other goods  
# After aggregation, we have to re-index these series.

# Combine CPI and ACQ


# Rename the long names:
ACQ <- ACQ |> 
  rename(
    EntertSports_N  = `Entertainment and sports_N`,
    FoodBevS_N      = `Food and beverage services_N`,
    HealthS_N       = `Human health services_N`,
    PersonalS_N     = `Personal services_N`,
    ProfessionalS_N = `Professional services_N`,
    RetFoBeTo_N     = `Retail: Food, beverage, tobacco_N`,
    RetFuel_N       = `Retail: Fuel stations_N`,
    RetOth_N        = `Retail: Other goods_N`,
    TransportS_N    = `Transport services_N`
    )

DF <- left_join(x = ACQ, y = CPI, by = "Date")
DF |> names()

DF <- DF |> 
  mutate(
    RetFoBeTo_R     = RetFoBeTo_N / `Food, beverage, tobacco_CPI`,
    RetFuel_R       = RetFuel_N / Fuel_CPI,
    RetOth_R        = RetOth_N / Total_CPI,
    TransportS_R    = TransportS_N / `Transport services_CPI`,
    PersonalS_R     = PersonalS_N / Total_CPI,
    ProfessionalS_R = ProfessionalS_N / `Personal care_CPI`,
    Accommodation_R = Accommodation_N / Accommodation_CPI,
    EntertSports_R  = EntertSports_N / `Recreation and culture_CPI`,
    FoodBevS_R      = FoodBevS_N / `Restaurants and hotels_CPI`,
    HealthS_R       = HealthS_N / Healthcare_CPI,
    Other_R         = Other_N / Total_CPI
  ) |>
  select(Date, contains("_R"), contains("_N")) |> 
  mutate(
    RetTot_N       = RetFoBeTo_N + RetFuel_N + RetOth_N,
    RetTot_R       = RetFoBeTo_R + RetFuel_R + RetOth_R,
    RetTotwoFuel_N = RetFoBeTo_N + RetOth_N,
    RetTotwoFuel_R = RetFoBeTo_R + RetOth_R
  ) |> 
  mutate(
    RetTot_N = RetTot_N / RetTot_N[Date == "2019-01-15"] * 100,
    RetTot_R = RetTot_R / RetTot_R[Date == "2019-01-15"] * 100,
    RetTotwoFuel_N = RetTotwoFuel_N / RetTotwoFuel_N[Date == "2019-01-15"] * 100,
    RetTotwoFuel_R = RetTotwoFuel_R / RetTotwoFuel_R[Date == "2019-01-15"] * 100
  ) |> 
  pivot_longer(cols = !c("Date"), names_to = c("Merchant category", "Index"), names_sep = "_", values_to = "Value")
DF

# Check difference for overall retail sales:
DF |> 
  filter(`Merchant category` == "RetTot") |> 
  ggplot(mapping = aes(x = Date, y = Value, color = Index)) +
  geom_step() +
  ylim(0, 200) +
  theme_bw()

DF |> 
  filter(Index == "N") |> 
  filter(`Merchant category` %in% c("RetTot")) |> 
  ggplot(mapping = aes(x = Date, y = Value, group = `Merchant category`, color = `Merchant category`)) +
  geom_step(size = 1) +
  ylim(0, 200) +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    y = "", x = ""
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_hline(yintercept = 100, show.legend = NA, linetype = 2) +
  geom_hline(yintercept = 0, show.legend = NA) +
  geom_rect(aes(xmin = date("2020-03-16"), xmax = date("2020-05-11"), ymin = -Inf, ymax = Inf), fill = "#df7c18", alpha = 0.01, show.legend = FALSE) +
  geom_rect(aes(xmin = date("2021-01-18"), xmax = date("2021-02-28"), ymin = -Inf, ymax = Inf), fill = "#df7c18", alpha = 0.01, show.legend = FALSE) +
  geom_vline(xintercept = date("2020-03-16"), color = "black", show.legend = FALSE) +
  geom_vline(xintercept = date("2020-05-11"), color = "black", show.legend = FALSE) +
  geom_vline(xintercept = date("2021-01-18"), color = "black", show.legend = FALSE) +
  geom_vline(xintercept = date("2021-02-28"), color = "black", show.legend = FALSE) +
  theme_bw()

DF |> names()
unique(DF$`Merchant category`)
unique(DF$Index)

# After having real and nominal values, we need:
# - Calendar adjustment -> For number of "Trading days" aka "Workdays" or
# - Seasonal adjustment -> X12-ARIMA
# - Trend

mydata <- DF |> 
  mutate(id = paste0(`Merchant category`, "_", Index)) |> 
  mutate(Date = Date - days(14)) |> # time must be the first date of the period
  rename(time = Date, value = Value) |> 
  select(id, time, value) |> 
  arrange(id, time) |> 
  pivot_wider(names_from = id, values_from = value)

mydata |> head()
mydata |> tail()




myts <- ts(data = mydata, start = c(2019, 1), 
           frequency = 12, 
           class = c("mts", "ts", "matrix", "array")
)
class(myts)
str(myts)
tail(myts)

plot(acf(myts[, "RetTot_N"]))
plot(pacf(myts[, "RetTot_N"]))

# Try including a level shift in 2020-5
# Note: For forecasts and backcasts, the serie smust be longer
lshift <- ts(-1, start = c(2019, 1), end = c(2026, 8), freq = 12)
window(lshift, start = c(2020, 5)) <- 0
seas(myts, xreg = lshift, outlier = NULL)
seas(unemp, xreg = lshift, outlier = NULL)

# This is the easiest specification that seemed to work:
m <- seas(myts, 
          x11 = "",
          transform.function = "log",
          regression.aictest = NULL,
          arima.model = "(0 1 1)(0 1 1)12",
          regression.variables = "td"
          )
# This might be the best adjustment. Trading day is included, 
# because SFSO adjusts DHU for trading days.
m

m <- seas(myts, 
          x11 = "",
          transform.function = "log", 
          regression.aictest = NULL,
          arima.model = "(0 1 1)(0 1 1)12",
          xreg = lshift
)

# Or specify regression.variables = "ls2020.05" -> This looks better
m <- seas(myts, 
          x11 = "",
          transform.function = "log", 
          regression.aictest = NULL,
          arima.model = "(0 1 1)(0 1 1)12",
          regression.variables = "ls2020.05"
)
# I like the above model the most...

final(m)
series(m$Retail_Fuelstations_, "rsd")

m <- seas(myts,
     x11 = "",
     transform.function = "log",
     regression.variables = c("td"),
     regression.aictest = NULL
)

m

m_ret <- m$RetTot_N
summary(m_ret)
plot(m_ret, trend = TRUE)

pacf(resid(m_ret))
spectrum(diff(resid(m_ret)))
plot(density(resid(m_ret)))
qqnorm(resid(m_ret))
monthplot(m_ret)
monthplot(m_ret, choice = "irregular")
ts.plot(series(m_ret, "forecast.forecasts"))

view(m_ret)


# For the SNB payments data (Credit and debit card transactions),
# do a calendar adjustment
# and seasonal adjustment and Trend accordingly.

# Die Resultate werden in indexierter Form (2015 = 100) sowohl nominal als auch real bereitgestellt. 
# Die realen Werte ergeben sich aus der Preisbereinigung (Deflationierung) der nominalen Werte. 
# Hierzu wird der Landesindex der Konsumentenpreise (LIK) verwendet.

# Um saisonal bedingte Schwankungen aus der Zeitreihe auszuschliessen, werden die Daten saisonbereinigt. 
# Dies geschieht mit der Methode X12-ARIMA. 
# Dabei wird jeweils die gesamte Zeitreihe neu berechnet. -> Dh die Daten können sich ändern!

# Alle Zeitreihen werden zudem um die Anzahl Kalendertage 
# (nicht jeder Monat hat gleich viele Verkaufs- und Feiertage) bereinigt. 

# Die angewandte Methode zur Kalenderbereinigung besteht darin, 
# die Kalendereffekte mittels eines Regressionsmodells zu schätzen.

# Das Modell berechnet für jede Serie ein durchschnittliches Gewicht der einzelnen Wochentage 
# und wendet diese dann auf jeden Monat an. 

# Mit den daraus berechneten Monatsfaktoren werden die Umsätze bereinigt. 
# Die Bereinigung des Dezembers kann etwas verzerrt sein, 
# da der Bereinigungsfaktor auf die gesamten Dezember-Umsätze angewandt wird, 
# obwohl die Weihnachtsumsätze nicht von den Wochentagen abhängen. 

# Im Dezember 2016 (ebenso 2011 und 2005) ist dieser Effekt besonders stark, 
# da Weihnachten/Stephanstag auf Sonntag/Montag fielen und dieser Monat somit 
# besonders viele umsatzstarke Wochentage aufweist.
