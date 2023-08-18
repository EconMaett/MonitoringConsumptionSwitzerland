# Access BFS DHU ----
library(tidyverse)

### BFS - Detailhandelsumsatzstatistik (DHU) ----
# URL: https://www.bfs.admin.ch/bfs/de/home/statistiken/industrie-dienstleistungen/erhebungen/dhu.html

# 1) Die Zahlen des letzten aufgeführten Monats sind provisorisch.
# 2) Die Basis 2015 = 100 bezieht sich auf die unbereinigten Indizes. 
#   Durch die Bereinigung kann es in der Tabelle zu leichten Abweichungen für das Basisjahr kommen. 

# Use BFS Data Cube
url <- "https://www.pxweb.bfs.admin.ch/sq/4f36e497-86bc-4176-8ba5-54fcfa8a7d83"
download.file(url = url, destfile = "DATA/DHU.csv", method = "curl")

DHU <- read_csv(
  file = "DATA/DHU.csv", 
  skip = 1,
  locale = locale(encoding = 'latin1'), # To keep German Umlauts
  col_names = c("Index / Change", "Nominal / Real", "Merchant Category", "Month", "Raw", "Calendar Adjusted", "Seasonally Adjusted", "Trend"),
  col_types = c("ccccdddd")
  )|>
  rename(Date = Month) |> 
  mutate(
    Date = as_date(paste0(str_replace(string = Date, pattern = "M", replacement = "-"), "-15"))
  )

## DHU Categories ----
unique(DHU$`Index / Change`) # Indizes, Veränderungen
unique(DHU$`Nominal / Real`) # Nominal, Real
unique(DHU$`Merchant Category`)
# - 47: Total Detailhandel
# - 47 (ohne 473): Total Detailhandel ohne Tankstellen                                           
# - 4711, 472: Detailhandel mit Nahrungsmitteln, Getränken, Tabakwaren                           
# - 4719, 474-479: Detailhandel mit Nicht-Nahrungsmitteln (ohne Tankstellen), davon:             
#   - 474: Detailhandel mit Geräten der Informations- und Kommunikationstechnik                    
#   - 475: Detailhandel mit sonstigen Haushaltsgeräten, Textilien, Heimwerker- und Einrichtungsbedarf
#   - 476: Detailhandel mit Verlagsprodukten, Sportausrüstungen und Spielwaren"                      
#   - 477: Detailhandel mit sonstigen Gütern                                                       
#   - 478, 479: Detailhandel an Verkaufsständen und auf Märkten; Versand- und Internet-Detailhandel 
#   - 473: Detailhandel mit Tankstellen                                                       
# - Total Detailhandel ohne Treibstoffe                                                         
# - Nahrungsmittel, Getränke, Tabak                                                             
# - Bekleidung, Schuhe                                                                        
# - Übrige Warengruppen (ohne Treibstoffe)                                                       
# - Treibstoffe                                    
DHU |> 
  pivot_longer(cols = Raw:Trend, names_to = "Index", values_to = "Value") |> 
  filter(
    `Index / Change` == "Indizes", 
    `Nominal / Real` == "Nominal", 
    `Merchant Category` %in% c("47: Total Detailhandel", "47 (ohne 473): Total Detailhandel ohne Tankstellen", "473: Detailhandel mit Tankstellen"),
    Index == "Raw",
    Date >= date("2019-01-01")
  ) |> 
  # select(Date, Value) |> 
  ggplot(mapping = aes(x = Date, y = Value, color = `Merchant Category`)) +
  geom_step() +
  ylim(0, 200) +
  theme_bw()
