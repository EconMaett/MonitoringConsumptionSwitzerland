# Access DHU from SNB data portal ----
library(tidyverse)
library(jsonlite)

### Access DHU from SNB data portal ----

#### Access the dimensions ----
library(jsonlite)
url_json_dim <- "https://data.snb.ch/api/cube/conretail/dimensions/en"

mydf <- fromJSON(txt = url_json_dim, simplifyDataFrame = TRUE)

mydf |> 
  as_tibble() |> 
  unnest_wider(dimensions) |> 
  select(dimensionItems) |> 
  unnest(dimensionItems) |> 
  unnest(dimensionItems) |> 
  select(name, dimensionItems) |> 
  unnest(dimensionItems, names_repair = "unique") |> 
  unnest(dimensionItems, names_repair = "unique") |> 
  select("name...1", "name...3", "id...4", "name...5") |> 
  rename(D0 = "id...4") |> 
  mutate(
    Name = paste(`name...1`, `name...5`, `name...3`)
  ) |> 
  select(D0, Name) -> pattern

pattern
pattern_vec <- pattern$Name
names(pattern_vec) <- pattern$D0
pattern_vec

#### Access the data ----
url_dhu_csv  <- "https://data.snb.ch/api/cube/conretail/data/csv/en"

DHU <- readr::read_delim(file = url_dhu_csv, delim = ";", skip = 2)
DHU <- DHU |> 
  mutate(D0 = str_replace_all(string = D0, pattern = pattern_vec))
DHU


# SNB does not provide the non-seasonally adjusted index!

# 1) Die Zahlen des letzten aufgeführten Monats sind provisorisch.
# 2) Die Basis 2015 = 100 bezieht sich auf die unbereinigten Indizes. 
#  Durch die Bereinigung kann es in der Tabelle zu leichten Abweichungen für das Basisjahr kommen.
# Quelle: Bundesamt für Statistik (BFS)

# Note you always have
url_csv <- paste0("https://data.snb.ch/api/cube/", cubeId, "/data/csv/en")
url_dim <- paste0("https://data.snb.ch/api/cube/", cubeId, "/dimensions/en")
