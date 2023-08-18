# NZZ Visuals style guide color palette ----

# URL: https://nzzdev.github.io/Storytelling-Styleguide/#/colors


## Level 1: Spezifische Farben ----
color_names <- c("Nacht", "Lagune", "Aquamarin", "Moos", "Pesto", "Guacamole",
                 "Nachos", "Mandarine", "Sugo", "Chianti", "Amethyst", "Flieder", "Himmel",
                 "Schokolade", "Sand", "Latte Macchiato", "Aubergine", "Regen", "Nebel")

color_values <- c("#374e8e", "#1b87aa", "#4fbbae", "#006d64", "#478c5b", "#93a345",
                  "#e3b13e", "#df7c18", "#ce4631", "#ac004f", "#ae49a2", "#a07bde", "#8aabfd",
                  "#704600", "#a08962", "#d5cdb9",
                  "#383751", "#7e7e8f", "#cdcdd1")

color_palette_1 <- color_values
names(color_palette_1) <- color_names



## Level 2: Semantische Farben ----

color_names <- c("Dark Blue", "Teal", "Turquoise", "Dark Green", "Green", "Yellow Green",
                 "Yellow", "Orange", "Orange Red", "Red", "Purple", "Violet", "Light Blue",
                 "Brown", "Beige", "Warm White",
                 "Black", "Grey", "Cool White")

color_values <- c("#374e8e", "#1b87aa", "#4fbbae", "#006d64", "#478c5b", "#93a345",
                  "#e3b13e", "#df7c18", "#ce4631", "#ac004f", "#ae49a2", "#a07bde", "#8aabfd",
                  "#704600", "#a08962", "#d5cdb9",
                  "#383751", "#7e7e8f", "#cdcdd1")

color_palette_2 <- color_values
names(color_palette_2) <- color_names


## Vordefinierte Kategoriefarben ----

### Farben für Daten zu Geschlechtsunterschieden ----

color_names <- c("male-priamry", "male-light",
                 "female-primary", "female-light")

color_values <- c("#24B39C", "#7dd1c3",
                  "#6C43C0", "#aa90de")

color_palette_mf <- color_values
names(color_palette_mf) <- color_names

### Farben zu Corona-relevanten Themen
color_names <- c("infections", "hospitalisations", "vaccinations",
                 "fatalities", "recovered", "tests")

color_values <- c("#e66e4a", "#24b39c", "#0ba9d9",
                  "#05032d", "#6c43c0", "#3952ee")

color_palette_pandemic <- color_values
names(color_palette_pandemic) <- color_names


### Farben für Energieträger ----

color_names <- c("Kohle", "Öl", "Erdgas", "Kernkraft", "Fossile",
                 "Solar", "Biomasse", "Wind", "Pumpspeicher", "Laufwasserkraft", "Erneuerbar")

color_values <- c("#383751", "#704600", "#D5CDB9", "#DF7C18", "#A08962",
                  "#E3B13E", "#478C5B", "#4FBBAE", "#374E8E", "#7E90BD", "#1B87AA")

color_palette_energy <- color_values
names(color_palette_energy) <- color_names

