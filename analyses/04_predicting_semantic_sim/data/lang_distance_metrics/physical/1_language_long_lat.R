# get lon and lat for each language
# lon, lat come from glottolog (lingtypology package)

library(tidyverse)
library(lingtypology) # glottolog (2.7)

LANGPATH <- "../../../../../data/processed/lang_names/ets_to_google_langcodes_complete.csv"
OUTFILE <- "data/long_lat_by_language.csv"

# get languages
langs <- read_csv(LANGPATH) %>%
  select(ETS_lang_name, google_lang_name, iso)

# get long lat
lon_lat_by_lang <- glottolog.original %>%
  select(iso, longitude, latitude) %>%
  right_join(langs) %>%
  select(iso, ETS_lang_name, google_lang_name, longitude, latitude)

# nepali is missing inlingtypology_1.0.13; corrected from glottolog website (glottolog 3.4)
lon_lat_by_lang[lon_lat_by_lang$iso == "npi","longitude"] <- 85.00
lon_lat_by_lang[lon_lat_by_lang$iso == "npi","latitude"]  <- 28.00	

# vietnamese is incorrect in lingtypology_1.0.13; corrected from glottolog website (glottolog 3.4)
lon_lat_by_lang[lon_lat_by_lang$iso == "vie","longitude"] <- 105.77
lon_lat_by_lang[lon_lat_by_lang$iso == "vie","latitude"]  <- 20.68	

# CHECK LANG LATS
#library(ggmap)
#library(maptools)
#library(maps)
#mapWorld <- borders("world", colour="gray50", fill="white")
#mp <- ggplot() + mapWorld
#mp + geom_point(data = lon_lat_by_lang, aes(x = longitude, y = latitude), alpha = 0.5, color = "red")

write_csv(lon_lat_by_lang, OUTFILE)
  