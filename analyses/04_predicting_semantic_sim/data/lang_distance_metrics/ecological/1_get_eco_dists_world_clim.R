# save eco distances from worldclim data (https://www.worldclim.org/)
# variables: mean/sd temp; mean/sd precipitation

library(tidyverse)

OUTFILE <- "data/eco_language_distance.csv"

LONG_LAT <- "../physical/data/long_lat_by_language.csv"
long_lat <- read_csv(LONG_LAT)

climate_data <- raster::getData("worldclim", var = "bio", res=2.5)

points <-  long_lat %>%
  rename(x = longitude, y = latitude) %>%
  select("x", "y") %>%
  sp::SpatialPoints(proj4string = climate_data@crs)

climate_data <- climate_data[[c(1,4,12,15)]] # variables: https://www.worldclim.org/bioclim
names(climate_data) <- c("mean_annual_temp", "sd_temp", "mean_annual_precip", "sv_precip")

raw_values <- raster::extract(climate_data, points)
climate_df <- cbind.data.frame(sp::coordinates(points), raw_values) %>%
  rename(longitude = x, latitude = y) %>%
  left_join(long_lat %>% select(ETS_lang_name, contains("ude"))) %>%
  select(ETS_lang_name, contains("_"))

# caculate distance
rownames(climate_df) <- climate_df$ETS_lang_name
eco_dist_matrix <- dist(scale(climate_df[,-1]))

eco_dists_df <- eco_dist_matrix %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column("lang1") %>%
  gather("lang2", "eco_dist", -1) %>%
  rename(lang1_ETS = lang1,
         lang2_ETS = lang2)

write_csv(eco_dists_df, OUTFILE) # codes are ETS codes
