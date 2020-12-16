# get physical distance between each language 
# physical distances come from geosphere packages
# "Estimate of the shortest distance between two points on an ellipsoid (default is WGS84 ellipsoid). 
# The shortest path between two points on an ellipsoid is called the geodesic."

library(tidyverse)
library(geosphere)

LONGLAT <- "data/long_lat_by_language.csv"
OUTFILE <- "data/physical_language_distance.csv"

lang_data <- read_csv(LONGLAT)

pairwise_physical_distances <- distm(as.matrix(lang_data[,c("longitude", "latitude")]))

colnames(pairwise_physical_distances) <- 
  lang_data$google_lang_name

pairwise_physical_dists <- pairwise_physical_distances %>%
  data.frame() %>%
  mutate(lang1 = colnames(pairwise_physical_distances)) %>%
  select(lang1, everything()) %>%
  gather("lang2", "dist", -lang1)

pairwise_physical_dists_full <- pairwise_physical_dists %>%
  left_join(lang_data %>% select(ETS_lang_name, google_lang_name), by = c("lang1" = "google_lang_name")) %>%
  rename(lang1_ETS = "ETS_lang_name") %>%
  left_join(lang_data %>% select(ETS_lang_name, google_lang_name), by = c("lang2" = "google_lang_name")) %>%
  rename(lang2_ETS = "ETS_lang_name") %>%
  rename(physical_dist = dist)

write_csv(pairwise_physical_dists_full, OUTFILE)