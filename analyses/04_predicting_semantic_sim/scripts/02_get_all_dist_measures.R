# merge together all dist measures
library(tidyverse)
library(janitor)
library(here)

OUTFILE <- here("analyses/04_predicting_semantic_sim/data/language_all_vars_distance.csv")
DIST1 <- here("analyses/04_predicting_semantic_sim/data/lang_distance_metrics/cultural/trade/data/trade_distances.csv")
DIST2 <- here("analyses/04_predicting_semantic_sim/data/lang_distance_metrics/ecological/data/eco_language_distance.csv")
DIST3 <- here("analyses/04_predicting_semantic_sim/data/lang_distance_metrics/linguistic/data/asjp_language_distance")
DIST4 <- here("analyses/04_predicting_semantic_sim/data/lang_distance_metrics/linguistic/data/wals_language_distance")
DIST5 <- here("analyses/04_predicting_semantic_sim/data/lang_distance_metrics/physical/data/physical_language_distance.csv")
DIST6 <- here("analyses/04_predicting_semantic_sim/data/lang_distance_metrics/cultural/dplace/data/dplace_distances.csv")
SEMANTIC_DIST <-  here("analyses/04_predicting_semantic_sim/data/language_semantics_distance.csv") # from 01a_lang_distances_raw.R

all_dists <- read_csv(DIST1) %>%
  full_join(read_csv(DIST2)) %>%
  full_join(read_csv(DIST3)) %>%
  full_join(read_csv(DIST4)) %>%
  full_join(read_csv(DIST5)) %>%
  full_join(read_csv(DIST6)) %>%
  left_join(read_csv(SEMANTIC_DIST)) %>%
  select(-lang1, -lang2)   %>%
  select(contains("ETS"),  "semantic_dist", contains("dist"), everything()) %>%
  clean_names()

write_csv(all_dists, OUTFILE)
