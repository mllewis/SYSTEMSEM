# Get ling distances for WALS based on https://github.com/ddediu/lgfam-newick\

# load packages
library(tidyverse)
library(feather)

LANG_INFILE <- "data/iso_to_wals_for_ling_dists.csv"
INFILE <- "data/wals-euclidean-mode-dm.RData" # euclidean distance with missing data imputation
OUTFILE <-  "data/wals_language_distance"

# read in correct codes
lang_names <- read_csv(LANG_INFILE)

# read in linguistic distances
load(INFILE, verbose = T)
wals <- as.matrix(wals.euclidean.mode.dm)
remove(wals.euclidean.mode.dm)


# Get pairiwse distances - WALS
wals_dist_matrix <- wals[match(lang_names$wals_code, rownames(wals)), 
                         match(lang_names$wals_code, rownames(wals))]

wals_dists_df = wals_dist_matrix %>%
  as.data.frame() %>%
  rownames_to_column("lang1") %>%
  gather("lang2", "wals_lang_dist", -1) %>%
  left_join(lang_names %>% select(ETS_lang_name, wals_code), by = c("lang1" = "wals_code")) %>%
  select(-lang1) %>%
  rename(lang1_ETS = ETS_lang_name) %>%
  left_join(lang_names %>% select(ETS_lang_name, wals_code), by = c("lang2" = "wals_code")) %>%
  select(-lang2) %>%
  rename(lang2_ETS = ETS_lang_name) %>%
  select(lang1_ETS, lang2_ETS, wals_lang_dist)

write_csv(wals_dists_df, OUTFILE) # codes are ETS codes
