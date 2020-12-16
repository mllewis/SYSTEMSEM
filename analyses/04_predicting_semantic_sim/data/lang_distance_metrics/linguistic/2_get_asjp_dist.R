# Get ling distances for ASJP based on https://github.com/ddediu/lgfam-newick\

# load packages
library(tidyverse)
library(feather)

LANG_INFILE <- "data/iso_to_wals_for_ling_dists.csv"
INFILE <- "data/asjp16-dists.RData"
OUTFILE <-  "data/asjp_language_distance"

# read in correct codes
lang_names <- read_csv(LANG_INFILE)

# read in linguistic distances
load(INFILE, verbose = T) 
asjp <-  asjp16.dm
remove(asjp16.dm)

# check codes
# setdiff(lang_names$asjp_code, rownames(asjp))

asjp_dist_matrix <- asjp[match(lang_names$asjp_code, rownames(asjp)), 
                         match(lang_names$asjp_code, rownames(asjp))]

asjp_dists_df <- asjp_dist_matrix %>%
  as.data.frame() %>%
  rownames_to_column("lang1") %>%
  gather("lang2", "asjp_lang_dist", -1) %>%
  left_join(lang_names %>% select(ETS_lang_name, asjp_code), by = c("lang1" = "asjp_code")) %>%
  select(-lang1) %>%
  rename(lang1_ETS = ETS_lang_name) %>%
  left_join(lang_names %>% select(ETS_lang_name, asjp_code), by = c("lang2" = "asjp_code")) %>%
  select(-lang2) %>%
  rename(lang2_ETS = ETS_lang_name) %>%
  select(lang1_ETS, lang2_ETS, asjp_lang_dist)

write_csv(asjp_dists_df, OUTFILE) # codes are ETS codes
