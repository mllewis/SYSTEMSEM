# evaluate similarity between original (english-based) clusters and native language model clusters for ets words

library(tidyverse)
library(data.table)
library(here)
library(sabre)

############# PARAMETERS #############
TARGET_LANGS <- c('ARA', 'BEN', 'BUL', 'CHI', 'DUT', 'ENG', 'FAS', 'FRE', 'GER', 'GRE', 'GUJ',
                  'HIN', 'IBO', 'IND', 'ITA', 'JPN','KAN', 'KOR', 'MAL', 'MAR', 'NEP', 'PAN', 'POL',
                  'POR', 'RUM', 'RUS', 'SPA', 'TAM', 'TEL', 'TGL', 'THA', 'TUR', 'URD', 'VIE', 'YOR')
ORIGINAL_CLUSTERS <- here("analyses/02_concreteness_semantics/data/ets/target_word_cluster_assignments_ets_all.csv")
NATIVE_CLUSTER_PATH <- here("analyses/11_native_cluster_control/data/native_lang_clusters/")
OUTPATH <-  here("analyses/11_native_cluster_control/data/ets_cluster_vmeasure.csv")

original_clusters_df <- read_csv(ORIGINAL_CLUSTERS) %>%
  rename(original_cluster = cluster) %>%
  filter(n_clusters <= 250) %>%
  mutate(word = tolower(word)) # deal with TRUE/FALSE words

get_cluster_eval <- function(lang, cluster_in_path, original_clusters){

  full_inpath <- paste0(cluster_in_path, "clusters_ets_", lang, ".csv")
  native_lang_clusters <- read_csv(full_inpath,
                                   col_names = c("word", "native_cluster", "n_clusters", "lang"))

  all_clusters <- original_clusters %>%
    left_join(native_lang_clusters) %>%
    filter(!is.na(original_cluster), !is.na(native_cluster))

  all_clusters %>%
    group_by(lang, n_clusters) %>%
    nest() %>%
    mutate(vmeasure = map_dbl(data, ~vmeasure(.$original_cluster, .$native_cluster)[[1]][1]), # this measure is symmetric
           n_words = map_dbl(data, nrow)) %>%
    select(-data)
}


all_langs_vmeasure <- map_df(TARGET_LANGS,
                             get_cluster_eval,
                             NATIVE_CLUSTER_PATH,
                             original_clusters_df)

write_csv(all_langs_vmeasure, OUTPATH)
