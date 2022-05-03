# for each item, get correlation across language pairs of distances using QAP method
library(tidyverse)
library(sna)
library(psych)
library(here)

PAIRWISE_WORD_DIST_CORRS <- here("analyses/03_swadesh/data/language_pairwise_swadesh_correlations_by_item.csv")
PAIRWISE_LANG_DISTS <- here("analyses/04_predicting_semantic_sim/data/lang_distance_metrics/physical/data/physical_language_distance.csv")
QAP_MODEL_PARAMETERS_SWADESH <- here("analyses/03_swadesh/data/item_qap_p_values_physical.csv")

corr_df <- read_csv(PAIRWISE_WORD_DIST_CORRS)
pairwise_physical_dists <- read_csv(PAIRWISE_LANG_DISTS)  %>%
  filter(lang1 != lang2)
corr_geo <- full_join(corr_df, pairwise_physical_dists)

# qap regression
get_qap_p <- function(this_item, this_df) {
  print(this_item)

  dv_mat <- this_df %>%
    select(word, lang1_ETS, lang2_ETS, cor) %>%
    filter(word == this_item) %>%
    spread(lang1_ETS, cor) %>%
    select(-lang2_ETS, -word) %>%
    as.matrix()

  iv_mat <- this_df %>%
    filter(lang1_ETS %in% colnames(dv_mat),
           lang2_ETS %in% colnames(dv_mat)) %>%
    select(physical_dist, lang1_ETS, lang2_ETS) %>%
    distinct() %>%
    spread(lang1_ETS, physical_dist) %>%
    select(-lang2_ETS) %>%
    as.matrix()

  qap_model <- netlm(iv_mat,
                     dv_mat,
                     mode = "graph", # weights are undirected
                     nullhyp = "qap",
                     reps = 1000)

  data.frame(item = this_item,
             qap_p = summary(qap_model)$pgreqabs[2],
             estimate = summary(qap_model)$coefficients[2],
             tstat = summary(qap_model)$tstat[2],
             n =  summary(qap_model)$n)
}

p_vals <- map_df(unique(corr_geo$word),
                 get_qap_p, corr_geo)

write_csv(p_vals, QAP_MODEL_PARAMETERS_SWADESH)

