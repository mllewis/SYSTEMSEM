# get pairwise correlations between words across languages for each language pair

library(tidyverse)
library(feather)
library(data.table)
library(parallel)
library(here)
library(dtplyr)

####################### PARAMETERS ######################
LANGS <- c( "el", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko", "ar", "bn", "bg", "zh", "nl",  "fr", "de",
            "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo","fa", "en")
PAIRWISE_DIST_PREFIX <- "/Volumes/walrus/concreteness_distances_for_paper/wiki/pairwise_word_10x1000_"
CLUSTER_ASSIGNMENTS <- here("analyses/02_concreteness_semantics/data/wiki/target_word_cluster_assignments_all.csv")
OUTPATH <-  here("analyses/02_concreteness_semantics/data/wiki/mean_cluster_corrs/pairwise_semantics_correlations_wiki_by_nclusters_")

write_corrs <- function(current_n_clusters,
                        merged_df,
                        clusters_assignments,
                        this_lang1,
                        this_lang2,
                        this_outfile) {

  print(current_n_clusters)

  sub_cluster_assignments <- clusters_assignments %>%
    filter(n_clusters == current_n_clusters)

  corr_values <- merged_df %>%
    lazy_dt() %>%
    filter(word1 != word2) %>%
    left_join(sub_cluster_assignments %>% select(word1, cluster1, n_clusters), by = "word1") %>% # merge in cluster assignments
    left_join(sub_cluster_assignments %>% select(word2, cluster2), by = "word2") %>%
    group_by(n_clusters, cluster1, cluster2) %>% # get cluster pair correlations
    summarize(cor = cor(cos_dist.x, cos_dist.y),
              n = n())

  rm(merged_df)

  mean_corrs <- corr_values %>%
    mutate(same = case_when(cluster1 == cluster2 ~ "local", TRUE ~ "global")) %>%
    group_by(n_clusters, cluster1, same) %>% # get cluster pair correlations
    summarize(mean_cor = mean(cor, na.rm = TRUE),
              n_sum = sum(n)) %>%
    group_by(n_clusters,  same) %>%
    summarize(cor = mean(mean_cor, na.rm = TRUE))  %>%
    mutate(lang1 = this_lang1,
           lang2 = this_lang2) %>%
    data.frame()

  rm(corr_values)

  full_outfile <- paste0(this_outfile, this_lang1, "_", this_lang2, ".csv")
  write_csv(mean_corrs, full_outfile, append = T)

}

get_pairwise_lang_cluster_pairwise_correlations <- function(lang1,
                                                            lang2,
                                                            all_cluster_assignments,
                                                            pairwise_dist_prefix,
                                                            outfile) {
  print(lang1)
  print(lang2)

  pairwise_dists1 <-  read_feather(paste0(pairwise_dist_prefix,
                                          lang1 , "_wiki.feather")) %>%
    select(-lang) %>%
    data.table()

  pairwise_dists2 <- read_feather(paste0(pairwise_dist_prefix,
                                         lang2 , "_wiki.feather")) %>%
    select(-lang) %>%
    data.table()

  merged_pairwise_distances <- merge(pairwise_dists1, pairwise_dists2,
                                     by = c("word1", "word2"))

  rm(pairwise_dists1)
  rm(pairwise_dists2)

  walk(unique(all_cluster_assignments$n_clusters),
       write_corrs,
       merged_pairwise_distances,
       all_cluster_assignments,
       lang1,
       lang2,
       outfile)

}

############ OTHER DATA ##########
cluster_assignments <- read_csv(CLUSTER_ASSIGNMENTS,
                                col_names = c("word", "cluster", "n_clusters")) %>%
  mutate(word = tolower(word), # deals with TRUE/FALSE excel issue
         cluster1 = cluster,
         cluster2 = cluster,
         word1 = word,
         word2 = word) %>%
  select(-word, -cluster) %>%
  arrange(n_clusters) %>%
  data.table()

# cross lang pairs with number of clusters
all_combos <- list(lang1 = LANGS,
                   lang2 = LANGS) %>%
  cross_df() %>%
  filter(lang1 < lang2)

############# DO THE THING ##########

walk2(all_combos$lang1,
      all_combos$lang2,
      get_pairwise_lang_cluster_pairwise_correlations,
      cluster_assignments,
      PAIRWISE_DIST_PREFIX,
      OUTPATH)
