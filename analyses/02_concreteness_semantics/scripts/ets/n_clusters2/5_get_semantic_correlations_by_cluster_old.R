# get pairwise correlations between words across languages for each language pair, for each n_clusters

library(tidyverse)
library(feather)
library(data.table)
library(parallel)
library(here)

####################### PARAMETERS ######################
LANGS <- c('ARA', 'BEN', 'BUL', 'CHI', 'DUT', 'ENG', 'FAS', 'FRE', 'GER', 'GRE', 'GUJ',
           'HIN', 'IBO', 'IND', 'ITA', 'JPN','KAN', 'KOR', 'MAL', 'MAR', 'NEP', 'PAN', 'POL',
           'POR', 'RUM', 'RUS', 'SPA', 'TAM', 'TEL', 'TGL', 'THA', 'TUR', 'URD', 'VIE', 'YOR')
OUTPATH <- here("analyses/02_concreteness_semantics/data/ets/lang_pairwise_semantics_correlations_ets_by_nclusters.csv")
CLUSTER_ASSIGNMENTS <- here("analyses/02_concreteness_semantics/data/ets/target_word_cluster_assignments_ets_all.csv")
PAIRWISE_DIST_PREFIX <- "/Volumes/wilbur_the_great/ets_lang_pairwise_distances/pairwise_distances/"
N_COMP_CLUSTERS <- 6

get_pairwise_lang_cluster_pairwise_correlations <- function(lang1,
                                                            lang2,
                                                            nclusts_total,
                                                            all_cluster_assignments,
                                                            pairwise_dist_prefix,
                                                            outfile) {

  pairwise_dists1 <-  read_feather(paste0(pairwise_dist_prefix,
                                         lang1 , "_common_word_dists.feather")) %>%
    data.table()

  pairwise_dists2 <- read_feather(paste0(pairwise_dist_prefix,
                                           lang2 , "_common_word_dists.feather")) %>%
    data.table()

  merged_pairwise_distances <- merge(pairwise_dists1, pairwise_dists2, by = c("w1", "w2"))

  corr_values <- merged_pairwise_distances %>%
    filter(w1 != w2) %>%
    left_join(all_cluster_assignments %>% select(w1, cluster1), by = "w1") %>% # merge in cluster assignments
    left_join(all_cluster_assignments %>% select(w2, cluster2), by = "w2") %>%
    group_by(cluster1, cluster2) %>% # get cluster pair correlations
    summarize(cor = cor(cos_dist.x, cos_dist.y)) %>%
    mutate(lang1 = lang1,
           lang2 = lang2,
           n_clust = nclusts_total)

  write_csv(corr_values, outfile, append = T)

}

############ OTHER DATA ##########
cluster_assignments <- read_csv(CLUSTER_ASSIGNMENTS) %>%
  mutate(word = tolower(word),# deals with TRUE/FALSE excel issue
         cluster1 = cluster,
         cluster2 = cluster,
         w1 = word,
         w2 = word) %>%
  select(-word, -cluster) %>%
  filter(n_clusters < 500) %>%
  arrange(n_clusters) 

# cross lang pairs with number of clusters
all_combos <- list(lang1 = LANGS,
                   lang2 = LANGS,
                   n_clusts = unique(cluster_assignments$n_clusters)) %>%
  cross_df() %>%
  filter(lang1 < lang2)

############# DO THE THING ##########
# INITIATE CLUSTER
cluster <- makeCluster(N_COMP_CLUSTERS, type = "FORK")

parallel_wrapper <- function(i, combos, these_clusters_assignments, prefix_path, outpath){
  lang1 <- combos %>% slice(i) %>% pull(lang1)
  lang2 <- combos %>% slice(i) %>% pull(lang2)
  total_n_clusters <-  combos %>% slice(i) %>% pull(n_clusts)

  sub_cluster_assignments <- these_clusters_assignments %>% # get cluster assignments for relevant clusters
    filter(n_clusters == total_n_clusters) %>%
    data.table()

 temp <- get_pairwise_lang_cluster_pairwise_correlations(lang1,
                                                      lang2,
                                                      total_n_clusters,
                                                      sub_cluster_assignments,
                                                      prefix_path,
                                                      outpath)
}

# DO THE THING (IN PARALLEL)
parLapply(cluster,
            1:nrow(all_combos),
            parallel_wrapper,
            all_combos,
            cluster_assignments,
            PAIRWISE_DIST_PREFIX,
            OUTPATH)


