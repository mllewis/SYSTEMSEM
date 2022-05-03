# get pairwise correlations between words across languages for each cluster

library(tidyverse)
library(feather)
library(data.table)
library(parallel)
library(broom)
library(here)

####################### PARAMETERS ######################
LANGS <- c('ARA', 'BEN', 'BUL', 'CHI', 'DUT', 'ENG', 'FAS', 'FRE', 'GER', 'GRE', 'GUJ',
           'HIN', 'IBO', 'IND', 'ITA', 'JPN','KAN', 'KOR', 'MAL', 'MAR', 'NEP', 'PAN', 'POL',
           'POR', 'RUM', 'RUS', 'SPA', 'TAM', 'TEL', 'TGL', 'THA', 'TUR', 'URD', 'VIE', 'YOR')
PAIRWISE_DIST_PREFIX <- "/Volumes/wilbur_the_great/concreteness_distances_for_paper/ets/semantics_split_ward/"
SEMANTICS_PATH <-  here("analyses/09_clustering_control/data/ets/target_word_cluster_assignments_ets_ward.csv")
OUTPATH <- here("analyses/09_clustering_control/data/ets/lang_pairwise_semantics_correlations_ets_by_cluster_ward.csv")
NSEMANTIC_CLUSTERS <- 10
NCLUSTERS <- 5


## For each language pair, get correlations between pairwise word distances
get_pairwise_lang_word_pairwise_correlations <- function(lang1, lang2,
                                                         pairwise_dist_prefix, outpath,
                                                         clusters_df){
  print(paste0("====== ", lang1, " ",lang2, " ======"))

  out_clusters <- crossing(c1 = 1:10, c2 = 1:10) %>%
    filter(c2 <= c1)

  pairwise_dists1 <-  map2_df(out_clusters$c1,
                              out_clusters$c2, function(x,y){read_feather(
                                paste0(pairwise_dist_prefix, lang1 ,"_cluster_", x, "_",
                                       y, "_pairwise_distances_ets_ward.feather"))}) %>%
    data.table(key = c("w1", "w2"))

  pairwise_dists2 <-  map2_df(out_clusters$c1,
                              out_clusters$c2, function(x,y){read_feather(
                                paste0(pairwise_dist_prefix, lang2 ,"_cluster_", x, "_",
                                       y, "_pairwise_distances_ets_ward.feather"))}) %>%
    data.table(key = c("w1", "w2"))

  # inner join two sets of distances
  merged_dists <- merge(pairwise_dists1,
                        pairwise_dists2, by = c("w1", "w2"))  %>%
    left_join(clusters_df %>% select(w1, cluster1), by = "w1") %>%
    left_join(clusters_df %>% select(w2, cluster2), by = "w2") %>%
    filter(w1 != w2)

  corr_values <-  merged_dists %>%
    group_by(cluster1, cluster2) %>%
    nest() %>%
    mutate(cor_r = map_dbl(data, ~cor(.$cos_dist.x, .$cos_dist.y))) %>%
    select(-data) %>%
    mutate(lang1 = lang1,
           lang2 = lang2)

  write_csv(corr_values, outpath, append = T)

}

############# OTHER DATA ##########
all_pairs <- data.frame(t(combn(LANGS, 2))) %>%
  rename(lang1 = X1,
         lang2 = X2) %>%
  mutate_all(as.character)

words_to_clusters <- read_csv(SEMANTICS_PATH) %>%
  mutate(cluster1 = cluster,
         cluster2 = cluster,
         w1 = word,
         w2 = word) %>%
  select(-word, -cluster)

############# DO THE THING ##########
# INITIATE CLUSTER
cluster <- makeCluster(NCLUSTERS, type = "FORK")

parallel_wrapper <- function(i){
  lang1 = all_pairs[i, 1]
  lang2 = all_pairs[i, 2]
  m = get_pairwise_lang_word_pairwise_correlations(lang1,
                                                   lang2,
                                                   PAIRWISE_DIST_PREFIX,
                                                   OUTPATH,
                                                   words_to_clusters)
}

# DO THE THING (IN PARALLEL)
parLapply(cluster, 1:dim(all_pairs)[1], parallel_wrapper)

