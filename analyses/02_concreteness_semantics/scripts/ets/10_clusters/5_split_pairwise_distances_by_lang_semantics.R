# Given the sample of concreteness words,
# get the pairwise distances for words based on semantic
# kmeans clusters of words (N = 10)

library(tidyverse)
library(feather)
library(data.table)
library(parallel)

############ PARAMETERS ############
SEMANTICS_PATH <-  here("analyses/02_concreteness_semantics/data/ets/target_word_cluster_assignments_ets.csv")
PAIRWISE_DIST_PREFIX <- "/Volumes/wilbur_the_great/concreteness_distances_for_paper/ets/"
NCLUSTERS <- 10
LANGS <- c('ARA', 'BEN', 'BUL', 'CHI', 'DUT', 'ENG', 'FAS', 'FRE', 'GER', 'GRE', 'GUJ',
           'HIN', 'IBO', 'IND', 'ITA', 'JPN','KAN', 'KOR', 'MAL', 'MAR', 'NEP', 'PAN', 'POL',
           'POR', 'RUM', 'RUS', 'SPA', 'TAM', 'TEL', 'TGL', 'THA', 'TUR', 'URD', 'VIE', 'YOR')

############# FUNCTIONS ##########

write_distance_chunk <- function(this_cluster1,
                                 this_cluster2,
                                 pairwise_dists,
                                 current_lang,
                                 pairwise_dist_prefix){

  output_path <- paste0(pairwise_dist_prefix, "semantics_split/",
                        current_lang, "_cluster_", this_cluster1,
                        "_", this_cluster2, "_pairwise_distances_ets.feather")

  pairwise_dists[cluster1 == this_cluster1 & cluster2 == this_cluster2] %>%
  write_feather(output_path)

}

# wrapper function for reading and writing
break_up_pairwise_distances <- function(current_lang,
                                        cluster_assignments,
                                        clusters_to_loop,
                                        pairwise_dist_prefix){

  print(paste0("====== ", current_lang,  " ======"))

  pairwise_dists <-  read_feather(paste0(pairwise_dist_prefix,
                                         current_lang , "_common_word_dists.feather")) %>%
    data.table(key = c("w1", "w2")) %>%
    merge(cluster_assignments[,w1, cluster1], by = "w1") %>%
    merge(cluster_assignments[,w2, cluster2], by = "w2")

  walk2(clusters_to_loop$cluster1,
        clusters_to_loop$cluster2,
        write_distance_chunk,
        pairwise_dists,
        current_lang,
        pairwise_dist_prefix)

}

############# OTHER DATA ##########
cluster_assignments  <- read_csv(SEMANTICS_PATH) %>%
  mutate(cluster1 = cluster,
         cluster2 = cluster,
         w1 = word,
         w2 = word) %>%
  select(-word, -cluster) %>%
  data.table()

clusters_to_loop <- crossing(cluster1 = 1:NCLUSTERS,
                             cluster2 = 1:NCLUSTERS) %>%
  arrange(cluster1)  %>%
  filter(cluster2 <= cluster1)


############# DO THE THING ##########

# INITIATE CLUSTER - crashes
#cluster <- makeCluster(NCLUSTERS, type = "FORK")

# WRAPPER FUNCTION
parallel_wrapper <- function(lang){
  break_up_pairwise_distances(lang, cluster_assignments,
                              clusters_to_loop,
                              PAIRWISE_DIST_PREFIX)
}

# DO THE THING (IN PARALLEL)
#parLapply(cluster, LANGS, parallel_wrapper)
walk(LANGS, parallel_wrapper)


