# Given the sample of concreteness words,
# get the pairwise distances for words based on semantic
# kmeans clusters of words (N = 10)

library(tidyverse)
library(feather)
library(data.table)
library(parallel)

############ PARAMETERS ############
SEMANTICS_PATH <-  here("analyses/02_concreteness_semantics/data/wiki/target_translations_xling_words.csv")
PAIRWISE_DIST_PREFIX <- "/Volumes/wilbur_the_great/concreteness_distances_for_paper/"
NCLUSTERS <- 10
LANGS <- c("ar", "bn", "bg", "zh", "nl",  "fr", "de", "el", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko",
           "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo", "fa", "en")

############# FUNCTIONS ##########

write_distance_chunk <- function(this_cluster1,
                                 this_cluster2,
                                 pairwise_dists,
                                 current_lang,
                                 pairwise_dist_prefix){

  output_path <- paste0(pairwise_dist_prefix, "semantics_split/",
                        current_lang, "_cluster_", this_cluster1,
                        "_", this_cluster2, "_pairwise_distances.feather")

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
                                         "pairwise_word_10x1000_",
                                         current_lang , "_wiki.feather")) %>%
    data.table(key = c("word1", "word2")) %>%
    merge(cluster_assignments[,word1, cluster1], by = "word1") %>%
    merge(cluster_assignments[,word2, cluster2], by = "word2")

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
         word1 = word,
         word2 = word) %>%
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


