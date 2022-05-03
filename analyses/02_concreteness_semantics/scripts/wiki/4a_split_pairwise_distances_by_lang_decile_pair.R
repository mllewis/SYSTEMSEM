# Given a sample of words from each tile of concretess, get the pairwise distances for those words
# for all wiki model.

library(tidyverse)
library(feather)
library(data.table)
library(parallel)

####################### PARAMETERS ######################
TARGET_WORD_PATH <- "data/target_translations_xling_words.csv"
PAIRWISE_DIST_PREFIX <- "/Volumes/wilbur_the_great/concreteness_distances_for_paper/"
NTILES <- 10
#LANGS <- c( "el", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko", "ar", "bn", "bg", "zh", "nl",  "fr", "de",
#           "ml", "mr", "ne", "pa", "pl",
LANGS <- c( "pl")


############# FUNCTIONS ##########

write_distance_chunk <- function(tile1,
                                 tile2,
                                 pairwise_dists,
                                 current_lang, pairwise_dist_prefix){

  output_path <- paste0(pairwise_dist_prefix, "concreteness_split/", current_lang,
                        "_", tile1, "_", tile2, "_pairwise_distances.feather")

  pairwise_dists[concreteness_tile1 == tile1 & concreteness_tile2 == tile2] %>%
    write_feather(output_path)

}

# wrapper function for reading and writing
break_up_pairwise_distances <- function(current_lang,
                                      target_words,
                                      tiles_to_loop,
                                      pairwise_dist_prefix){

  print(paste0("====== ", current_lang,  " ======"))

  pairwise_dists <-  read_feather(paste0(pairwise_dist_prefix, "pairwise_word_10x1000_", current_lang , "_wiki.feather")) %>%
    data.table(key = c("word1", "word2")) %>%
    merge(target_words[,word1, concreteness_tile1], by = "word1") %>%
    merge(target_words[,word2, concreteness_tile2], by = "word2")

  walk2(tiles_to_loop$concreteness_tile1, tiles_to_loop$concreteness_tile2,
        write_distance_chunk, pairwise_dists,
        current_lang,
        pairwise_dist_prefix)

}

############# OTHER DATA ##########
target_words <- read_csv(TARGET_WORD_PATH) %>%
  distinct(word,concreteness_tile) %>%
  mutate(word1 = word,
         word2 = word,
         concreteness_tile1 = concreteness_tile,
         concreteness_tile2 = concreteness_tile) %>%
  select(-concreteness_tile, -word) %>%
  data.table()

tiles_to_loop <- crossing(concreteness_tile1 = 1:NTILES,
                          concreteness_tile2 = 1:NTILES) %>%
  arrange(concreteness_tile1)  %>%
  filter(concreteness_tile2 <= concreteness_tile1) %>%
  as.data.frame()

############# DO THE THING ##########

# INITIATE CLUSTER - crashes
#cluster <- makeCluster(NCLUSTERS, type = "FORK")

# WRAPPER FUNCTION
parallel_wrapper <- function(lang){
  break_up_pairwise_distances(lang, target_words, tiles_to_loop, PAIRWISE_DIST_PREFIX)
}

# DO THE THING (IN PARALLEL)
#parLapply(cluster, LANGS, parallel_wrapper)
walk(LANGS, parallel_wrapper)

break_up_pairwise_distances("pl", target_words, tiles_to_loop, PAIRWISE_DIST_PREFIX)



