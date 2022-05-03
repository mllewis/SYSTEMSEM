# get pairwise correlations between words across languages for each decile

library(tidyverse)
library(feather)
library(data.table)
library(parallel)
library(broom)
library(here)

####################### PARAMETERS ######################
LANGS <- c("ar", "bn", "bg", "zh", "nl",  "fr", "de", "el", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko",
           "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo", "fa", "en")
PAIRWISE_DIST_PREFIX <- "/Volumes/wilbur_the_great/concreteness_distances_for_paper/wiki/concreteness_split/"
OUTPATH <- here("analyses/02_concreteness_semantics/data/wiki/lang_pairwise_tile_correlations.csv")
NTILES <- 10
NCLUSTERS <- 5

####################### LANGUAGE PAIRWISE DISTANCE CORRS######################
calculate_correlation <- function(tile1, tile2, lang1, lang2, pairwise_dist_prefix){

  # read in two sets of pairiwise dists
  pairwise_dists1 <-  data.table(read_feather(paste0(pairwise_dist_prefix, lang1 ,"_", tile1,
                                                     "_", tile2, "_pairwise_distances.feather")),
                                 key = c("word1", "word2"))
  pairwise_dists2 <-  data.table(read_feather(paste0(pairwise_dist_prefix, lang2 ,"_", tile1,
                                                     "_", tile2, "_pairwise_distances.feather")),
                                 key = c("word1", "word2"))

  # inner join two sets of distances
  merged_dists <- merge(pairwise_dists1,
                        pairwise_dists2, by = c("word1", "word2"))  %>%
    filter(word1 != word2) %>%
    data.table()

  data.frame(concreteness_tile1 = tile1,
             concreteness_tile2 = tile2,
             pearsons_r = merged_dists[,cor(cos_dist.x, cos_dist.y)])
}

## For each language pair, get correlations between pairwise word distances
get_pairwise_lang_word_pairwise_correlations <- function(lang1, lang2, tiles_to_loop,
                                                         pairwise_dist_prefix, outpath){
  print(paste0("====== ", lang1, " ",lang2, " ======"))

  lang_pair_corrs <- map2_df(tiles_to_loop$concreteness_tile1,
          tiles_to_loop$concreteness_tile2,
          calculate_correlation,
          lang1,
          lang2,
          pairwise_dist_prefix) %>%
    mutate(lang1 = lang1,
           lang2 = lang2)

  write_csv(lang_pair_corrs, outpath, append = T)

}

############# OTHER DATA ##########
tiles_to_loop <- crossing(concreteness_tile1 = 1:NTILES,
                          concreteness_tile2 = 1:NTILES) %>%
  arrange(concreteness_tile1)  %>%
  filter(concreteness_tile2 <= concreteness_tile1) %>%
  as.data.frame()

all_pairs <- data.frame(t(combn(LANGS, 2))) %>%
              rename(lang1 = X1,
                    lang2 = X2) %>%
  mutate_all(as.character)



complete_pairs <- read_csv(OUTPATH, col_names = F) %>%
  distinct(X4, X5) %>%
  rename(lang1 = X4,
         lang2 = X5)

lang_pairs = anti_join(all_pairs, complete_pairs)

############# DO THE THING ##########
# INITIATE CLUSTER
cluster <- makeCluster(NCLUSTERS, type = "FORK")

parallel_wrapper <- function(i){
  lang1 = lang_pairs[i, 1]
  lang2 = lang_pairs[i, 2]
  m = get_pairwise_lang_word_pairwise_correlations(lang1,
                                                    lang2,
                                                    tiles_to_loop,
                                                    PAIRWISE_DIST_PREFIX,
                                                    OUTPATH)
}

# DO THE THING (IN PARALLEL)
parLapply(cluster, 1:dim(lang_pairs)[1], parallel_wrapper)
