# get pairwise correlations between words across languages for each cluster

library(tidyverse)
library(feather)
library(data.table)
library(parallel)
library(broom)

####################### PARAMETERS ######################
LANGS <- c("ar", "bn", "bg", "zh", "nl",  "fr", "de", "el", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko",
           "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo", "fa", "en")
PAIRWISE_DIST_PREFIX <-"/Volumes/wilbur_the_great/concreteness_distances_for_paper/wiki/semantics_split/"
OUTPATH <- here("analyses/02_concreteness_semantics/data/wiki/lang_pairwise_semantics_correlations_wiki_by_cluster.csv")
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
                                       y, "_pairwise_distances.feather"))}) %>%
    data.table(key = c("word1", "word2"))

  pairwise_dists2 <-  map2_df(out_clusters$c1,
                              out_clusters$c2, function(x,y){read_feather(
                                paste0(pairwise_dist_prefix, lang2 ,"_cluster_", x, "_",
                                       y, "_pairwise_distances.feather"))}) %>%
    data.table(key = c("word1", "word2"))

  # inner join two sets of distances
  merged_dists <- merge(pairwise_dists1,
                        pairwise_dists2, by = c("word1", "word2"))  %>%
    filter(word1 != word2)

  corr_values <-  merged_dists %>%
    group_by(cluster1.x, cluster2.x) %>%
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

all_pairs <- data.frame(lang1 = c("id"),
                         lang2 = c("en"))

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

