# get pairwise correlations between words across languages for each decile

library(tidyverse)
library(feather)
library(data.table)
library(parallel)
library(here)

####################### PARAMETERS ######################

MODELS <- map(0:34, ~paste0("random-", .)) %>%
  unlist() %>%
  rev()
PAIRWISE_DIST_PREFIX <-  here("analyses/06_random_control/data/pairwise_word_dists/")
CONC_PATH <- here("analyses/06_random_control/data/concreteness_deciles_random.csv")
OUTPATH <-  here("analyses/06_random_control/data/lang_pairwise_tile_correlations_random_ets_decile.csv")
NCLUSTERS <- 3

####################### LANGUAGE PAIRWISE DISTANCE CORRS######################

## For each language pair, get correlations between pairwise word distances
get_pairwise_lang_word_pairwise_correlations <- function(lang1, lang2,
                                                         pairwise_dist_prefix, outpath,
                                                         deciles_df){
  print(paste0("====== ", lang1, " ",lang2, " ======"))

  # read in two sets of pairiwise dists
    pairwise_dists1 <-  read_feather(paste0(pairwise_dist_prefix, lang1,
                                                     "_common_word_dists.feather")) %>%
                          data.table(key = c("w1", "w2"))

    pairwise_dists2 <-  read_feather(paste0(pairwise_dist_prefix, lang2,
                                           "_common_word_dists.feather")) %>%
                          data.table(key = c("w1", "w2"))

    # inner join two sets of distances
    merged_dists <- merge(pairwise_dists1,
                          pairwise_dists2, by = c("w1", "w2"))  %>%
      left_join(deciles_df %>% select(w1, conc_tile1), by = "w1") %>%
      left_join(deciles_df %>% select(w2, conc_tile2), by = "w2") %>%
      filter(w1 != w2)

    corr_values <-  merged_dists %>%
      group_by(conc_tile1, conc_tile2) %>%
      nest() %>%
      mutate(cor_r = map_dbl(data, ~cor(.$cos_dist.x, .$cos_dist.y))) %>%
      select(-data) %>%
      mutate(lang1 = lang1,
             lang2 = lang2)

  write_csv(corr_values, outpath, append = T)

}

############# OTHER DATA ##########
all_pairs <- data.frame(t(combn(MODELS, 2))) %>%
              rename(lang1 = X1,
                    lang2 = X2) %>%
  mutate_all(as.character)

deciles <- read_csv(CONC_PATH) %>%
  mutate(conc_tile1 = conc_tile,
         conc_tile2 = conc_tile,
         w1 = word,
         w2 = word) %>%
  select(-n, -word, -conc_tile)

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
                                                   deciles)
}

# DO THE THING (IN PARALLEL)
parLapply(cluster, 1:dim(all_pairs)[1], parallel_wrapper)
