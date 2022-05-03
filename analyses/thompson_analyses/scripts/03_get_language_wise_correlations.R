# get pairwise correlations between words across languages for each decile

library(tidyverse)
library(feather)
library(data.table)
library(parallel)
library(here)

####################### PARAMETERS ######################
LANGS <- c('ARA', 'BEN', 'BUL', 'CHI', 'DUT', 'ENG', 'FAS', 'FRE', 'GER', 'GRE', 'GUJ',
           'HIN', 'IBO', 'IND', 'ITA', 'JPN','KAN', 'KOR', 'MAL', 'MAR', 'NEP', 'PAN', 'POL',
           'POR', 'RUM', 'RUS', 'SPA', 'TAM', 'TEL', 'TGL', 'THA', 'TUR', 'URD', 'VIE', 'YOR')
PAIRWISE_DIST_PREFIX <- here("analyses/thompson_analyses/data/pairwise_distances/")
OUTPATH <- here("analyses/thompson_analyses/data/lang_pairwise_word_correlations.csv")
NCLUSTERS <- 4

####################### LANGUAGE PAIRWISE DISTANCE CORRS######################


## For each language pair, get correlations between pairwise word distances
get_pairwise_lang_word_pairwise_correlations <- function(i, pairwise_dist_prefix, outpath, pairs){

  lang1 = pairs[i, 1]
  lang2 = pairs[i, 2]

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
    filter(w1 != w2)

  corr_values <-  merged_dists %>%
    group_by(w1) %>%
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
#
# ############# DO THE THING ##########
# # INITIATE CLUSTER
# cluster <- makeCluster(NCLUSTERS, type = "FORK")
#
# parallel_wrapper <- function(i){
#   lang1 = all_pairs[i, 1]
#   lang2 = all_pairs[i, 2]
#   m = get_pairwise_lang_word_pairwise_correlations(lang1,
#                                                    lang2,
#                                                    PAIRWISE_DIST_PREFIX,
#                                                    OUTPATH)
# }
#
# # DO THE THING (IN PARALLEL)
# parLapply(cluster,  1:dim(all_pairs)[1],  parallel_wrapper)

walk(1:dim(all_pairs)[1], get_pairwise_lang_word_pairwise_correlations, PAIRWISE_DIST_PREFIX, OUTPATH, all_pairs)

