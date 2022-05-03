# get pairwise correlations between words across languages for each decile

library(tidyverse)
library(feather)
library(data.table)
library(parallel)


####################### PARAMETERS ######################
LANGS <- c('ARA', 'BEN', 'BUL', 'CHI', 'DUT', 'ENG', 'FAS', 'FRE', 'GER', 'GRE', 'GUJ',
           'HIN', 'IBO', 'IND', 'ITA', 'JPN','KAN', 'KOR', 'MAL', 'MAR', 'NEP', 'PAN', 'POL',
           'POR', 'RUM', 'RUS', 'SPA', 'TAM', 'TEL', 'TGL', 'THA', 'TUR', 'URD', 'VIE', 'YOR')

PAIRWISE_DIST_PREFIX <- "/Volumes/wilbur_the_great/syntax_control_analysis_for_paper/"
OUTPATH <-  here("analyses/05_syntax_control/data/lang_pairwise_correlations_by_syntactic_word_type.csv")
NCLUSTERS <- 6

####################### LANGUAGE PAIRWISE DISTANCE CORRS######################
## For each language pair, get correlations between pairwise word distances
get_pairwise_lang_word_pairwise_correlations <- function(arg_values,
                                                         pairwise_dist_prefix,
                                                         outpath){
  current_group <- arg_values[[1]][[1]]
  current_word_type <- arg_values[[1]][[2]]
  current_lang1 <- arg_values[[1]][[3]]
  current_lang2 <- arg_values[[1]][[4]]


  # read in two sets of pairiwise dists
    pairwise_dists1 <-  read_feather(paste0(pairwise_dist_prefix,
                                            current_lang1, "_",
                                            current_word_type, "_",
                                            current_group, "_", "word_dists.feather")) %>%
                          data.table(key = c("w1", "w2"))

    pairwise_dists2 <-  read_feather(paste0(pairwise_dist_prefix,
                                            current_lang2, "_",
                                            current_word_type, "_",
                                            current_group, "_", "word_dists.feather")) %>%
      data.table(key = c("w1", "w2"))

    # inner join two sets of distances
    merged_dists <- merge(pairwise_dists1,
                          pairwise_dists2, by = c("w1", "w2"))  %>%
      filter(w1 != w2)

    corr_values <-  merged_dists %>%
      summarize(cor_r = cor(.$cos_dist.x, .$cos_dist.y)) %>%
      mutate(lang1 = current_lang1,
             lang2 = current_lang2,
             score_group = current_group,
             word_type = current_word_type) %>%
      select(lang1, lang2, score_group, word_type, cor_r)

  write_csv(corr_values, outpath, append = T)

}

### DO THE THING ##
args <- cross(list(c("low", "high"),
           c("grammatical", "content"),
           LANGS,
           LANGS)) %>%
  keep(~.x[[3]] < .x[[4]]) # get unique language pairs


############# DO THE THING ##########
# INITIATE CLUSTER
cluster <- makeCluster(NCLUSTERS, type = "FORK")

parallel_wrapper <- function(i){

  m = get_pairwise_lang_word_pairwise_correlations(args[i],
                                                   PAIRWISE_DIST_PREFIX,
                                                   OUTPATH)
}

# DO THE THING (IN PARALLEL)
parLapply(cluster, 1:length(args), parallel_wrapper)
