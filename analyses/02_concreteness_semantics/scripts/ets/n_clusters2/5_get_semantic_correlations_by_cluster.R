# get pairwise correlations between words across languages for each language pair

library(tidyverse)
library(feather)
library(data.table)
library(parallel)
library(here)

####################### PARAMETERS ######################
LANGS <- c('ARA', 'BEN', 'BUL', 'CHI', 'DUT', 'ENG', 'FAS', 'FRE', 'GER', 'GRE', 'GUJ',
           'HIN', 'IBO', 'IND', 'ITA', 'JPN','KAN', 'KOR', 'MAL', 'MAR', 'NEP', 'PAN', 'POL',
           'POR', 'RUM', 'RUS', 'SPA', 'TAM', 'TEL', 'TGL', 'THA', 'TUR', 'URD', 'VIE', 'YOR')
PAIRWISE_DIST_PREFIX <- here("analyses/02_concreteness_semantics/data/ets/pairwise_distances/")
OUTPATH <- here("analyses/02_concreteness_semantics/data/ets/lang_pairwise_semantics_correlations_ets_by_nclusters.csv")
CLUSTER_ASSIGNMENTS <- here("analyses/02_concreteness_semantics/data/ets/target_word_cluster_assignments_ets_all.csv")
OUTFILE <-  here("analyses/02_concreteness_semantics/data/ets/mean_cluster_corrs/lang_pairwise_semantics_correlations_ets_by_nclusters")
N_COMP_CLUSTERS <- 4

get_pairwise_lang_cluster_pairwise_correlations <- function(lang1,
                                                            lang2,
                                                            nclusts_total,
                                                            all_cluster_assignments,
                                                            pairwise_dist_prefix,
                                                            outfile) {

  # pairwise cosine distances between words in a language (symmetrical)
  pairwise_dists1 <-  read_feather(paste0(pairwise_dist_prefix,
                                         lang1 , "_common_word_dists.feather")) %>%
    data.table()

  pairwise_dists2 <- read_feather(paste0(pairwise_dist_prefix,
                                           lang2 , "_common_word_dists.feather")) %>%
    data.table()

  # merge together two language distances (inner join)
  merged_pairwise_distances <- merge(pairwise_dists1, pairwise_dists2,
                                     by = c("w1", "w2"))

  corr_values <- merged_pairwise_distances %>%
    lazy_dt() %>%
    filter(w1 != w2) %>% # remove distances between same word
    left_join(all_cluster_assignments %>% select(w1, cluster1), by = "w1") %>% # merge in cluster assignments
    left_join(all_cluster_assignments %>% select(w2, cluster2), by = "w2") %>%
    group_by(cluster1, cluster2) %>% # get all cluster pair correlations (symmetrical)
    summarize(cor = cor(cos_dist.x, cos_dist.y), # note that each word pair is represented twice here - but this is fine since were not doing signficance testing
              n = n())


  mean_corrs <- corr_values %>%
    mutate(same = case_when(cluster1 == cluster2 ~ "local", TRUE ~ "global")) %>%
    group_by(cluster1, same) %>% # get cluster pair correlations
    summarize(mean_cor = mean(cor, na.rm = T),
              sd_corr = sd(cor, na.rm = T),
              n_sum = sum(n)) %>%
    mutate(lang1 = lang1,
           lang2 = lang2,
           n_clust = nclusts_total) %>%
    as.data.frame()

  this_outfile <- paste0(outfile, "_", nclusts_total, ".csv")
  write_csv(mean_corrs, this_outfile, append = T)

}

############ OTHER DATA ##########
cluster_assignments <- read_csv(CLUSTER_ASSIGNMENTS) %>%
  mutate(word = tolower(word), # deals with TRUE/FALSE excel issue
         cluster1 = cluster,
         cluster2 = cluster,
         w1 = word,
         w2 = word) %>%
  select(-word, -cluster) %>%
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
    filter(n_clusters == total_n_clusters)

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

