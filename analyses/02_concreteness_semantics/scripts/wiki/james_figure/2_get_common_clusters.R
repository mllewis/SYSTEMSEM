# get kmeans clusters for target words for each languages

library(feather)
library(tidyverse)
library(data.table)
library(parallel)


############# PARAMETERS #############

CLUSTER_INPATH <- "../../data/v3/target_word_cluster_assignments_by_lang.csv"
XLING_CLUSTER_HYPOTHESES <- "../../data/v3/hypothesis_xling_clusters3.csv" 
TARGET_WORDS2 <-  "../../data/v3/target_words2.csv" 
OUTFILE <- "../../data/v3/target_cluster_langs2.csv" 

cluster_assignment = read_csv(CLUSTER_INPATH, col_names = c("word", "cluster", "lang",
                                                    "nclusters")) %>%
  select(-nclusters) 

hypotheses <- read_csv(XLING_CLUSTER_HYPOTHESES)

get_prop_same_by_target_group <- function(this_lang, hyp){
  
  cluster_assignment %>%
    filter(lang == this_lang) %>%
    right_join(hyp) %>%
    group_by(category_label) %>%
    summarize(n_total =  n(),
              prop_distinct = 1-(length(unique(cluster))/ n()),
              n_distinct = length(unique(cluster))) %>%
    mutate(lang = this_lang) %>%
    select(lang, everything())

}

all_words <- map_df(unique(cluster_assignment$lang), 
                    get_prop_same_by_target_group,
                    hypotheses)

all_words %>%
  #filter(n_distinct <= 2) %>%
  filter(prop_distinct >= .5) %>%
  count(category_label) %>%
  arrange(-n) %>%
  data.frame()

target_categories <- c("animal", "food", "earth", "body", "human_development", "health", "religion",
                       "democracy", "geography", "time", "disaster", "profession", "political_movements", "civic", "justice")



get_words_in_top_cluster <- function(this_lang, hyp){
  
  lang_clusters <- cluster_assignment %>%
    filter(lang == this_lang) %>%
    right_join(hyp) %>%
    count(category_label,cluster) %>%
    arrange(category_label, -n) %>%
    group_by(category_label) %>%
    slice(1) %>%
    #filter(category_label %in% target_categories) %>%
    data.frame() %>%
    select(-n)
  
  cluster_assignment %>%
    filter(lang == this_lang) %>%
    right_join(hyp) %>%
    group_by(category_label)  %>%
    right_join(lang_clusters)
  
}


top_words <- map_df(unique(cluster_assignment$lang), 
                    get_words_in_top_cluster,
                    hypotheses)

critical_words <- top_words %>%
  count(category_label, word) %>%
  arrange(category_label, -n) %>%
  group_by(category_label) %>%
  slice(1:3) %>%
  select(-n) %>%
  data.frame()

write_csv(critical_words, TARGET_WORDS2)


good_langs <- cluster_assignment %>%
  #filter(lang %in% target_langs) %>%
    right_join(critical_words) %>%
    group_by(lang, category_label) %>%
    summarize(n_distinct = length(unique(cluster))) %>%
    filter(n_distinct == 1) %>%
    select(-n_distinct)

good_word_good_langs_with_clusters_ids <- good_langs %>%
  left_join(critical_words) %>%
  left_join(cluster_assignment) %>%
  distinct(lang, category_label, cluster)

write_csv(good_word_good_langs_with_clusters_ids, OUTFILE)

