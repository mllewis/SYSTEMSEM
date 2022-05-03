# get kmeans clusters for target words for each languages, with fixed number of clusters
# num clusters = num words/200

library(feather)
library(tidyverse)
library(data.table)
library(parallel)

############# PARAMETERS #############
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models_concreteness_models/"
OUT_PATH <- "../../data/v3/target_word_cluster_assignments_by_lang.csv"
TARGET_WORDS <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/L2ETS/studies/study2/analyses/8_tensor_factorization/A_preprocessing_R/wiki/wiki_lang/target_common_words.csv"

MODEL_PATH <- "/Volumes/wilbur_the_great/fasttext_models_concreteness_words/wiki_conc_words_"

LANGS <- c("ar", "bn", "bg", "zh", "nl",  "fr", "de", "el", "en", "gu", "hi", "id", "it", "kn", "ko",
           "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "tl", "ta", "te",  "tr", "ur", 
           "fa", "es") 
NCLUSTERS <- 4 # for parallel computer (not semantic cluster!)
NSEMANTIC_CLUSTERS = 50

target_words <- read_csv(TARGET_WORDS) %>% pull(target_word)


############# FUNCTIONS ##########
get_clusters_by_lang <- function(current_lang, 
                                  targ_words,
                                  model_prefix, 
                                  outpath,
                                  n_clusters){
  
  print(paste0("====== ", current_lang,  " ======"))
  
  
  # read in model
  file_name <- paste0(model_prefix, current_lang, ".feather")
  
  current_model <- read_feather(file_name) %>%
    filter(target_word %in% targ_words) %>%
    arrange(target_word)
  
  # tidy and scale data
  d_clean <- current_model %>%
    select(-lang) %>%
    column_to_rownames("target_word") %>%
    scale(.)
  
  fixed_clusters <- kmeans(d_clean, centers = n_clusters,
                             iter.max = 50)
  
  cluster_assignments <- data.frame(cluster = fixed_clusters$cluster) %>%
    rownames_to_column("target_word") %>%
    mutate(lang = current_lang, 
           n_cluster = n_clusters)
  
  write_csv(cluster_assignments, outpath, append = T)
}

############# DO THE THING #############

# INITIATE CLUSTER
cluster <- makeCluster(NCLUSTERS, type = "FORK")

# WRAPPER FUNCTION
parallel_wrapper <- function(lang){
  get_clusters_by_lang(lang,
                       target_words,
                       MODEL_PATH, 
                       OUT_PATH,
                       NSEMANTIC_CLUSTERS)
}

# DO THE THING (IN PARALLEL)
parLapply(cluster, LANGS, parallel_wrapper)


