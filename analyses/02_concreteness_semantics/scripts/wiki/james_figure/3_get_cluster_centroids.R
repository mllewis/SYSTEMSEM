# Read in model for each language and get centroid of each semantic cluster
# writes NCLUSTERS x NLANGUAGES csv

library(tidyverse)
library(feather)
library(data.table)
library(parallel)

LANGS <- c("ar", "bn", "bg", "zh", "nl",  "fr", "de", "el", "en", "gu", "hi", "id", "it", "kn", "ko",
           "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "tl", "ta", "te",  "tr", "ur", 
           "fa", "es") 

MODEL_PATH <- "/Volumes/wilbur_the_great/fasttext_models_concreteness_words/wiki_conc_words_"
SEMANTICS_PATH <- "../../data/v3/target_word_cluster_assignments_by_lang.csv"
OUTPATH <- "../../data/v3/lang_semantics_cluster_by_lang_centroids.csv"
NCLUSTERS <- 3

####################### FUNCTIONS ######################
# wrapper function for reading and writing
get_cluster_centroids <- function(current_lang, 
                                  model_prefix,
                                  all_semantic_clusters,
                                  outpath){
  
  print(paste0("====== ", current_lang,  " ======"))
  
  # subset cluster assignments 
  current_clusters <- all_semantic_clusters  %>%
    filter(lang == current_lang) %>%
    select(-lang, -nclusters) %>%
    rename(target_word = word)
  
  # read in model
  file_name <- paste0(model_prefix, current_lang, ".feather")
  
  current_model <- read_feather(file_name) %>%
    select(-lang) %>%
    filter(target_word %in% targ_words) %>%
    arrange(target_word)
  
  # get model for only target words
  words_in_model <- current_model %>%
    left_join(current_clusters) %>%
    select(-target_word) %>%
    data.table(key = "cluster") 
  
  centroids <- words_in_model[, lapply(.SD,mean, na.rm = T), by = cluster] %>%
    mutate(lang = current_lang) %>%
    select(lang, cluster, everything())
  
  write_csv(centroids,outpath, append = T)

}

############# OTHER DATA ##########
semantic_clusters <- read_csv(SEMANTICS_PATH, col_names = c("word", "cluster", "lang",
                                                            "nclusters"))  

############# DO THE THING ##########

# INITIATE CLUSTER
cluster <- makeCluster(NCLUSTERS, type = "FORK")

# WRAPPER FUNCTION
parallel_wrapper <- function(lang){
  get_cluster_centroids(lang, 
                        MODEL_PATH,
                        semantic_clusters,
                        OUTPATH)
}

# DO THE THING (IN PARALLEL)
parLapply(cluster,LANGS, parallel_wrapper)


