# get kmeans clusters for target words

library(feather)
library(tidyverse)
library(data.table)
library(here)

############# PARAMETERS #############
ENG_MODEL_PATH <- "/Volumes/wilbur_the_great/fasttext_models/wiki.en.vec"
TARGET_WORDS_PATH <- here("analyses/02_concreteness_semantics/data/ets/concreteness_deciles.csv")
OUT_PATH <- here("analyses/02_concreteness_semantics/data/ets/target_word_cluster_assignments_ets.csv")
N_CLUSTERS <- 10

############# GET ENGLISH MODEL FOR CLUSTERING #############

eng_model <- fread(
  ENG_MODEL_PATH,
  header = FALSE,
  skip = 1,
  quote = "",
  key = "V1",
  encoding = "UTF-8",
  data.table = TRUE)

target_words_df <- read_csv(TARGET_WORDS_PATH)
target_words <- target_words_df %>%
  distinct(word) %>%
  unlist(use.names = F)

target_word_model <- filter(eng_model, V1 %in% target_words)

############# GET CLUSTERS #############

# this code is for finding the optimal cluster
# compute average silhouette for k clusters (from: https://uc-r.github.io/kmeans_clustering)
#avg_silhouette <- function(k, df) {
#  print(k)
#  km.res <- kmeans(df, centers = k, nstart = 50)
#  ss <- silhouette(km.res$cluster, dist(df))
#  mean(ss[, 3])
#}

# tidy and scale data
d_clean <- target_word_model %>%
  column_to_rownames("V1") %>%
  scale(.)

# get model with optimal number of clusters
#k_values <- 2:MAX_CLUSTERS
#avg_sil_values <- map_dbl(k_values, avg_silhouette, d_clean)
#optimal_num_clusters <- which.max(avg_sil_values) + 1
#optimal_clusters <- kmeans(d_clean, centers = optimal_num_clusters, nstart = 25)

optimal_clusters <- kmeans(d_clean, centers = N_CLUSTERS,
                           iter.max = 50)

cluster_assignments <- data.frame(cluster = optimal_clusters$cluster) %>%
  rownames_to_column("word")

write_csv(cluster_assignments, OUT_PATH)
