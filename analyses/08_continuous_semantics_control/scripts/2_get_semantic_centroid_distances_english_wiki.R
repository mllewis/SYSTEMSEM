# get kmeans clusters for target words

library(feather)
library(tidyverse)
library(data.table)
library(here)

############# PARAMETERS #############
ENG_MODEL_PATH <- "/Volumes/wilbur_the_great/fasttext_models/wiki.en.vec"
CLUSTER_ASSIGNMENTS_WIKI <- here("analyses/02_concreteness_semantics/data/wiki/target_word_cluster_assignments.csv")
CLUSTER_DISTANCES_WIKI <- here("analyses/08_continuous_semantics_control/data/target_word_cluster_distances_wiki.csv")

N_CLUSTERS <- 10

############# GET MODEL FOR RELEVANT WORD #############

cluster_assignments <- read_csv(CLUSTER_ASSIGNMENTS_WIKI)

eng_model <- fread(
  ENG_MODEL_PATH,
  header = FALSE,
  skip = 1,
  quote = "",
  key = "V1",
  encoding = "UTF-8",
  data.table = TRUE)

target_word_model <- eng_model %>%
  right_join(cluster_assignments, by = c("V1" = "word")) %>%
  select(-V1) %>%
  data.table()

centroids <- target_word_model[, lapply(.SD,mean), by=cluster]

cluster_distances <- coop::cosine(t(centroids[,-1])) %>%
  as.data.frame() %>%
  mutate(cluster = centroids$cluster)

colnames(cluster_distances) <- c(centroids$cluster, "cluster2")

cluster_distances_long <- cluster_distances %>%
  gather(cluster1, cosine_distance, -cluster2)

write_csv(cluster_distances_long, CLUSTER_DISTANCES_WIKI)

