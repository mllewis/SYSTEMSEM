# get pairwise distances between cluster centroids

library(feather)
library(tidyverse)
library(data.table)
library(here)


############# PARAMETERS #############
ENG_MODEL_PATH <- "/Volumes/wilbur_the_great/fasttext_models/wiki.en.vec"
CLUSTER_ASSIGNMENTS <- here("analyses/02_concreteness_semantics/data/ets/target_word_cluster_assignments_ets.csv")
CLUSTER_DISTANCES <- here("analyses/08_continuous_semantics_control/data/target_word_cluster_distances_ets.csv")

############# GET MODEL FOR RELEVANT WORD #############

cluster_assignments <- read_csv(CLUSTER_ASSIGNMENTS)

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

write_csv(cluster_distances_long, CLUSTER_DISTANCES)
