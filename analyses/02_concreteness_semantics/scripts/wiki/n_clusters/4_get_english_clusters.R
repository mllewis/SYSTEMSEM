# get kmeans clusters for target words

library(feather)
library(tidyverse)
library(data.table)
library(here)

############# PARAMETERS #############
ENG_MODEL_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/exploratory_analyses/0_exploration/wiki.en.vec" #/Volumes/wilbur_the_great/fasttext_models/wiki.en.vec"
TARGET_WORDS_PATH <- here("analyses/02_concreteness_semantics/data/wiki/target_translations_xling_words.csv")
OUT_PATH <- here("analyses/02_concreteness_semantics/data/wiki/target_word_cluster_assignments_all.csv")
N_CLUSTERS <- c(10, 15, 20, 25, 50, 75, 100, 150, 200, 250)

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


# tidy and scale data
d_clean <- target_word_model %>%
  column_to_rownames("V1") %>%
  scale(.)

get_clusters <- function(n_clusters, df) {
  optimal_clusters <- kmeans(df,
                             centers = n_clusters,
                             iter.max = 50,
                             algorithm = "Hartigan-Wong")

  cluster_assignments <- data.frame(cluster = optimal_clusters$cluster) %>%
    rownames_to_column("word") %>%
    mutate(n_clusters = n_clusters)
  cluster_assignments
}

all_clusters <- map_df(N_CLUSTERS, get_clusters, d_clean)

write_csv(all_clusters, OUT_PATH, append = T)
