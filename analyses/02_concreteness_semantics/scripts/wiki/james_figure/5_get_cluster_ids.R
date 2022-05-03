# Target cluster does it have the three words together for each of my aprior categories

library(tidyverse)
library(data.table)

####################### PARAMETERS ######################

INPATH <- "../../data/v2/target_word_cluster_assignments_by_lang.csv"
TARGETINPATH <- "../../data/v2/fig_target_words.csv"
OUTPATH <- "../../data/v2/target_cluster_ids.csv"

#### DATA ###
langs_to_cats <- data.frame(category = c("time", "time", "time", "time", "time", "time", 
                        "mountain", "mountain", "mountain", "mountain", "mountain", "mountain",
                        "moon",  "moon",  "moon",  "moon",  "moon"),
            language = c("el", "es", "hi", "id", "ko", "pa", 
                         "de", "es", "it", "ko", "pt", "te",
                         "es", "kn", "te", "gu", "ru"))

####################### GET DISTANCES ######################

cluster_assignments <- read_csv(INPATH, col_names = c("word", "cluster", "lang",
                                                     "nclusters", "divisor"))  %>%
  select(word, cluster, lang)

target_words_raw <- read_csv(TARGETINPATH)  %>%
  mutate(id = 1:n()) %>%
  gather("x", "word", -category, -id) %>%
  select(-x)

target_words <- target_words_raw %>%
  distinct(word) %>%
  pull(word)

cluster_assignments_target_clusters <- cluster_assignments %>%
  filter(word %in% target_words) %>%
  left_join(target_words_raw) %>%
  rename(language = lang)
  
target_clusters <- cluster_assignments_target_clusters %>%
  inner_join(langs_to_cats) %>%
  left_join(target_words_raw)

target_clusters %>%
  count(id, language, cluster)%>%
  data.frame()

write_csv(target_clusters, OUTPATH)
