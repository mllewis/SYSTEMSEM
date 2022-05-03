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
rm(eng_model)
############# GET CLUSTERS #############

# Ward
# tidy and scale data
d_clean <- target_word_model %>%
  column_to_rownames("V1") %>%
  dist()

all_clusters <- hclust(d_clean, method = "average") # ll go into one

target_clusters <- cutree(all_clusters, k = N_CLUSTERS)

cluster_assignments <- data.frame(cluster = target_clusters) %>%
  rownames_to_column("word")

count(cluster_assignments, cluster) %>%
  arrange(-n)


# EM
library(mclust)
d_clean <- target_word_model %>%
  column_to_rownames("V1") %>%
   scale(.)

BIC <- mclustBIC(d_clean) # init

#emobj <- simple.init(d_clean, nclass = 10)
# ret <- emcluster(d_clean)

count(cluster_assignments, cluster) %>%
  arrange(-n)


# Mean shift
library(meanShiftR)

d_clean <- target_word_model %>%
  column_to_rownames("V1") %>%
  scale(.)

cluster_assignment <- meanShift(d_clean,iterations = 1000,
                                nNeighbors = 100)

cluster_assignment$assignment %>%
  as.data.frame() %>%
  count(V1) %>%
  arrange(-n)

x <- matrix(runif(20),10,2)
classification <- meanShift(x,
                            algorithm="KDTREE",
                            nNeighbor=8,
                            parameters=c(5,7.1) )

x <- matrix(runif(20),10,2)
classification <- meanShift(x,x)


library(LPCM)
j = ms(d_clean)

j$closest.label %>%
  as.data.frame() %>%
  tail()
  count(.) %>%
  arrange(-n)



#write_csv(cluster_assignments, OUT_PATH)
