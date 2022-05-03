# Given a sample of words from each tile of concretess, get the pairwise distances for those words
# for all wiki model.

library(tidyverse)
library(feather)
library(data.table)
library(parallel)
library(here)

####################### PARAMETERS ######################
TARGET_WORD_PATH <- here("analyses/02_concreteness_semantics/data/wiki/target_translations_xling_words.csv")
MODEL_PREFIX <- "/Volumes/walrus/fasttext_models/"

LANGS <- c( "el", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko", "ar", "bn", "bg", "zh", "nl",  "fr", "de",
           "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo","fa", "en")
LANGS <- c( "el", "gu", "hi", "ja", "kn", "ko", "ar", "bn", "bg", "zh", "nl", "fr", "tr")

NCLUSTERS <- 1

############# FUNCTIONS ##########

# wrapper function for reading and writing
save_pairwise_dists_trans <- function(current_lang,
                                      target_translations,
                                      model_prefix){

  print(paste0("====== ", current_lang,  " ======"))

  # subset translations
  lang_translations <- target_translations %>%
    filter(lang == current_lang)

  # read in model
  file_name <- paste0(model_prefix, "wiki.", current_lang, ".vec")

  model <- fread(
    file_name,
    header = FALSE,
    skip = 1,
    quote = "",
    key = "V1",
    encoding = "UTF-8",
    data.table = TRUE)

  # get model for only target words
  subsetted_model <- model[V1 %in% lang_translations$translation] %>%
    left_join(lang_translations %>% select(word, translation),
              by = c("V1" = "translation")) %>%
    rename(translation = V1)  %>%
    select(word, translation, everything())

  pairwise_cosines_wide <- coop::cosine(t(as.matrix(subsetted_model[,c(-1,-2)])))

  word_indices <- data.frame(word = subsetted_model$word,
                             word_index = 1:length(subsetted_model$word))

  pairwise_cosines_long <- melt(pairwise_cosines_wide)

 tidy_long <- pairwise_cosines_long %>%
    left_join(word_indices, c("Var1"= "word_index")) %>%
    rename(word1 = word) %>%
    left_join(word_indices, c("Var2"= "word_index")) %>%
    rename(word2 = word,
           cos_dist = value) %>%
    mutate(lang = current_lang) %>%
    select(lang, word1, word2, cos_dist)

  file_name <- paste0("/Volumes/walrus/concreteness_distances_for_paper/pairwise_word_10x1000_",current_lang, "_wiki.feather")
  write_feather(tidy_long, file_name)
}

############# OTHER DATA ##########
target_translations <- read_csv(TARGET_WORD_PATH)

############# DO THE THING ##########

# walk(LANGS, save_pairwise_dists_trans, target_translations, tiles_to_loop)

# INITIATE CLUSTER
cluster <- makeCluster(NCLUSTERS, type = "FORK")

# WRAPPER FUNCTION
parallel_wrapper <- function(lang){
  save_pairwise_dists_trans(lang, target_translations, MODEL_PREFIX)
}

# DO THE THING (IN PARALLEL)
parLapply(cluster, LANGS, parallel_wrapper)
