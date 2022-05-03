# Get pairwise distances between swadesh words for each language

library(tidyverse)
library(feather)
library(data.table)
library(parallel)
library(here)

####################### PARAMETERS ######################
TARGET_WORD_PATH <- here("analyses/03_swadesh/data/swadesh_words_translated.csv")
SWADESH_WORDS <- here("analyses/03_swadesh/data/swadesh_words.csv")
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/"
OUT_PREFIX <- "/Volumes/wilbur_the_great/pairwise_swadesh_words/pairwise_swadesh_"

LANGS <- c("ar", "bn", "bg", "zh", "nl",  "fr", "de", "el", "en", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko",
                  "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo",
                  "fa") 

NCLUSTERS <- 4

############# FUNCTIONS ##########

# wrapper function for reading and writing
save_pairwise_dists_trans <- function(current_lang, 
                                      target_translations, 
                                      model_prefix,
                                      out_prefix,
                                      word_id_to_word){
  
  print(paste0("====== ", current_lang,  " ======"))
  
  # subset translations
  lang_translations <- target_translations %>%
    filter(lang == current_lang) %>%
    select(word, translation, word_id, morpheme_id) %>%
    filter(!is.na(translation))

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
    right_join(lang_translations, by = c("V1" = "translation")) %>%
    rename(translation = V1)  %>%
    select(word, word_id, morpheme_id, translation, everything())
  
  calculated_vectors <- subsetted_model %>%
    select(-translation) %>%
    group_by(word, word_id) %>% # mean across morphemes
    summarise_at(vars(V2:V301), mean, na.rm = TRUE) %>%
    group_by(word_id) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) %>% # mean across words
    left_join(word_id_to_word %>%
                distinct(word_id, .keep_all = T)
              ) %>%
    select(-word_id) %>%
    select(word, everything())
  
  pairwise_cosines_wide <- coop::cosine(t(as.matrix(calculated_vectors[,-1])))
  
  wide_word_word_dists <- pairwise_cosines_wide %>%
    as.data.frame(row.names = calculated_vectors$word) %>%
    setNames(., calculated_vectors$word)  %>%
    rownames_to_column(var = "word1")

  tidy_long  <- gather(wide_word_word_dists, 
                       "word2",
                       "cos_dist", 
                       -word1) %>%
    mutate(lang = current_lang) %>%
    select(lang, word1, word2, cos_dist) %>%
    arrange(word1)
  
  file_name <- paste0(out_prefix ,current_lang, "_wiki.feather")
  write_feather(tidy_long, file_name)
}

############# OTHER DATA ##########
swadesh_words <- read_csv(SWADESH_WORDS) %>%
  rename(word = swadash_word) %>%
  mutate(word_id = paste0("w", word_id),
         word = tolower(word))

# break up multiword translation into 
target_translations <- read_csv(TARGET_WORD_PATH, 
                                col_names = c("lang", "word", "translation")) %>%
  left_join(swadesh_words) %>%
  mutate(translation = tolower(translation)) %>%
  select(lang, word, word_id, translation) %>%
  separate(translation, 
           c("m1", "m2", "m3", "m4", "m5"), " ") %>%
  gather("morpheme_id", "translation", c(-1:-3)) %>%
  mutate_all(as.factor)

############# DO THE THING ##########

# INITIATE CLUSTER
cluster <- makeCluster(NCLUSTERS, type = "FORK")

# WRAPPER FUNCTION
parallel_wrapper <- function(lang){
  save_pairwise_dists_trans(lang, target_translations, MODEL_PREFIX, 
                            OUT_PREFIX, swadesh_words)
}


# DO THE THING (IN PARALLEL)
parLapply(cluster, LANGS, parallel_wrapper) 


save_pairwise_dists_trans("bn", target_translations, MODEL_PREFIX, OUT_PREFIX, swadesh_words)
