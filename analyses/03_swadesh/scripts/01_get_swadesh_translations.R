# Use google translation API to get translations for 22 swadesh words

library(tidyverse)
library(feather)
library(googleLanguageR)
source("essay_translation_helper.R")
gl_auth("data/L2ETS\ prompt\ translations-a8edd99b5aa9.json") # authenticate google api

####################### PARAMETERS ######################
SWADESH_PATH <- here("analyses/03_swadesh/data/swadesh_words.csv") # these forms are from Youn et al. 
OUTPUT_PATH <- here("analyses/03_swadesh/data/swadesh_words_translated.csv")
GOOGLE_LANGS <- c("ar", "bn", "bg", "zh", "nl",  "fr", "de", "el", "en", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko",
                  "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo",
                  "fa") 

####################### DO THE TRANSLATIONS #################
swadesh <- read_csv(SWADESH_PATH)  %>%
  mutate(swadash_word = tolower(swadash_word))

lw_combos <- expand.grid(GOOGLE_LANGS, 
                         swadesh$swadash_word) %>%
  rename(langs = Var1, words = Var2)

map2_df(as.character(lw_combos$langs), 
          as.character(lw_combos$word), 
          get_essay_words_translation,
          OUTPUT_PATH) 
