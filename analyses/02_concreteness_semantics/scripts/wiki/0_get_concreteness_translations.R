# Use google translation API to get translations for all words in brysbaert corpus

library(tidyverse)
library(feather)
library(googleLanguageR)
library(here)
gl_auth("L2ETS\ prompt\ translations-a8edd99b5aa9.json") # authenticate google api

####################### PARAMETERS ######################
BRYSBAERT_PATH <- here("analyses/02_concreteness_semantics/data/brysbaert_corpus.csv")
OUTPUT_PATH <- "../../data/processed/concreteness/brysbaert_words_translated3.csv" # this took several runs and was merged into a feather file
GOOGLE_LANGS <- c("ar", "bn", "bg", "zh", "nl",  "fr", "de", "el", "en", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko",
                  "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo",
                  "fa")

# translate prompt using google translate
get_essay_words_translation <- function(targ_lang, word, output_path){

  if(targ_lang == "ar"){print(as.character(word))}

  if (targ_lang == "en"){
    translated_text <- word
  } else {
    translated_text <- gl_translate(as.character(word),
                                    target = targ_lang,
                                    source = "en")$translatedText
  }

  new_row <- data.frame(lang = targ_lang,
                        target_word = word,
                        translated_word = translated_text)

  write_csv(new_row, append = TRUE, path = output_path)
}

####################### DO THE TRANSLATIONS #################
brysbaert_norms <- read_csv(BRYSBAERT_PATH) %>%
  select(Word, Conc.M) %>%
  filter(!is.na(Word))

lw_combos <- expand.grid(GOOGLE_LANGS,
                         brysbaert_norms$Word[1:nrow(brysbaert_norms)]) %>%
  rename(langs = Var1, words = Var2)

map2_df(as.character(lw_combos$langs),
          as.character(lw_combos$word),
          get_essay_words_translation,
          OUTPUT_PATH)
