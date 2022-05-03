# get words most frequent across langauges (common words), and then filter based on some criteria
# (minimum number of languages that have each word)

library(feather)
library(tidyverse)
library(data.table)
library(here)

# (1) Get all words sampled for concrete deciles
# (2) get english embeddings
# (3) cluster in english
# (4) For each language-pair, get correlation of those distances within vs. across each cluster
# (5) For each language-pair, get corelation of pairwise cluster centroids

####################### PARAMETERS ######################

TRANSLATION_PATH <-here("analyses/02_concreteness_semantics/data/wiki/brysbaert_words_translated.feather")
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/"
COMMON_WORDS_OUTPATH <- here("analyses/02_concreteness_semantics/data/wiki/common_brysbaert_words_across_models.csv")
FILTERED_COMMON_WORDS_OUTPATH <- here("analyses/02_concreteness_semantics/data/wiki/filtered_common_brysbaert_words_across_models.csv")
MIN_LANGS <- 10

LANGS <- c( "el", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko", "ar", "bn", "bg", "zh", "nl",  "fr", "de",
             "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi",
            "yo","fa", "en")

############# FUNCTIONS ##########
get_words_in_models <- function(current_lang,
                                target_translations,
                                model_prefix,
                                outpath){

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
    words_in_model <- model[V1 %in% lang_translations$translation] %>%
      left_join(lang_translations %>% select(word, translation),
                by = c("V1" = "translation")) %>%
      rename(translation = V1) %>%
      select(word, translation) %>%
      distinct(translation, .keep_all  = T) %>%
      mutate(lang = current_lang)

    write_csv(words_in_model, outpath, append = T)

}


############# DO THE THING ##########
translated_words <- read_feather(TRANSLATION_PATH) %>%
  filter(word != translation) %>%
  mutate(translation = tolower(translation))

walk(LANGS, get_words_in_models,
                        translated_words,
                        MODEL_PREFIX,
                        COMMON_WORDS_OUTPATH)

words_in_models <- read_csv(COMMON_WORDS_OUTPATH,
                            col_names = c("word", "translation", "lang"))

# filter to words that english has
eng_model <- fread(
  paste0(MODEL_PREFIX, "wiki.", "en", ".vec"),
  header = FALSE,
  skip = 1,
  quote = "",
  key = "V1",
  encoding = "UTF-8",
  data.table = TRUE)

english_words <- eng_model[V1 %in% unique(translated_words$word)] %>%
  select(V1) %>%
  unlist(use.names = F)

common_words_df <- count(words_in_models, word) %>%
   arrange(-n)   %>%
   filter(n >= MIN_LANGS,
          word %in% english_words)

write_csv(common_words_df, FILTERED_COMMON_WORDS_OUTPATH)
