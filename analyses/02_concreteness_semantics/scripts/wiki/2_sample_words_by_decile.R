# get sample of brysbaert words that are common across language for concreteness/semantics analysis

library(feather)
library(tidyverse)
library(here)

####################### PARAMETERS ######################
BRYSBAERT_PATH <- here("analyses/02_concreteness_semantics/data/brysbaert_corpus.csv")
COMMON_WORDS <- here("analyses/02_concreteness_semantics/data/wiki/filtered_common_brysbaert_words_across_models.csv")
MIN_LANGS <- 10
TRANSLATION_PATH <- here("analyses/02_concreteness_semantics/data/wiki/brysbaert_words_translated.feather")
OUTPATH <- here("analyses/02_concreteness_semantics/data/wiki/target_translations_xling_words.csv")

N_SAMPLE_WORDS_PER_TILE <- 1000
N_TILE <- 10

############# SAMPLE WORDS AND SAVE ##########
brysbaert_norms <- read_csv(BRYSBAERT_PATH) %>%
  select(Word, Conc.M) %>%
  rename(word = Word) %>%
  filter(!is.na(word))  %>%
  mutate(concreteness_tile = ntile(Conc.M, N_TILE))  %>%
  select(-Conc.M)

# sample words from each concreteness tile
common_words <- read_csv(COMMON_WORDS)
target_words <- common_words %>%
  left_join(brysbaert_norms) %>%
  group_by(concreteness_tile) %>%
  sample_n(N_SAMPLE_WORDS_PER_TILE)

# merge in translations
target_word_translations <- read_feather(TRANSLATION_PATH) %>% # read in all translations
  filter(word %in% target_words$word) %>% # filter to only the ones we sampled
  mutate(translation = tolower(translation)) %>% # make lower case so can index into model
  filter(word != translation | lang == "en") %>% # remove cases where they use english word
  left_join(target_words %>% select(-n))

write_csv(target_word_translations, OUTPATH)
