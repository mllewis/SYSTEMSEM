# get target words for concreteness analysis (ETS)

library(tidyverse)
library(here)
library(feather)

LANGS <- c('ARA', 'BEN', 'BUL', 'CHI', 'DUT', 'ENG', 'FAS', 'FRE', 'GER', 'GRE', 'GUJ',
           'HIN', 'IBO', 'IND', 'ITA', 'JPN','KAN', 'KOR', 'MAL', 'MAR', 'NEP', 'PAN', 'POL',
           'POR', 'RUM', 'RUS', 'SPA', 'TAM', 'TEL', 'TGL', 'THA', 'TUR', 'URD', 'VIE', 'YOR')
MODEL_PATH <-  here("data/processed/models/L1_models/")
BRYSBAERT_PATH <- here("analyses/02_concreteness_semantics/data/brysbaert_corpus.csv")
MIN_WORDS_IN_MODEL <- 4
OUTFILE <- here("analyses/02_concreteness_semantics/data/ets/concreteness_deciles.csv")


brysbaert_norms <- read_csv(BRYSBAERT_PATH) %>%
  select(Word, Conc.M) %>%
  filter(!is.na(Word)) %>%
  mutate(conc_tile = ntile(Conc.M, 10))

# get unique words
get_common_words_across_langs <- function(lang){
  print(lang)
  these_words <- read_feather(paste0( MODEL_PATH, "words_", lang, ".model.feather"))
  these_words$`0`
}

common_words <- map(LANGS, get_common_words_across_langs)

# all words that appear in at least 5 languages
target_words <-  flatten(common_words) %>%
  unlist() %>%
  as.data.frame() %>%
  rename(word = ".") %>%
  count(word) %>%
  arrange(-n) %>%
  left_join(brysbaert_norms %>% select(Word, conc_tile) %>% rename(word = Word)) %>%
  filter(!is.na(conc_tile)) %>%
  filter(n > MIN_WORDS_IN_MODEL)

write_csv(target_words, OUTFILE)
