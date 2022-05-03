# get pairwise distances between all words
library(tidyverse)
library(here)

MODELS <- map(0:34, ~paste0("random-", .)) %>%
  unlist()
MODEL_PATH <-  here("data/processed/models/random/")
BRYSBAERT_PATH <- here("analyses/02_concreteness_semantics/data/brysbaert_corpus.csv")
OUTFILE <- here("analyses/06_random_control/data/concreteness_deciles_random.csv")

brysbaert_norms <- read_csv(BRYSBAERT_PATH) %>%
  select(Word, Conc.M) %>%
  filter(!is.na(Word)) %>%
  mutate(conc_tile = ntile(Conc.M, 10))

# get unique words
get_common_words_across_langs <- function(lang){
  print(lang)
  these_words <- read_csv(paste0(MODEL_PATH, "words_", lang, ".model.csv"))
  these_words$`0`
}

common_words <- map(MODELS, get_common_words_across_langs)

# all words that appear in at least 5 languages
target_words <-  flatten(common_words) %>%
  unlist() %>%
  as.data.frame() %>%
  rename(word = ".") %>%
  count(word) %>%
  arrange(-n) %>%
  left_join(brysbaert_norms %>% select(Word, conc_tile) %>% rename(word = Word)) %>%
  filter(!is.na(conc_tile)) %>%
  filter(n > 4)

write_csv(target_words, OUTFILE)

