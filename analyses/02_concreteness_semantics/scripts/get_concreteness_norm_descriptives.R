# get mean and standard deviations for concreteness norms

library(tidyverse)
library(here)
library(janitor)
library(tidyboot)

ETS_WORDS <- here("analyses/02_concreteness_semantics/data/ets/concreteness_deciles.csv")
WIKI_WORDS <- here("analyses/02_concreteness_semantics/data/wiki/target_translations_xling_words.csv")
BRYSBAERT_PATH <- here("analyses/02_concreteness_semantics/data/brysbaert_corpus.csv")


brysbaert_norms <- read_csv(BRYSBAERT_PATH) %>%
  clean_names() %>%
  select(word, conc_sd, total)

ets_words <- read_csv(ETS_WORDS) %>%
  select(word) %>%
  mutate(corpus = "ets") %>%
  left_join(brysbaert_norms)

wiki_words <- read_csv(WIKI_WORDS) %>%
  distinct(word) %>%
  mutate(corpus = "wiki") %>%
  left_join(brysbaert_norms)

descriptives <- bind_rows(ets_words, wiki_words) %>%
  mutate_if(is.factor, is.character)

n_rater_descriptives <- descriptives %>%
  group_by(corpus) %>%
  summarize(mean = mean(total),
            sd= sd(total))


sd_descriptives <- descriptives %>%
  group_by(corpus) %>%
  summarize(mean_sd= mean(conc_sd))

# there were about 32 words that were rated by 6000 participants, the remaining were rated by ~28
brysbaert_norms %>%
  filter(total < 500) %>%
  pull(total) %>%
  hist()

descriptives %>%
  filter(total <1000) %>%
ggplot( aes(x = total)) +
  geom_histogram() +
  facet_wrap(~corpus)


