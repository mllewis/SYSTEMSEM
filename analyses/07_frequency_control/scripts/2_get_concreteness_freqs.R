# get frequency for words by concreteness deciles (ETS)
# frequency estimates come from essays

library(tidyverse)
library(here)
library(langcog)

TARGET_WORDS <-  here("analyses/02_concreteness_semantics/data/ets/concreteness_deciles.csv")
WORD_FREQS <- here("analyses/07_frequency_control/data/essay_word_counts.csv")
FREQ_SUMMARY_OUT <- here("analyses/07_frequency_control/data/freq_by_ets_decile.csv")

target_words <- read_csv(TARGET_WORDS)

word_freqs <- read_csv(WORD_FREQS, col_names  = c("word", "count", "essay_id"),
                       col_types = c("cdc"), skip = 1) %>%
  mutate(word = tolower(word)) # deal with TRUE case from python

ets_word_counts <- word_freqs %>%
  group_by(word) %>%
  summarize(word_count = sum(count)) %>%
  mutate(log_word_count = log(word_count)) %>%
  right_join(target_words) %>%
  drop_na() # "Cannot" doesn't appear in freq list

tiles_by_frequency <- ets_word_counts %>%
  group_by(conc_tile) %>%
  multi_boot_standard(col = "log_word_count")

write_csv(tiles_by_frequency, FREQ_SUMMARY_OUT)

