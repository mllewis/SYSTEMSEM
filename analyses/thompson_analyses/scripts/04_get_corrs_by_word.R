# get pairwise correlations between words

library(tidyverse)
library(here)
library(langcog)

####################### PARAMETERS ######################

OUTFILE <- here("analyses/thompson_analyses/data/corrs_by_word.csv")
CORRS_BY_LANG_PAIR <- here("analyses/thompson_analyses/data/lang_pairwise_word_correlations.csv")

corrs_by_word <- read_csv(CORRS_BY_LANG_PAIR,
                          col_names = c("word", "corr", "l1", "l2"))


target_words <- corrs_by_word %>%
 count(word) %>%
  filter(n >450)

mean_corrs_by_word <- corrs_by_word %>%
  #filter(word %in% target_words$word) %>%
  group_by(word) %>%
  multi_boot_standard(col = "corr") %>%
  rename(corr = mean)
 # summarize(corr = mean(corr))

write_csv(mean_corrs_by_word, OUTFILE)
