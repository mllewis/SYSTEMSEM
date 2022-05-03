# get target words from thompson et al. analysis (N = 1010)
# paper reports 1010 with maybe some filtering? but 1010 definitely implied - unclear where this number comes from:
# alignments-nel-wiki-trl.csv (the pairwise alignments) has 933
# primary-data-stats-by-concept.csv has 1,025
# semantic-domains-and-their-concepts.csv has 1031


library(tidyverse)
library(here)
library(janitor)

THOMPSON_ALIGNMENTS <- here("analyses/thompson_analyses/thompson_materials/additional-datasets/primary-data-summaries/primary-data-stats-by-concept.csv")
THOMPSON_DOMAINS <- here("analyses/thompson_analyses/thompson_materials/additional-datasets/primary-data-summaries/semantic-domains-and-their-concepts.csv")

OUTFILE <- here("analyses/thompson_analyses/data/thompson_words_with_alignments.csv")

target_words <- read_csv(THOMPSON_ALIGNMENTS) %>%
  clean_names() %>%
  select(-x1)

target_domains <- read_csv(THOMPSON_DOMAINS)  %>%
  clean_names() %>%
  select(english_word_form, concept_id, semantic_domain)

words_with_alignments <- full_join(target_words, target_domains) %>%
  mutate(english_word_form= tolower(english_word_form))

write_csv(words_with_alignments, OUTFILE)


