# get wikipedia page views and prop pages views by language
library(tidyverse)
library(here)
library(janitor)

POP_COUNTS_DATA <- here("analyses/10_wikipedia_stats/data/raw/rspb20141574supp2_amano.csv")
DATA_PATH <- here('analyses/10_wikipedia_stats/data/raw/pageview_data')
LANG_NAMES <- here('analyses/04_predicting_semantic_sim/data/lang_distance_metrics/linguistic/data/iso_to_wals_for_ling_dists.csv')
OUTFILE <- here("analyses/10_wikipedia_stats/data/processed/page_views_by_language.csv")

GOOGLE_LANGS <- c("ar", "bn", "bg", "zh", "nl", "en", "fr", "de", "el", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko",
                  "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo",
                  "fa")
arabic_dialects <- c("acm", "aeb", "afb", "ajp", "apc", "ary", "arz", "ayn", "shu")

all_files <- list.files(DATA_PATH, full.names = T)

all_data <- map_df(all_files, read_table2, col_names = c("project", "-", "nrequests", "nbytes"), col_types = "ccdd")
lang_names <- read_csv(LANG_NAMES) %>%
  mutate(wiki = GOOGLE_LANGS)

pop_counts <- read_csv(POP_COUNTS_DATA) %>%
  clean_names() %>%
  select(language_iso, population_size) %>%
  mutate(language_iso = case_when(language_iso %in% arabic_dialects ~ "arb",
         TRUE ~ language_iso)) %>%
  rename(iso = language_iso,
         speakers_l1 = population_size) %>%
  right_join(lang_names %>% select(iso, wiki)) %>%
  group_by(iso, wiki) %>% # sum across arabic dialects
  summarize(speakers_l1 = sum(speakers_l1))

total_requests_by_language <- all_data %>%
  select(-`-`, -"nbytes") %>%
  filter(!str_detect(project, "\\.")) %>%
  group_by(project) %>%
  summarize(total_requests = sum(nrequests)) %>%
  filter(project %in% lang_names$wiki) %>%
  rename(wiki = project)

requests <- full_join(pop_counts, total_requests_by_language)

write_csv(requests, OUTFILE)
