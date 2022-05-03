# pos control analysis - get pairwise distances for lang/score/word type
library(tidyverse)
library(data.table)
library(feather)
library(here)

POS_PATH <- here("analyses/06_syntax_control/data/SUBTLEX-US frequency list with PoS information text version.txt")
WORD_PATH <- here("analyses/02_concreteness_semantics/data/ets/target_word_cluster_assignments_ets.csv")
MODEL_PATH <-  here("data/processed/models/L1_high_low_models/")
OUTPATH <- "/Volumes/wilbur_the_great/syntax_control_analysis_for_paper/syntax_control_analysis_for_paper/"

LANGS <- c('ARA', 'BEN', 'BUL', 'CHI', 'DUT', 'ENG', 'FAS', 'FRE', 'GER', 'GRE', 'GUJ',
           'HIN', 'IBO', 'IND', 'ITA', 'JPN','KAN', 'KOR', 'MAL', 'MAR', 'NEP', 'PAN', 'POL',
           'POR', 'RUM', 'RUS', 'SPA', 'TAM', 'TEL', 'TGL', 'THA', 'TUR', 'URD', 'VIE', 'YOR')

pos <- read_tsv(POS_PATH) %>%
  janitor::clean_names() %>%
  rename(pos = dom_po_s_subtlex) %>%
  select(word, pos)

target_words <- read_csv(WORD_PATH) %>%
  select(-cluster)

target_words_with_pos <- target_words %>%
  left_join(pos) %>%
  mutate_all(as.factor) %>%
  mutate(pos = case_when(pos %in%  c("Article", "Conjunction", "Determiner", "Pronoun",
                                        "Preposition") ~ "grammatical",
                          pos %in%  c("Interjection", "Adverb", "Adjective",
                                      "Verb", "Noun") ~ "content",
                          TRUE ~ "other")) %>%
  filter(pos != "other") %>%
  group_by(pos) %>%
  nest()

get_pairwise_dist_between_words <- function(wordvecs, words){

  word_word_dists <- coop::cosine(t(as.matrix(wordvecs))) # this is fast

  wide_word_word_dists <- word_word_dists %>%
    as.data.frame(row.names = words) %>%
    setNames(., words)  %>%
    rownames_to_column(var = "w1")

  long_word_word_dists <- gather(wide_word_word_dists, "w2", "cos_dist", -w1)

  long_word_word_dists
}

get_pairwise_distances <- function(arg_values, outpath, model_path, target_words){
  current_group <- arg_values[1]
  current_word_type <- arg_values[2]
  current_lang <- arg_values[3]

  # get grammatical/content word lists
  target_word_set <- filter(target_words, pos == current_word_type) %>%
    pull(data) %>%
    pluck(1) %>%
    pull(word)

  current_wordvecs <- read_feather(paste0(model_path, "wordvecs_", current_lang, "-", current_group, ".model.feather") )
  current_words <- read_feather(paste0(model_path, "words_", current_lang, "-",current_group, ".model.feather")) %>%
    rename(word = `0`)

  df <-  current_words %>%
    bind_cols(current_wordvecs)  %>%
    data.table() %>%
    .[word %in% target_word_set]

  these_dists <- get_pairwise_dist_between_words(df[,-c("word")], df$word)

  write_feather(these_dists, paste0(outpath,
                                    current_lang, "_",
                                    current_word_type, "_",
                                    current_group, "_", "word_dists.feather"))

}

### DO THE THING ##
cross(list(c("low", "high"),
           c("grammatical", "content"),
           LANGS)) %>%
  walk(get_pairwise_distances, OUTPATH, MODEL_PATH, target_words_with_pos)
