# get pairwise distances between all words
library(tidyverse)
library(here)

MODELS <- map(0:34, ~paste0("random-", .)) %>%
  unlist()
MODEL_PATH <-  here("data/processed/models/random/")
OUTPATH <-  here("analyses/06_random_control/data/pairwise_word_dists/")
TARGET_WORDS <-  here("analyses/06_random_control/data/concreteness_deciles_random.csv")

target_words <- read_csv(TARGET_WORDS)


get_pairwise_dist_between_words <- function(wordvecs, words){

  word_word_dists <- coop::cosine(t(as.matrix(wordvecs))) # this is fast

  wide_word_word_dists <- word_word_dists %>%
    as.data.frame(row.names = words) %>%
    setNames(., words)  %>%
    rownames_to_column(var = "w1")

  long_word_word_dists <- gather(wide_word_word_dists, "w2", "cos_dist", -w1)

  long_word_word_dists
}


get_dists <- function(lang, model_path, target_words, outpath){
  current_wordvecs <- read_csv(paste0(model_path, "wordvecs_", lang, ".model.csv") )
  current_words <- read_csv(paste0(model_path, "words_", lang, ".model.csv")) %>%
    rename(word = `0`)

  df <-  current_words %>%
    bind_cols(current_wordvecs)  %>%
    data.table() %>%
    .[word %in% target_words]

  these_dists <- get_pairwise_dist_between_words(df[,-c("word")], df$word)

  write_feather(these_dists, paste0(outpath, lang ,"_common_word_dists.feather"))

}

walk(MODELS, get_dists, MODEL_PATH, target_words$word, OUTPATH)
