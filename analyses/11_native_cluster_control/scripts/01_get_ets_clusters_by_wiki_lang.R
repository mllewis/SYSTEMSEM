# get kmeans clusters for target words

library(feather)
library(tidyverse)
library(data.table)
library(here)

############# PARAMETERS #############
TARGET_LANGS <-  c("ar", "bn", "bg", "zh", "nl",  "fr", "de", "el", "en", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko",
                                  "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo",
                                  "fa")

MODEL_PATH <- "/Volumes/walrus/fasttext_models/wiki."
TARGET_WORDS_PATH <- here("analyses/02_concreteness_semantics/data/ets/concreteness_deciles.csv")
LANG_NAMES <- here("data/processed/lang_names/ets_to_google_langcodes_complete.csv") # convert wiki to ets names
OUTPATH <- here("analyses/11_native_cluster_control/data/native_lang_clusters/")
N_CLUSTERS <- c(10, 15, 20, 25, 50, 75, 100, 150, 200, 250)

lang_names <- read_csv(LANG_NAMES) %>%
  select(ETS_lang_name, google_lang_name) %>%
  mutate(ETS_lang_name = toupper(ETS_lang_name))

target_words_df <- read_csv(TARGET_WORDS_PATH)
target_words <- target_words_df %>%
  distinct(word) %>%
  unlist(use.names = F)

get_clusters <- function(n_clusters, df) {
  optimal_clusters <- kmeans(df,
                             centers = n_clusters,
                             iter.max = 50,
                             algorithm = "Hartigan-Wong")

  cluster_assignments <- data.frame(cluster = optimal_clusters$cluster) %>%
    rownames_to_column("word") %>%
    mutate(n_clusters = n_clusters)
  cluster_assignments
}

get_clusters_for_one_lang <- function(lang, nclusts, words, model_path, ets_lang_names, outpath){

  # get model for clustering
  full_model_path <- paste0(model_path, lang, ".vec")
  model <- fread(
    full_model_path,
    header = FALSE,
    skip = 1,
    quote = "",
    key = "V1",
    encoding = "UTF-8",
    data.table = TRUE)

  target_word_model <- filter(model, V1 %in% words)
  rm(model)

  # tidy and scale data
  d_clean <- target_word_model %>%
    column_to_rownames("V1") %>%
    scale(.)

  # get clusters
  all_clusters <- map_df(nclusts, get_clusters, d_clean) %>%
    mutate(google_lang_name = lang)  %>% # convert to ets lang names
    left_join(ets_lang_names, by = c("google_lang_name")) %>%
    rename(lang = ETS_lang_name) %>%
    select(-google_lang_name)

  full_outpath <- paste0(outpath, "clusters_ets_", all_clusters$lang[1], ".csv")

  write_csv(all_clusters, full_outpath, append = T)

}

walk(TARGET_LANGS,
     get_clusters_for_one_lang,
     N_CLUSTERS,
     target_words,
     MODEL_PATH,
     lang_names,
     OUTPATH)

