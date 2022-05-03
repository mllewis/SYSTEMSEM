# get language pairwise correlation of swadesh distances by item

library(tidyverse)
library(feather)
library(here)



all_dists <- map_df(fs::dir_ls("/Volumes/wilbur_the_great/pairwise_swadesh_words/"), read_feather)
OUTFILE <- here("analyses/03_swadesh/data/language_pairwise_swadesh_correlations_by_item.csv")

GOOGLE_LANGS <- c("ar", "bn", "bg", "zh", "nl",  "fr", "de", "el",  "gu", "hi", "ig", "id", "it", "ja", "kn", "ko",
                  "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo",
                  "fa", "en")

possibly_corr <- possibly(cor, NA)

get_lang_pair_corr_for_each_item <- function(lang1, lang2, df, items){
  df_lang1 <-  filter(df, lang == lang1)
  df_lang2 <-  filter(df, lang == lang2)

  by_item_df <- full_join(df_lang1, df_lang2,
                      by = c("word1", "word2"))  %>%
    filter(word1 != word2) %>%
    group_by(word1) %>%
    nest() %>%
    mutate(cor = map_dbl(data, ~possibly_corr(.$cos_dist.x, .$cos_dist.y, use = "complete"))) %>%
    select(-data) %>%
    mutate(lang1 = lang1,
           lang2 = lang2) %>%
    rename(word = word1) %>%
    select(lang1, lang2, word, cor)

  by_item_df

}

###  DO THE THING ###
lang_pairs <- tibble(google_langs = GOOGLE_LANGS) %>%
  tidyr::expand(google_langs, google_langs) %>%
  rename(lang1 = google_langs,
         lang2 = google_langs1) %>%
  filter(lang1 != lang2)

all_items <- unique(c(as.character(all_dists$word1),
                      as.character(all_dists$word2)))

# loop over language pairs and items
pairwise_corrs_by_item <-
  map2_df(lang_pairs$lang1,
          lang_pairs$lang2,
          get_lang_pair_corr_for_each_item,
          all_dists,
          all_items)


write_csv(pairwise_corrs_by_item, OUTFILE)
