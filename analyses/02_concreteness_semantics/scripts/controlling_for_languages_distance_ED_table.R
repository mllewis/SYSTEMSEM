# predict local global distances, controling for language distance

library(tidyverse)
library(broom)
library(here)
library(glue)

ETS_CLUSTER_CORR_PATH <- here("analyses/02_concreteness_semantics/data/ets/lang_pairwise_semantics_correlations_ets_by_cluster.csv")
WIKI_CLUSTER_CORR_PATH <- here("analyses/02_concreteness_semantics/data/wiki/lang_pairwise_semantics_correlations_wiki_by_cluster.csv")
LANG_NAMES1 <- here("analyses/04_predicting_semantic_sim/data/lang_distance_metrics/linguistic/data/iso_to_wals_for_ling_dists.csv")
LANG_NAMES2 <- here("analyses/02_concreteness_semantics/data/lang_name_to_wiki_iso.csv")
LANGUAGE_DISTANCES <- here("analyses/04_predicting_semantic_sim/data/lang_distance_metrics/linguistic/data/wals_language_distance")
OUTFILE <- here("analyses/02_concreteness_semantics/data/lang_distiances_control_models.csv")

# get cluster distances
cluster_ets <-  read_csv(ETS_CLUSTER_CORR_PATH,
                         col_names = c("cluster1", "cluster2", "cor", "lang1", "lang2")) %>%
  mutate(corpus = "TOEFL")

cluster_wiki <- read_csv(WIKI_CLUSTER_CORR_PATH,
                         col_names = c("cluster1", "cluster2", "cor", "lang1", "lang2")) %>%
  distinct() %>%
  mutate(corpus = "Wikipedia")

cluster_corr <- cluster_ets %>%
  bind_rows(cluster_wiki)  %>%
  mutate(local_global = case_when(cluster1 == cluster2 ~ "local", TRUE ~ "global"),
         lang_pair = glue("{lang1}_{lang2}")) %>%
  group_by(corpus, lang_pair, local_global) %>%
  summarize(mean_corr = mean(cor, na.rm = T)) %>% # aggregate across cluster pairs
  ungroup()  %>%
  as.data.frame()

cluster_dif_wide_wiki <- cluster_corr %>%
  filter(corpus == "Wikipedia") %>%
  spread(local_global, mean_corr) %>%
  mutate(dif = local- global)

cluster_dif_wide_ets <- cluster_corr %>%
  filter(corpus == "TOEFL") %>%
  spread(local_global, mean_corr) %>%
  mutate(dif = local- global)

# get language distances
lang_names1 <- read_csv(LANG_NAMES1)
lang_names2 <- read_csv(LANG_NAMES2)

all_names <- lang_names1 %>%
  left_join(lang_names2, by = c("lang_name2" = "language_name")) %>%
  mutate(wiki_language_code=replace(wiki_language_code, ETS_lang_name=="ara", "ar"),
         wiki_language_code=replace(wiki_language_code, ETS_lang_name=="chi", "zh"),
         wiki_language_code=replace(wiki_language_code, ETS_lang_name=="guj", "gu"),
         wiki_language_code=replace(wiki_language_code, ETS_lang_name=="kan", "kn"),
         wiki_language_code=replace(wiki_language_code, ETS_lang_name=="mal", "ml"),
         wiki_language_code=replace(wiki_language_code, ETS_lang_name=="mar", "mr"),
         wiki_language_code=replace(wiki_language_code, ETS_lang_name=="pan", "pa"),
         wiki_language_code=replace(wiki_language_code, ETS_lang_name=="tel", "te"),
         wiki_language_code=replace(wiki_language_code, ETS_lang_name=="yor", "yo"),
         wiki_language_code=replace(wiki_language_code, ETS_lang_name=="fas", "fa"),
         wiki_language_code=replace(wiki_language_code, ETS_lang_name=="ibo", "ig")) %>%
  select(ETS_lang_name, wiki_language_code)

lang_dists <- read_csv(LANGUAGE_DISTANCES)

lang_dists_tidy <- lang_dists %>%
  left_join(all_names, by = c("lang1_ETS" = "ETS_lang_name")) %>%
  rename("wiki_language_code1" = "wiki_language_code") %>%
  left_join(all_names, by = c("lang2_ETS" = "ETS_lang_name")) %>%
  rename("wiki_language_code2" = "wiki_language_code") %>%
  mutate(lang_pair = paste0(wiki_language_code1, "_", wiki_language_code2)) %>%
  select(lang_pair, wals_lang_dist)

# language distances wiki
diff_with_language_distance <- cluster_dif_wide_wiki %>%
  left_join(lang_dists_tidy)

cor.test(diff_with_language_distance$dif,
         diff_with_language_distance$wals_lang_dist)

wiki_model <- lm(dif ~ 1 + wals_lang_dist, data = diff_with_language_distance) %>%
  summary() %>%
  tidy() %>%
  mutate(corpus = "Wikipedia")


# language distances ets
lang_dists_tidy_ets <- lang_dists %>%
  mutate(lang_pair = paste0(toupper(lang1_ETS), "_", toupper(lang2_ETS))) %>%
  select(lang_pair, wals_lang_dist)

diff_with_language_distance_ets <- cluster_dif_wide_ets %>%
  left_join(lang_dists_tidy_ets)

cor.test(diff_with_language_distance_ets$dif,
         diff_with_language_distance_ets$wals_lang_dist)

ets_model <-lm(dif ~ 1 + wals_lang_dist, data = diff_with_language_distance_ets) %>%
  summary() %>%
  tidy() %>%
  mutate(corpus = "TOEFL")

model_table <- bind_rows(wiki_model, ets_model) %>%
  select(corpus, everything())
write_csv(model_table, OUTFILE)

## get latex table

model_table <- read_csv(OUTFILE)

model_table_tidy <- model_table %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate_if(is.numeric, ~ifelse(.x == 0, "<.0001", as.character(.x))) %>%
  rename("SE" = std.error,
         "t-statistic" = "statistic",
         "p-value" = "p.value")


xtable::xtable(model_table_tidy, type = "latex")
