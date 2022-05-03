# get wikipedia stats

library(tidyverse)
library(janitor)
library(here)
library(glue)

PAGEVIEWS_PATH <- here("analyses/10_wikipedia_stats/data/processed/page_views_by_language.csv")
PRODUCITIVY_PATH <- here("analyses/10_wikipedia_stats/data/raw/wiki_productivity.csv")
NARTICLES_PATH <- here("analyses/10_wikipedia_stats/data/raw/num_articles.tsv")
LANG_NAMES <- here('analyses/04_predicting_semantic_sim/data/lang_distance_metrics/linguistic/data/iso_to_wals_for_ling_dists.csv')
OUTFILE <- here("analyses/10_wikipedia_stats/data/wikipedia_stats_cor_table.csv")

WIKI_LANGS <- c("ar", "bn", "bg", "zh", "nl", "en", "fr", "de", "el", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko",
                "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo",
                "fa")

narticles <- read_tsv(NARTICLES_PATH) %>%
  clean_names() %>%
  select(language, wiki, articles, speakers, admins, active_users,
         percent_of_admins, depth, speakers_article) %>%
  mutate(speakers = as.numeric(str_remove_all(speakers, ",")))

productivity <- read_csv(PRODUCITIVY_PATH) %>%
  select(2,4) %>%
  mutate(potential_authors = potential_authors * 1000000)

page_views <- read_csv(PAGEVIEWS_PATH)

all_measures <- narticles %>%
  full_join(productivity, by = "wiki") %>%
  full_join(page_views, by = "wiki") %>%
  filter(!is.na(iso)) %>%
  filter(wiki %in% WIKI_LANGS) %>%
  select(-iso, -speakers_article, -percent_of_admins) %>%
  mutate_at(c("articles", "speakers", "speakers_l1", "total_requests", "potential_authors"), log) %>%
  mutate(prop_articles = articles/speakers_l1,
         prop_requests = total_requests/speakers_l1,
         productivity = articles/potential_authors )

get_simple_correlation <- function(m1, m2, df){
  cor_model <- cor.test(df[,m1] %>% unlist(use.names = F),
                        df[,m2] %>% unlist(use.names = F))

  tibble(measure1 = m1,
         measure2 = m2,
         simple_cor = cor_model$estimate,
         simple_p =  cor_model$p.value,
         ci_lower = cor_model$conf.int[1],
         ci_upper = cor_model$conf.int[2])

}

measures <- cross_df(data.frame(measure1 = names(all_measures)[3:13],
                                measure2 = names(all_measures)[3:13])) %>%
  mutate_all(as.character) %>%
  filter(measure1 != measure2)

# get cors
all_corrs_table <- map2_df(measures$measure1, measures$measure2,
                           get_simple_correlation, all_measures) %>%
  mutate(sig = simple_p < .05,
         simple_p = format(round(simple_p, 2), nsmall = 2),
         simple_p = case_when(simple_p == "0.00" ~ "<.01", TRUE ~ as.character(simple_p)),
         ci_lower = format(round(ci_lower, 2), nsmall = 2),
         ci_upper = format(round(ci_upper, 2), nsmall = 2),
         simple_cor = format(round(simple_cor, 2), nsmall = 2),
         sig = case_when(sig ~ "*",
                              TRUE ~ " "),
         cor_text = glue("${simple_cor}$ $({simple_p})$"))

         #cor_text = glue("{simple_cor} [{ci_lower},{ci_upper}]{sig}"))


table <- all_corrs_table %>%
  select(measure1, measure2, cor_text) %>%
  spread(measure2, cor_text) %>%
  mutate(
    measure1 = fct_relevel(measure1, "speakers", "speakers_l1",  "articles",
                           "active_users", "total_requests",  "admins", "depth", "prop_articles",  "productivity", "prop_requests", "potential_authors")) %>%
  select("measure1", "speakers", "speakers_l1",  "articles",
         "active_users", "total_requests",  "admins", "depth", "prop_articles",  "productivity", "prop_requests", "potential_authors") %>%
  arrange(measure1)


TARGET_VARIABLES <- c("speakers", "speakers_l1", "potential_authors", "articles", "total_requests", "prop_articles",  "prop_requests", "productivity")
final_table <- table %>%
  select(measure1, TARGET_VARIABLES) %>%
  filter(measure1 %in% TARGET_VARIABLES) %>%
  mutate(measure1 = fct_recode(measure1,
                               "N speakers" = "speakers",
                               "N L1 speakers" = "speakers_l1",
                               "N potential authors" = "potential_authors",
                               "N articles" = "articles",
                               "N pageviews" = "total_requests",
                               "Consumption rate" = "prop_requests",
                               "Productivity (1)" = "prop_articles",
                               "Productivity (2)"  = "productivity"),
          measure1 = fct_relevel(measure1,  "N speakers" , "N L1 speakers","N potential authors", "N articles",
                                "N pageviews", "Consumption rate", "Productivity (1)",
                                "Productivity (2)")) %>%
  arrange(measure1) %>%
  rename("N speakers" = "speakers",
         "N L1 speakers" = "speakers_l1",
         "N potential authors" = "potential_authors",
         "N articles" = "articles",
         "N pageviews" = "total_requests",
         "Productivity (1)" = "prop_articles",
         "Consumption rate" = "prop_requests",
         "Productivity (2)"  = "productivity") %>%
  select("measure1", "N speakers" , "N L1 speakers", "N potential authors", "N articles",
         "N pageviews", "Consumption rate", "Productivity (1)",
         "Productivity (2)")

write_csv(final_table, OUTFILE)

## get latex table

final_table <- read_csv(OUTFILE)

xtable::xtable(final_table, type = "latex")
