
library(data.table)
library(tidyverse)

OUTFILE <- "data/trade_language_distance_all_years.csv"
INFILE <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/L2ETS/studies/study2/analyses/_paper_analyses/lang_dists/lang_distance_metrics/cultural/trade/data/20180521-ESCAP-WB-tradecosts-dataset.csv"

## get trade data
# https://artnet.unescap.org/databases
trade_data <- read_csv(INFILE) %>%
  select(reporter, partner, year, sectorname, tij) %>%
  rename(country1 = reporter,
         country2 = partner)

# The data is symmetric
country_pair_means <- trade_data %>%
  filter(sectorname == "Total Goods (GTT)",) %>%
  group_by(country1, country2) %>%
  summarize(mean_tij = mean(tij)) 

## get lang -> country mappings
LD_INFILE <- "../../ecological/data/lupyan_2010.txt"
# read in ld data (note that each row here is a country)
ld <- read.table(LD_INFILE, fill = T, 
                 header = T, sep = "\t", na.strings = "*") %>%
  janitor::clean_names()

CODE_INFILE <- "../../ecological/data/iso_to_wals_for_lupyan_dale.csv"
lang_names <- read_csv(CODE_INFILE)

ld_target <- ld %>%
  filter(wals_code %in% lang_names$wals_code)  %>%
  select(country_code, country, wals_code, lang_country_pop) %>%
  left_join(lang_names %>% select(ETS_lang_name, wals_code))   %>%
  distinct() %>%
  mutate(country = countrycode::countrycode(country_code, "iso2c", "iso3c")) 

## for each lang, prop speakers in each country
prop_speakers_by_language <- ld_target %>%
  group_by(ETS_lang_name) %>%
  mutate(prop_speakers_of_language_lang = lang_country_pop/sum(lang_country_pop))  %>%
  ungroup() %>%
  select(ETS_lang_name, country, prop_speakers_of_language_lang)

# get unique language pairs to map over
lang_pairs <- expand.grid(data.frame(
  ETS_lang_name1 = unique(ld_target$ETS_lang_name),
  ETS_lang_name2 = unique(ld_target$ETS_lang_name)), stringsAsFactors = FALSE) %>%
  filter(as.character(ETS_lang_name1) > as.character(ETS_lang_name2)) 

get_lang_trade <- function(lang1, lang2, speaker_df, trade_df){
  target_countries <- speaker_df %>%
    filter(ETS_lang_name %in% c(lang1, lang2))
  
  # relevant country pairs (country1 %in% lang1, and country2 %in% lang2)
  target_country_country_trade <- trade_df %>%
    filter(country1 %in% (target_countries %>% filter(ETS_lang_name == lang1) %>% pull(country)),
           country2 %in% (target_countries %>% filter(ETS_lang_name == lang2) %>% pull(country)))
  
  print(c(lang1, lang2))
  
  lang_mean <- target_country_country_trade %>%
    left_join(target_countries %>% filter(ETS_lang_name == lang1) %>% select(-ETS_lang_name),
              by = c("country1" = "country")) %>% # merge in prop speakers for each country
    rename(prop_speakers_of_language_lang1 = prop_speakers_of_language_lang) %>%
    left_join(target_countries %>% filter(ETS_lang_name == lang2) %>% select(-ETS_lang_name), 
              by = c("country2" = "country")) %>%
    rename(prop_speakers_of_language_lang2 = prop_speakers_of_language_lang) %>%
    rowwise() %>%
    mutate(mean_prop_speakers_of_language_lang =  # get the mean prop of each lang in each country across langs
             mean(c(prop_speakers_of_language_lang1, prop_speakers_of_language_lang2))) %>%
    select(-prop_speakers_of_language_lang1, -prop_speakers_of_language_lang2) %>%
    ungroup() %>%
    summarize(mean_tij_across_countries = weighted.mean(mean_tij, w = mean_prop_speakers_of_language_lang),
              mean_tij_across_countries_unweighted = mean(mean_tij))
  
  data.frame(lang1_ETS = lang1,
             lang2_ETS = lang2,
             mean_tij_across_countries = pull(lang_mean, mean_tij_across_countries),
             mean_tij_across_countries_unweighted = pull(lang_mean, mean_tij_across_countries_unweighted))
}
pairwise_trade <- map2_df(as.character(lang_pairs$ETS_lang_name1), 
                          as.character(lang_pairs$ETS_lang_name2), 
       get_lang_trade, prop_speakers_by_language, country_pair_means)

write_csv(pairwise_trade, OUTFILE)
