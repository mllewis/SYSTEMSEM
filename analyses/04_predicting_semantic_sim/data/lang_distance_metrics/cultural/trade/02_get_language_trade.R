
library(data.table)
library(tidyverse)
library(tradestatistics)

INFILE <- "data/tradestatistics_data.csv"
OUTFILE <- "data/trade_distances.csv"

trade_raw <- read_csv(INFILE, col_names = c("year","reporter_iso", "partner_iso" ,"reporter_fullname_english",
                                           "partner_fullname_english" ,"export_value_usd", "import_value_usd","export_value_usd_change_1_year",            
                                            "export_value_usd_change_5_years", "export_value_usd_percentage_change_1_year", 
                                            "export_value_usd_percentage_change_5_years", "import_value_usd_change_1_year",            
                                            "import_value_usd_change_5_years", "import_value_usd_percentage_change_1_year", 
                                            "import_value_usd_percentage_change_5_years"))  %>%
  select(reporter_iso, partner_iso, export_value_usd, import_value_usd)  %>%
  rename(country1 = reporter_iso,
         country2 = partner_iso)

trade_data <- trade_raw %>%
  filter(!is.na(import_value_usd)) %>%
  mutate(export_value_usd = as.numeric(export_value_usd)) %>%
  mutate_if(is.character, toupper) %>%
  rowwise()%>%
  mutate(sum_trade = log(sum(c(export_value_usd, import_value_usd))),
         mean_trade = log(mean(c(export_value_usd, import_value_usd)))) %>%
  ungroup() %>%
  select(-export_value_usd, -import_value_usd) 
 
## for each lang, prop speakers in each country
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
  mutate(country = countrycode::countrycode(country_code, "iso2c", "iso3c"))  %>%
  filter(!is.na(country)) %>%
  filter(lang_country_pop > 1000)

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
    summarize(weighted_mean_trade = weighted.mean(mean_trade, w = mean_prop_speakers_of_language_lang, na.rm = T),
              mean_trade = mean(mean_trade, na.rm = T),
              weighted_sum_trade = weighted.mean(sum_trade, w = mean_prop_speakers_of_language_lang, na.rm = T),
              sum_trade = mean(sum_trade, na.rm = T))
  
  data.frame(lang1_ETS = lang1,
             lang2_ETS = lang2,
             weighted_mean_trade = pull(lang_mean, weighted_mean_trade),
             mean_trade = pull(lang_mean, mean_trade),
             weighted_sum_trade = pull(lang_mean, weighted_sum_trade),
             sum_trade = pull(lang_mean, sum_trade))
}
pairwise_trade <- map2_df(as.character(lang_pairs$ETS_lang_name1), 
                          as.character(lang_pairs$ETS_lang_name2), 
                          get_lang_trade, prop_speakers_by_language, trade_data) %>%
  mutate_if(is.numeric, ~-.x) # get distance measure

write_csv(pairwise_trade, OUTFILE)
