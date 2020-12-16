# get bilateral trade
library(data.table)
library(tidyverse)
library(tradestatistics)

OUTFILE <- "data/tradestatistics_data.csv"


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

country_pairs <- cross_df(data.frame(country1 = unique(ld_target$country),
                                    country2 =  unique(ld_target$country))) %>%
  mutate_all(as.character) %>%
  filter(country1 < country2) %>%
  mutate_all(tolower) 

make_data_set <- function(c1, c2, outfile_name){
    out_data <- ots_create_tidy_data(years = 2016, 
                        reporters = c1, 
                        partners = c2, 
                        table = "yrp") 
    write_csv(out_data, outfile_name, append = T)
}

walk2(country_pairs$country1, country_pairs$country2, possibly(make_data_set, NA), OUTFILE)
