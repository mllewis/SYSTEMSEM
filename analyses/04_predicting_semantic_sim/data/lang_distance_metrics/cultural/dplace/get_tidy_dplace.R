# get tidy dplace
library(tidyverse)

OUTFILE <- "data/dplace_distances.csv"
INFILE_ABBREV <- "data/ETS_abbrev_to_names.csv"

DOMAINS <- c("CulturalDistances","Agriculture_and_vegetation", "Basic_actions_and_technology",
             "Emotions_and_values", "Kinship", "Law", "Possession", "Religion_and_belief", 
             "Social_and_political_relations", "The_house", "The_physical_world")

# There are a 10 different cultural domain distances measures, and one overall measure: 
# The names are a little misleading, however, because they are mapped onto concepticon
# categories. We have cultural data for 28/35 langauges. The ones missing are: "GER", "GRE", "ITA", "POR", "RUM", "FAS", "YOR".
# For several there were not exact matches between the two datasets, and but I mapped them to very similar groups. 

abbrevs <- read_csv(INFILE_ABBREV)  

get_domain_data <- function(domain){
  path <- paste0("data/", domain, "_long.csv")
  cult_dists_raw <- read_csv(path) %>%
    mutate(measure = domain)
  
  all_cult_dists <- cult_dists_raw  %>%
    left_join(abbrevs %>% 
                select(ETS_code, EA_language_Name2), 
              by = c("Var1" = "EA_language_Name2")) %>%
    rename(lang1_ETS = ETS_code) %>%
    left_join(abbrevs %>% 
                select(ETS_code, EA_language_Name2,), 
              by = c("Var2" = "EA_language_Name2")) %>%
    rename(lang2_ETS = ETS_code,
           cultural_dist = value) %>%
    select(lang1_ETS, lang2_ETS, cultural_dist, measure) %>%
    filter(!is.na(lang1_ETS) & !is.na(lang2_ETS)) 
  
  all_cult_dists

}

all_cultural_values <- map_df(DOMAINS, get_domain_data)
all_cultural_values_wide <- all_cultural_values %>%
  distinct(lang1_ETS, lang2_ETS, measure, .keep_all = T) %>%
  spread(measure, cultural_dist)  %>%
  mutate_if(is.character, tolower)  

write_csv(all_cultural_values_wide, OUTFILE)

