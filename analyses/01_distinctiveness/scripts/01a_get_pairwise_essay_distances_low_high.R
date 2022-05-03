# get mean distance for same vs. different language for each score group (all model)

library(tidyverse)
library(here)
library(feather)

NSAMPS_PER_LANG <- 100
NSAMPS <- 100

OUTFILE <- paste0("data/sampled_essay_distances_high_low.csv")

INDICES_ALL <- here("data/processed/models/all_model/doctag_indices_all_model.feather")
MODEL_ALL <- here("data/processed/models/all_model/docvecs_all_model.feather")
METDATA_ALL <- here("data/raw/models/all_model/merged_metadata.csv")

doctag_indices <- read_feather(INDICES_ALL) %>%  # essay_id, offset, word count
  select(essay_id, offset)

docvecs <- read_feather(MODEL_ALL) %>%
  as.data.frame() %>%
  mutate(offset = 0:(n()-1)) %>%
  select(offset, everything())

metadata_clean <- read_csv(METDATA_ALL) %>%
  select(essay_id, L1_code,  score) %>%
  mutate_if(is.character, as.factor)  %>%
  mutate(essay_id = as.character(essay_id))

doc_df_data <- doctag_indices %>%
  full_join(docvecs, by = "offset")  %>%
  full_join(metadata_clean, by = "essay_id") %>%
  select(-offset) %>%
  mutate(score_bin = case_when(score >=  4 ~ "high",
                               score < 4 ~ "low")) %>%
  select(essay_id, L1_code, score, score_bin, everything()) 
  

get_distance_sample <- function(i, current_df, nsamps, outfile){
  print(i)
  
  # get nsamps essays for each language
  sampled_df <- current_df %>%
    group_by(L1_code, score_bin) %>%
    sample_n(nsamps)
  
  doc_matrix <- t(as.matrix(sampled_df[,-1:-4]))
  
  essay_essay_dists <- coop::cosine(doc_matrix)
  
  wide_essay_essay_dists <- essay_essay_dists %>%
    as.data.frame(row.names = sampled_df$essay_id) %>%
    setNames(., sampled_df$essay_id)  %>%
    rownames_to_column(var = "essay1")
  
  long_essay_essay_dists  <- gather(wide_essay_essay_dists, 
                                    "essay2", "cos_dist", -essay1)
  
  tidy_dists <- long_essay_essay_dists %>%
    left_join(sampled_df %>% select(essay_id, L1_code, score_bin) %>%
                rename(essay1 = essay_id, L1_code1 = L1_code, score_bin1 = score_bin)) %>%
    left_join(sampled_df %>% select(essay_id, L1_code, score_bin) %>%
                rename(essay2 = essay_id, L1_code2 = L1_code, score_bin2 = score_bin)) 
  
  pairwise_means <- tidy_dists %>%
    filter(score_bin1 == score_bin2) %>% # we only care about within score bin comparision
    filter(essay1 != essay2) %>% # exclude distances to self
    mutate(distance_type = case_when(L1_code1 == L1_code2 ~ "same_language",
                                     L1_code1 != L1_code2  ~ "diff_language")) %>%
    group_by(L1_code1, score_bin1, distance_type) %>%
    summarize(mean_dist = mean(cos_dist),
              n = n()) %>%
    mutate(sample_id = i)
  
  write_csv(pairwise_means, outfile, append = T)
}


map(1:NSAMPS, get_distance_sample, doc_df_data, NSAMPS_PER_LANG, OUTFILE)


