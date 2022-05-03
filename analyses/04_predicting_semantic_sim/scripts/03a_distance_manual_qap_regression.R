# saves csv of model parameters for 3 qap regression models 
# (1) predicting language distance including cultural predictor
# (2) predicting language distance *excluding* cultural predictor
# (3) predicting language distance with individual cultural predictors

library(tidyverse)
library(here)

INFILE <- here("analyses/04_predicting_semantic_sim/data/language_all_vars_distance.csv")
PARAM_OUT_FILE <- here("analyses/04_predicting_semantic_sim/data/model_params_manual.csv")
N_SAMPLES <- 1000

get_shuffle_semantic_dist <- function(var_df){
  
  wide_semantics <- var_df %>%
    pivot_wider(names_from = "lang2_ets", 
                values_from = "semantic_dist") 
  
  matrix_semantics <- slice(wide_semantics, 
                            match(names(wide_semantics), 
                                  lang1_ets)) %>%
    select(-lang1_ets) %>%
    as.matrix()
  
  shuffled_indicies <- sample(1:nrow(matrix_semantics), 
                              nrow(matrix_semantics),
                              replace = F)
  
  shuffled_semantics <- matrix_semantics[shuffled_indicies,
                                         shuffled_indicies]
  
  colnames(shuffled_semantics) <- colnames(matrix_semantics)
  rownames(shuffled_semantics) <- colnames(matrix_semantics)
  
  shuffled_semantics_long <- shuffled_semantics %>%
    as.data.frame() %>%
    rownames_to_column("lang1_ets") %>%
    pivot_longer(2:(nrow(matrix_semantics)+1),
                 names_to = "lang2_ets",
                 values_to = "semantic_dist_shuffled") 
  
  shuffled_semantics_long
  
}

get_sampled_params <- function(sample_id, df){
  
  shuffled_semantic_df <- df %>%
    select(lang1_ets, lang2_ets, semantic_dist) %>%
    get_shuffle_semantic_dist()
  
  shuffled_regression_df <- left_join(df, shuffled_semantic_df)
  
  lm(semantic_dist_shuffled ~ cultural_distances +
       eco_dist +
       physical_dist +
       wals_lang_dist +
       asjp_lang_dist,
       data = shuffled_regression_df) %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(sample = sample_id) %>%
    select(sample, everything())
  
}

#### MODEL 1: WITH CULTURE ####
all_dists <- read_csv(INFILE)  

regression_df <-  all_dists %>%
  mutate_if(is.numeric, scale) %>%
  select(lang1_ets, lang2_ets, semantic_dist, cultural_distances, eco_dist,
         physical_dist, wals_lang_dist, asjp_lang_dist) %>%
  as.data.frame()

unshuffled_params <- lm(semantic_dist ~ cultural_distances +
     eco_dist +
     physical_dist +
     wals_lang_dist +
     asjp_lang_dist,
   data = regression_df) %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  rename(unshuffled_estimate = estimate) %>%
  select(term, unshuffled_estimate)

sampled_params <- map_df(1:N_SAMPLES, get_sampled_params, regression_df)

p_values <- right_join(sampled_params, unshuffled_params) %>%
  mutate(as_extreme = estimate >= unshuffled_estimate) %>%
  group_by(term) %>%
  summarize(prop_as_extreme = sum(as_extreme)/n())