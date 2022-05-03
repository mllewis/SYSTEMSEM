# saves csv of model parameters for 3 qap regression models
# (1) predicting language distance including cultural predictor
# (2) predicting language distance *excluding* cultural predictor
# (3) predicting language distance with individual cultural predictors

library(tidyverse)
library(here)
library(sna)

INFILE <- here("analyses/04_predicting_semantic_sim/data/language_all_vars_distance.csv")
PARAM_OUT_FILE <- here("analyses/04_predicting_semantic_sim/data/model_params.csv")

# netlm ci int
get_netlm_cis <- function(raw_predictors_mat, model_summary){
  raw_predictors <- raw_predictors_mat %>%
    as.vector()  %>%
    .[!is.na(.)]

  mse <- mean(model_summary$residuals^2)
  mean_predictor <- mean(raw_predictors)
  crit_t <- qt(0.975, length(raw_predictors) - 2) # critical value of t

  ci_int <- tibble(diff =  raw_predictors - mean_predictor) %>%
    mutate(diff2 = diff^2) %>%
    summarize(denom = sum(diff2)) %>%
    mutate(ci_int = crit_t*sqrt(mse/denom)) %>%
    pull(ci_int)

  ci_int
}

get_regression_mat <- function(this_df){
  mat <- this_df %>%
    spread(lang1_ets, value) %>%
    select(-lang2_ets) %>%
    as.matrix()

  mat[lower.tri(mat, diag = TRUE)] <- NA
  mat
}

make_iv_mat <- function(mat_df, predictors){

  target_mats <- mat_df %>%
    filter(measure %in% predictors)

  mat_size <- dim(target_mats$mat[[1]])[1]

  out_array <- array(0, dim = c(nrow(target_mats), mat_size, mat_size))
  for (i in 1:nrow(target_mats)){
    out_array[i,,] <- filter(mat_df, measure == predictors[i])$mat[[1]]
  }
  out_array
}

get_model_parameters <- function(ivs, mats){
  model_type = ifelse(length(ivs) > 1, "full",  "single")

  iv_mat <- make_iv_mat(mats, ivs)
  dv_mat <- filter(mats, measure == "semantic_dist")$mat[[1]]

  # run the model
  qap_model <- netlm(dv_mat,
                     iv_mat,
                     nullhyp = "qap",
                     reps = 1000)
  # tidy the model
  model_output <- tibble(term = c("intercept", ivs),
                    qap_p = summary(qap_model)$pgreqabs,
                    estimate = summary(qap_model)$coefficients,
                    tstat = summary(qap_model)$tstat,
                    n =  summary(qap_model)$n) %>%
    left_join(mats, by = c("term" = "measure")) %>%
    filter(term != "intercept") %>%
    mutate(ci = map_dbl(mat , get_netlm_cis, summary(qap_model)),
           ci_lower = estimate - ci,
           ci_upper = estimate + ci) %>%
    select(-ci, -mat) %>%
    mutate(model = model_type)

  model_output
}

#### MODEL 1: WITH CULTURE ####
all_dists <- read_csv(INFILE)

# prepare the data
all_mats_wc <-  all_dists %>%
  filter(!is.na(cultural_distances)) %>%
  mutate_if(is.numeric, scale) %>% # scale so betas are standardized
  gather("measure", "value", -1:-2) %>%
  group_by(measure) %>%
  nest() %>%
  mutate(mat = map(data, get_regression_mat)) %>%
  select(-data)

# get the parameters
model_params_with_culture <- list(c("cultural_distances", "eco_dist", "physical_dist", "wals_lang_dist", "asjp_lang_dist"),
     "cultural_distances",
     "eco_dist",
     "physical_dist",
     "wals_lang_dist",
     "asjp_lang_dist") %>%
  map_df(get_model_parameters, all_mats_wc)  %>%
  mutate(term = fct_recode(term, "Grammatical Distance" = "wals_lang_dist",
                           "Physical Distance" = "physical_dist",
                           "Climate Distance" = "eco_dist",
                           "Lexical Distance" = "asjp_lang_dist",
                           "Cultural Distance"  = "cultural_distances"))  %>%
  mutate(model_type = "with culture")

#### MODEL 2: WITHOUT CULTURE ####
# prepare the data
all_mats_woc <-  all_dists %>%
  mutate_if(is.numeric, scale) %>%
  gather("measure", "value", -1:-2) %>%
  group_by(measure) %>%
  nest() %>%
  mutate(mat = map(data, get_regression_mat)) %>%
  select(-data)

# get the parameters
model_params_without_culture <- list(c("eco_dist", "physical_dist", "wals_lang_dist", "asjp_lang_dist"),
                     "eco_dist",
                     "physical_dist",
                     "wals_lang_dist",
                     "asjp_lang_dist") %>%
  map_df(get_model_parameters, all_mats_woc)  %>%
  mutate(term = fct_recode(term, "Grammatical Distance" = "wals_lang_dist",
                           "Physical Distance" = "physical_dist",
                           "Climate Distance" = "eco_dist",
                           "Lexical Distance" = "asjp_lang_dist",
                           "Cultural Distance"  = "cultural_distances")) %>%
  mutate(model_type = "without culture")


#### MODEL 3: REGRESSION WITH INDIVIDUAL CULTURAL PREDICTORS ####
# prepare the data
all_mats_cp <-  all_dists %>%
  filter(!is.na(cultural_distances)) %>%
  mutate_if(is.numeric, scale) %>%
  gather("measure", "value", -1:-2) %>%
  group_by(measure) %>%
  nest() %>%
  mutate(mat = map(data, get_regression_mat)) %>%
  select(-data)

# get the parameters
model_params_individual_cultural_predictors <- list("agriculture_and_vegetation",
                     "basic_actions_and_technology",
                     "emotions_and_values",
                     "kinship",
                     "law",
                     "possession",
                     "religion_and_belief",
                     "social_and_political_relations",
                     "the_house",
                     "the_physical_world") %>%
  map_df(get_model_parameters, all_mats_cp)  %>%
  mutate(term = fct_recode(term,
                           "Agriculture/Vegetation" = "agriculture_and_vegetation",
                           "Basic Actions/Technology" = "basic_actions_and_technology",
                           "Kinship" = "kinship",
                           "Law" = "law",
                           "Possession"  = "possession",
                           "Religion/Belief" = "religion_and_belief",
                           "Social/Political Relations" = "social_and_political_relations",
                           "The House" = "the_house",
                           "The Physical World" = "the_physical_world",
                           "Emotions/Values" = "emotions_and_values"),
         term = fct_relevel(term,  "Kinship",   "Possession", "Basic Actions/Technology",
                            "Religion/Belief",  "Law", "Social/Political Relations", "Emotions/Values",
                            "The House",  "The Physical World", "Agriculture/Vegetation")) %>%
  mutate(model_type = "individual cultural predictors")

all_params <- bind_rows(model_params_with_culture, model_params_without_culture) %>%
  bind_rows(model_params_individual_cultural_predictors)

write_csv(all_params, PARAM_OUT_FILE)
