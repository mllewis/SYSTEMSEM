# pairwise language distance correlations (with culture; n = 28)

library(tidyverse)
library(broom)
library(sna)

INFILE <- "../data/language_all_vars_distance.csv"
OUTFILE <- "../data/corr_data_df_with_culture.csv"
all_dists <- read_csv(INFILE)

get_regression_mat <- function(this_df){
  mat <- this_df %>%
    spread(lang1_ets, value) %>%
    select(-lang2_ets) %>%
    as.matrix()

  mat[lower.tri(mat, diag = TRUE)] <- NA
  mat
}

get_qap_cor <- function(m1, m2, mats){
  dv_mat <- filter(mats, measure == m1) %>% pull(mat) %>% pluck(1)
  iv_mat <- filter(mats, measure == m2) %>% pull(mat) %>% pluck(1)

  # qap cor
  out_array <- array(0, dim = c(2, nrow(dv_mat), ncol(dv_mat)))
  out_array[1,,] <- dv_mat
  out_array[2,,] <-  iv_mat

  qap_cor_model <- summary(qaptest(out_array, gcor, g1 = 1, g2 = 2))

  cor_model_output <- tibble(qap_cor_p1 = qap_cor_model$pgreq,
                             qap_cor_p2 = qap_cor_model$pleeq)

  # qap regression
  qap_reg_model <- netlm(iv_mat,
                     dv_mat,
                     nullhyp = "qap",
                     reps = 1000)
  reg_model_output <- tibble(qap_reg_p = summary(qap_reg_model)$pgreqabs[2],
                         estimate_reg = summary(qap_reg_model)$coefficients[2],
                         tstat_reg = summary(qap_reg_model)$tstat[2],
                         n =  summary(qap_reg_model)$n)

  tibble(measure1 = m1, measure2 = m2) %>%
    bind_cols(reg_model_output, cor_model_output)

}

get_simple_correlation <- function(m1, m2, df){
  cor_model <- cor.test(df[,m1] %>% unlist(use.names = F),
           df[,m2] %>% unlist(use.names = F))

  tibble(measure1 = m1,
         measure2 = m2,
         simple_cor = cor_model$estimate,
         simple_p =  cor_model$p.value)
}


# prepare the data

reg_df <- all_dists %>%
  filter(!is.na(cultural_distances)) %>%
  mutate_if(is.numeric, scale) %>%
  select(1:8)

# get qap ps

all_mats <- reg_df %>%
  gather("measure", "value", -1:-2) %>%
  group_by(measure) %>%
  nest() %>%
  mutate(mat = map(data, get_regression_mat)) %>%
  select(-data)

measures <- cross_df(data.frame(measure1 = all_mats$measure,
                                measure2 = all_mats$measure)) %>%
  mutate_all(as.character)

qap_ps <- map2_df(measures$measure1, measures$measure2,
                  get_qap_cor, all_mats)

# get cors
simple_corrs <- map2_df(measures$measure1, measures$measure2,
                  get_simple_correlation, reg_df)


all_corrs <- full_join(simple_corrs, qap_ps)
write_csv(all_corrs, OUTFILE)

all_corrs_table <- read_csv(OUTFILE)

# get latex table
table <- all_corrs_table %>%
  select(measure1, measure2, simple_cor) %>%
  spread(measure2, simple_cor) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(measure1 = fct_recode(measure1, "Grammatical Distance" = "wals_lang_dist",
                           "Physical Distance" = "physical_dist",
                           "Climate Distance" = "eco_dist",
                           "Lexical Distance" = "asjp_lang_dist",
                           "Cultural Distance"  = "cultural_distances",
                           "Semantic Distance"  = "semantic_dist"),
         measure1 = fct_relevel(measure1, "Semantic Distance", "Grammatical Distance",  "Physical Distance",
                                "Climate Distance",  "Cultural Distance","Lexical Distance")) %>%
    arrange(measure1) %>%
    rename( "Grammatical Distance" = "wals_lang_dist",
            "Physical Distance" = "physical_dist",
            "Climate Distance" = "eco_dist",
            "Lexical Distance" = "asjp_lang_dist",
            "Cultural Distance"  = "cultural_distances",
            "Semantic Distance"  = "semantic_dist") %>%
    select(measure1, "Semantic Distance", "Grammatical Distance",  "Physical Distance",
                                "Climate Distance",  "Cultural Distance","Lexical Distance")

xtable::xtable(table, type = "latex") %>%
  print(rotate.colnames = TRUE)
