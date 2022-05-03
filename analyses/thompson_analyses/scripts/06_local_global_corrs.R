# get pairwise correlations between words

library(tidyverse)
library(here)
library(langcog)

####################### PARAMETERS ######################

OUTFILE <- here("analyses/thompson_analyses/data/corrs_local_global.csv")
CORRS_BY_DOMAIN <-  here("analyses/thompson_analyses/data/lang_pairwise_domain_correlations.csv")
THOMPSON_ALIGN_BY_WORD <- here("analyses/thompson_analyses/data/thompson_words_with_alignments.csv")

corrs_by_domain <- read_csv(CORRS_BY_DOMAIN,
                          col_names = c("local_global", "domain", "corr", "n", "l1", "l2"),
                          col_types = "ccddcc")


corrs_by_unique_domain_pairs <- corrs_by_domain %>%
  mutate(domain1 = map_chr(domain, ~str_split(., "_")[[1]][[1]]),
         domain2 = map_chr(domain, ~str_split(., "_")[[1]][[2]])) %>%
  select(local_global, corr,  domain1, domain2, l1, l2)

complement_global_pairs <- corrs_by_unique_domain_pairs %>%
  filter(local_global == "global") %>%
  mutate(temp1 = domain1,
         temp2 = domain2) %>%
  select(-domain1, -domain2) %>%
  rename(domain1 = temp2,
         domain2 = temp1) %>%
  select(local_global, corr,  domain1, domain2, l1, l2)

all_domain_pairs <- corrs_by_unique_domain_pairs %>%
  bind_rows(complement_global_pairs)

mean_corrs_by_domain_lang_pair <- all_domain_pairs %>%
  group_by(l1, l2, local_global) %>%
  multi_boot_standard(col = "corr", na.rm = T) %>%
  rename(corr = mean)

write_csv(mean_corrs_by_domain_lang_pair, OUTFILE)

