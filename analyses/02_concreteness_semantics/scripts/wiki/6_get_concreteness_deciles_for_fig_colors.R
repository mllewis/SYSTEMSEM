library(feather)
library(tidyverse)
library(viridis)

####################### PARAMETERS ######################
BRYSBAERT_PATH <- "../data/brysbaert_corpus.csv"

LABEL_PATH <- "/Users/mollylewis/Downloads/category_labelsA.csv"
OUTPATH <- "../data/category_labels.csv"
N_TILE <- 10

############# SAMPLE WORDS AND SAVE ##########
brysbaert_norms <- read_csv(BRYSBAERT_PATH) %>%
  select(Word, Conc.M) %>%
  rename(word = Word) %>%
  filter(!is.na(word)) 

breaks = quantile(brysbaert_norms$Conc.M, prob = seq(0, 1, length = 11)) %>%
  as.data.frame()%>%
  rename(cutoff = ".") 

conc_labels <- data.frame(conc_break = sort(unique(cut(brysbaert_norms$Conc.M, breaks = breaks$cutoff))),
                          conc_tile = 1:10,
                          color_hex = rev(viridis_pal(option = "plasma")(10))) 

category_colors <- read_csv(LABEL_PATH) %>%
  filter(!is.na(category_label)) %>%
  mutate(conc_break = cut(mean_conc, breaks = breaks$cutoff))  %>%
  right_join(conc_labels)  %>%
  filter(!is.na(mean_conc)) %>%
  rowwise() %>%
  mutate(color_r = col2rgb(color_hex)[1],
         color_g = col2rgb(color_hex)[2],
         color_b = col2rgb(color_hex)[3])
  
write_csv(category_colors, OUTPATH)
  
