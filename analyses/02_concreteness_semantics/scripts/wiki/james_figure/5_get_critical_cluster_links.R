# Get critical cluster links

library(tidyverse)
library(data.table)

####################### PARAMETERS ######################
INPATH <- "../../data/v3/target_cluster_langs2.csv" 
CENTROID_IN <- "../../data/v3/pairwise_semantic_centroid_distances.csv"
WORDINPATH <- "../../data/v3/target_words2.csv" 
CONC_PATH <- "/Users/mollylewis/Documents/research/Projects/2_published/ref_complex/corpus/brysbaert_database/brysbaert_corpus.csv"
OUTFILE <- "../data/plotting_data.csv"
OUTFILE_CONC <- "../../data/category_mean_concretenss.csv"

####################### READ IN DATA ######################
# get cluster nums for target words by langauge
targets <- read_csv(INPATH) %>%
  rename(clust_num1 = cluster,
         category_label1 = category_label) %>%
  mutate(clust_num2 = clust_num1,
         category_label2 = category_label1) 

conc_data <- read_csv(CONC_PATH) %>%
  janitor::clean_names() %>%
  select(word, conc_m)

# get_centroids
centroids_dist <- read_csv(CENTROID_IN) %>%
  rename(clust_num1 = cluster1, 
         clust_num2 = cluster2) 

BADCATS <-  c("commerce", "size")

############### CONCRETENES BY CLUSTER #########

category_concreteness <- read_csv(WORDINPATH) %>%
  group_by(category_label) %>%
  left_join(conc_data) %>%
  summarise(category_label_long = paste(word, collapse = "-"),
            mean_conc = mean(conc_m))  %>%
  arrange(-mean_conc)
  
#write_csv(category_concreteness, OUTFILE_CONC)


####################### CLOSEST QUARTILE PLOT ######################

targs_with_links <- centroids_dist %>%
  select(-cos_dist, -decile) %>%
  right_join(targets %>% select(-clust_num2, -category_label2)) %>%
  rename(quartile_from_clust_num1 = quartile) %>%
  right_join(targets %>% select(lang, clust_num2, category_label2)) %>%
  filter(clust_num1 != clust_num2) %>%
  mutate(close = ifelse(quartile_from_clust_num1 == 4, TRUE, FALSE)) %>%
  mutate(lang_formated = ifelse(close, toupper(lang), lang)) %>%
  select(close, category_label1, category_label2, lang_formated) %>% 
  filter(!(category_label1 %in%  BADCATS)) %>%
  filter(!(category_label2 %in% BADCATS)) 
  #filter(category_label1 %in% c("body", "army"), category_label2 %in% c("body", "army"))

counts <- targs_with_links %>%
  group_by(category_label1, category_label2)  %>%
  count(close) %>%
  spread(close, n) %>%
  rename(close_langs = `TRUE`,
         far_langs = `FALSE`) %>%
  mutate_at(vars(close_langs, far_langs), ~ replace_na(., 0)) %>%
  mutate(prop_close_langs = close_langs/(far_langs + close_langs))

df_for_plotting <- targs_with_links %>%
  group_by(category_label1, category_label2) %>%
  mutate(n = 1:n()) %>%
  mutate(lang_formated = ifelse(n %% 6 == 0, paste0(lang_formated, "\n"), lang_formated)) %>%
  summarise(lang_formated = paste(lang_formated,collapse=" ")) %>%
  ungroup()  %>%
  mutate(line_positions = as.numeric(factor(category_label1, levels = unique(category_label1))), 
         line_positions = line_positions + .5,  
         line_positions = ifelse(line_positions == max(line_positions), NA, line_positions)) 

labels <- read_csv(WORDINPATH) %>%
  group_by(category_label) %>%
  summarise(category_label_long1 = paste(word, collapse = "-")) %>%
  rename(category_label1 = category_label) %>%
  mutate(category_label2 = category_label1,
         category_label_long2 = category_label_long1)

pdf("closest_quartile_plot_CORRECTED.pdf", width = 20, height = 12)
df_for_plotting  %>% 
  left_join(labels %>% select(category_label1, category_label_long1)) %>%
  left_join(labels %>% select(category_label2, category_label_long2)) %>%
  ggplot(aes(x = category_label_long1, y = category_label_long2)) +
  geom_text(aes(label = lang_formated), size = 1.5) +
  geom_hline(aes(yintercept = line_positions)) + 
  geom_vline(aes(xintercept = line_positions)) + 
  geom_hline(aes(yintercept = 14.5)) + 
  geom_vline(aes(xintercept = 14.5)) + 
  labs(caption = "Note: A lang listed in a cell means that the words are all in the same cluster for that lang.\n CAPITALS indicate, for that lang, Other Category is in the closest tile to Anchor Category\n (note that being 'close' here is not symmetrical).\n This is based on 700 words common to all models with 50 clusters each.",
       title = "Top Quartile",
       x = "Anchor Category",
       y = "Other Category") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

df_for_james <- df_for_plotting  %>% 
  left_join(labels %>% select(category_label1, category_label_long1)) %>%
  left_join(labels %>% select(category_label2, category_label_long2)) %>%
  left_join(counts)

write_csv(df_for_james, "language_counts_by_category_pair.csv")
 
####################### CLOSEST DECILE PLOT ######################

targs_with_links <- centroids_dist %>%
  select(-cos_dist, -quartile) %>%
  right_join(targets %>% select(-clust_num2, -category_label2)) %>%
  rename(decile_from_clust_num1 = decile) %>%
  right_join(targets %>% select(lang, clust_num2, category_label2)) %>%
  filter(clust_num1 != clust_num2) %>%
  mutate(close = ifelse(decile_from_clust_num1 == 10, TRUE, FALSE)) %>%
  mutate(lang_formated = ifelse(close, toupper(lang), lang)) %>%
  select(category_label1, category_label2, lang_formated)  %>% 
  filter(!(category_label1 %in%  BADCATS)) %>%
  filter(!(category_label2 %in% BADCATS))

df_for_plotting <- targs_with_links %>%
  group_by(category_label1, category_label2) %>%
  mutate(n = 1:n()) %>%
  mutate(lang_formated = ifelse(n %% 6 == 0, paste0(lang_formated, "\n"), lang_formated)) %>%
  summarise(lang_formated = paste(lang_formated,collapse=" ")) %>%
  ungroup()  %>%
  mutate(line_positions = as.numeric(factor(category_label1, levels = unique(category_label1))), 
         line_positions = line_positions + .5,  
         line_positions = ifelse(line_positions == max(line_positions), NA, line_positions)) 

labels <- read_csv(WORDINPATH) %>%
  group_by(category_label) %>%
  summarise(category_label_long1 = paste(word, collapse = "-")) %>%
  rename(category_label1 = category_label) %>%
  mutate(category_label2 = category_label1,
         category_label_long2 = category_label_long1)

pdf("closest_decile_plot_CORRECTED.pdf", width = 20, height = 12)
df_for_plotting  %>% 
  left_join(labels %>% select(category_label1, category_label_long1)) %>%
  left_join(labels %>% select(category_label2, category_label_long2)) %>%
  ggplot(aes(x = category_label_long1, y = category_label_long2)) +
  geom_text(aes(label = lang_formated), size = 1.5) +
  geom_hline(aes(yintercept = line_positions)) + 
  geom_vline(aes(xintercept = line_positions)) + 
  geom_hline(aes(yintercept = 14.5)) + 
  geom_vline(aes(xintercept = 14.5)) + 
  labs(caption = "Note: A lang listed in a cell means that the words are all in the same cluster for that lang.\n CAPITALS indicate, for that lang, Other Category is in the closest tile to Anchor Category\n (note that being 'close' here is not symmetrical).\n This is based on 700 words common to all models with 50 clusters each.",
       title = "Top Decile",
       x = "Anchor Category",
       y = "Other Category") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

### N LANGUAGES BY CATEGORY ####
OUTFILE <- "../../data/v3/n_languages_by_category.csv" 
n_langs <- targs_with_links %>%
  left_join(labels %>% select(category_label1, category_label_long1)) %>%
  left_join(labels %>% select(category_label2, category_label_long2)) %>%
  select(lang_formated, category_label_long1, category_label_long2) %>%
  gather("x","category_label",  -lang_formated) %>%
  select(-x) %>%
  mutate(lang = tolower(lang_formated)) %>%
  select(-lang_formated) %>%
  distinct() %>%
  count(category_label) %>%
  rename(n_languages = n)

write_csv(n_langs, OUTFILE)
