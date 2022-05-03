# Get pairwise language distances based on essay vectors for all model
# saves two files: raw centroid vetors (for dendro plotting) and distances (for predicting)

library(tidyverse)
library(feather)
library(here)

METADATA <- here("data/raw/models/all_model/merged_metadata.csv")
DOCVECS <- here("data/processed/models/all_model/docvecs_all_model.feather")
DOCINDICES <- here("data/processed/models/all_model/doctag_indices_all_model.feather")
OUTFILE_CENTROIDS <-  here("analyses/04_predicting_semantic_sim/data/language_semantics_centroids.csv") 
OUTFILE_DISTANCES <-  here("analyses/04_predicting_semantic_sim/data/language_semantics_distance.csv") 

language_semantics_distance.csv
get_group_centroid <- function(doc_vecs_df, group_name){
  centroid <- doc_vecs_df %>%
    select(doc_vector) %>%
    unnest(doc_vector) %>%
    colMeans() 
  
  this_group_name  <- doc_vecs_df %>%
    select_(group_name) %>%
    slice(1) %>%
    unlist()
  
  df <- data.frame(t(centroid)) %>%
    mutate(x = this_group_name) %>%
    select(x, everything())
  
  names(df)[1] = c(as.name(group_name))
  df
}

# get essay document vectors with metadata (38,500 essays)
doctag_indices <- read_feather(DOCINDICES) 

docvecs <- read_feather(DOCVECS) %>%
  as.data.frame() %>%
  mutate(offset = 0:(n()-1)) %>%
  select(offset, everything())

metadata <- read_csv(METADATA) 
metadata_clean <- metadata %>%
  mutate_if(is.character, as.factor)  %>%
  mutate(essay_id = as.character(essay_id))

# merge
d <- doctag_indices %>%
  left_join(metadata_clean) %>%
  left_join(docvecs) %>%
  nest(-1:-10, .key = "doc_vector") %>%
  select(-doc_count, -offset, -test_center_country_code) 

# get language distances
centroids_prompt <- d %>% # get centroid by prompt and L1
  mutate(L1_prompt = paste0(L1_code, "_", prompt_id )) %>%
  split(.$L1_prompt) %>% 
  map_df(get_group_centroid, "L1_prompt") %>%
  separate(L1_prompt, c("L1_code", "prompt_id"), sep = "_")

prompt_mean_centroids <- centroids_prompt %>% # get centroid by prompt
  split(.$L1_code) %>% 
  map_df(~colMeans(.[,-1:-2])) %>%
  t() %>% 
  as.data.frame() %>%
  rownames_to_column("L1_code")

prompt_mean_centroids_mat = as.matrix(prompt_mean_centroids[,-1])
rownames(prompt_mean_centroids_mat) = prompt_mean_centroids[,1]

#### save raw prompt centroids #### (necessary to save as mat so can run dedro in plotting script)
prompt_mean_centroids_mat_to_save <- prompt_mean_centroids_mat %>%
  as.data.frame() %>%
  rownames_to_column()

write_csv(prompt_mean_centroids_mat_to_save, OUTFILE_CENTROIDS) 

#### save tidy distances ####
dist_matrix <- dist(prompt_mean_centroids_mat)
dist_matrix_kable <- as.matrix(dist_matrix)
dist_matrix_kable[upper.tri(dist_matrix_kable)] <- NA

semantic_dist <- dist_matrix_kable %>%
  as.data.frame() %>%
  rownames_to_column("lang1_ETS") %>%
  gather("lang2_ETS", "semantic_dist", -1) %>%
  mutate_if(is.character, tolower)

write_csv(semantic_dist, OUTFILE_DISTANCES) 

