# get tsne coordinates by language for centroid plot
library(tidyverse)
library(feather)
library(here)

METADATA <- here("data/raw/models/all_model/merged_metadata.csv")
DOCVECS <- here("data/processed/models/all_model/docvecs_all_model.feather")
DOCINDICES <- here("data/processed/models/all_model/doctag_indices_all_model.feather")
TSNE_CACHE <- here("analyses/04_predicting_semantic_sim/data/tsne_dims_cached_all.csv")
TSNE_OUT <-  here("analyses/04_predicting_semantic_sim/data/tsne_language_coordinates.csv")

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

d <- doctag_indices %>%
  left_join(metadata_clean) %>%
  left_join(docvecs) %>%
  nest(-1:-10, .key = "doc_vector") %>%
  select(-doc_count, -offset, -test_center_country_code) 

# get tsney- coordinates
mats <- d %>%
  select(doc_vector) %>%
  unnest() %>%
  as.matrix()

tsne_out = Rtsne::Rtsne(mats)
tsne_dims <- tsne_out$Y %>%
  as.data.frame() %>%
  rename(tsne_X = V1,
         tsne_Y = V2) %>%
  bind_cols(d %>% select(essay_id, L1_code, prompt_id)) %>%
  select(everything(), tsne_X, tsne_Y)

write_csv(tsne_dims, TSNE_CACHE)

tsne_dims <- read_csv(TSNE_CACHE)

tsne_centroids_by_prompt <- tsne_dims %>%
  nest(1:2, .key = "doc_vector") %>%
  mutate(L1_prompt = paste0(L1_code, "_", prompt_id )) %>%
  split(.$L1_prompt) %>% 
  map_df(get_group_centroid, "L1_prompt") %>%
  separate(L1_prompt, c("L1_code", "prompt_id"), sep = "_") 

tsne_prompt_mean_centroids <- tsne_centroids_by_prompt %>%
  split(.$L1_code) %>% 
  map_df(~colMeans(.[,-1:-2])) %>%
  t() %>% 
  as.data.frame() %>%
  rownames_to_column("L1_code") %>%
  rename(tsne_X = V1,
         tsne_Y = V2)

write_csv(tsne_prompt_mean_centroids, TSNE_OUT)
