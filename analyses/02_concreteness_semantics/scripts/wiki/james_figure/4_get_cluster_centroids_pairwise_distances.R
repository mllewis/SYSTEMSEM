# Get pairwise distances between all cluster centroids 

library(tidyverse)
library(data.table)

####################### PARAMETERS ######################

INPATH <- "../../data/v3/lang_semantics_cluster_by_lang_centroids.csv"
OUTPATH <- "../../data/v3/pairwise_semantic_centroid_distances.csv"

####################### GET DISTANCES ######################

# check this, why are some NA?
centroids <- read_csv(INPATH, col_names = F) %>%
  rename(lang = X1,
         cluster = X2) %>%
  mutate_at(2:302, as.numeric)

get_lang_centroid_distances <- function(current_lang, centroids){
   current_centroids <- centroids %>%
                        filter(lang == current_lang) %>%
                        select(-lang)
   
  pairwise_centroids_cosines_wide <- coop::cosine(t(as.matrix(current_centroids[,c(-1)]))) 
  
  pairwise_cosines_long <- melt(pairwise_centroids_cosines_wide) 
  
  tidy_long <- pairwise_cosines_long %>%
    rename(cluster1 = Var1,
           cluster2 = Var2,
           cos_dist = value) %>%
    mutate(lang = current_lang) 

  ranks = tidy_long %>%
    group_by(cluster1) %>%
    mutate(quartile = ntile(cos_dist, 4),
           decile = ntile(cos_dist,10))  %>%
    select(lang, everything())

  ranks
}


clust_dists <- map_df(unique(centroids$lang), get_lang_centroid_distances, centroids)

write_csv(clust_dists, OUTPATH)