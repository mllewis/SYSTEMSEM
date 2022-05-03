# save pickled word count dataframe as csv

library(tidyverse)
library(here)
library(reticulate)

use_python("/Users/mollylewis/anaconda2/envs/py3/bin/python/") # python 3
PYTHON_SOURCE <- here("analyses/07_frequency_control/scripts/pickle_reader.py")
PICKLE_PATH <- here("data/raw/models/all_model/essay_word_counts.pkl")
WORD_FREQ_OUT <- here("analyses/07_frequency_control/scripts/essay_word_counts.csv")

source_python(PYTHON_SOURCE)
pickle_data <- read_pickle_file(PICKLE_PATH)

frequency_df <- map_df(1:length(pickle_data),
       function(x,y) {py_to_r(y[x][[1]]) %>%
           mutate(essay_id = names(y[x]))}, pickle_data)

write_csv(frequency_df, WORD_FREQ_OUT)

