# get appropriate WALS codes for merging with ling dist data
# these codes are identical to the eco codes, except arabic

library(tidyverse)

INFILE <-  "../../../../../data/processed/lang_names/ets_to_google_langcodes_complete.csv"
OUTFILE <- "data/iso_to_wals_for_ling_dists.csv"

#Read ETS language info
lang_names <- read_csv(INFILE) %>% select(iso, ETS_lang_name, lang_name2) %>%
  mutate(asjp_code = iso, 
         wals_code = iso) 

lang_names[lang_names$iso == "cmn", "wals_code"] = "mnd"
lang_names[lang_names$iso == "nld", "wals_code"] = "dut"
lang_names[lang_names$iso == "deu", "wals_code"] = "ger"
lang_names[lang_names$iso == "ell", "wals_code"] = "grk"
lang_names[lang_names$iso == "fra", "wals_code"] = "fre"
lang_names[lang_names$iso == "ibo", "wals_code"] = "igb"
lang_names[lang_names$iso == "kan", "wals_code"] = "knd"
lang_names[lang_names$iso == "mal", "wals_code"] = "mym"
lang_names[lang_names$iso == "mar", "wals_code"] = "mhi"
lang_names[lang_names$iso == "ron", "wals_code"] = "rom"
lang_names[lang_names$iso == "pes", "wals_code"] = "prs"
lang_names[lang_names$iso == "arb", "wals_code"] = "ams"
lang_names[lang_names$iso == "arb", "asjp_code"] = "arz"
lang_names[lang_names$iso == "tgl", "wals_code"] = "tag"

write_csv(lang_names, OUTFILE)


