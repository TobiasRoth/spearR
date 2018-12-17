rm(list=ls(all=TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)

# Connection to data base
db <- src_sqlite(path = "~/Documents/Dropbox/DB_BDM.db", create = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare csv file with monitoring data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Randomly select 10 surveys
set.seed(1234)
ausw <- sample(tbl(db, "KD_EPT") %>% pull(aID_KD), 10)

# Export monitoring data to these then surveys
left_join(tbl(db, "EPT"), tbl(db, "TRAITS_EPT")) %>%
  left_join(tbl(db, "KD_EPT")) %>% 
  filter(IBCH_gultig == 1 & !is.na(SPEAR_class)) %>% 
  as.tibble() %>% 
  filter(!is.na(match(aID_KD, ausw))) %>% 
  transmute(Name_1 = factor(aID_KD) %>% as.integer(),
            Name_2 = yearEPT,
            Name_3	= "",
            Taxa = Taxa_Spear,
            Abundance = IBCH_Anz_Ind) %>% 
  write_delim(path = "Data/monitoring_data.csv", delim = ";")
