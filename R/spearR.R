
rm(list=ls(all=TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings and load data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)

# Load data
dat <- read_csv("Data/monitoring_data.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Taxa traits and classification ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dat %>% 
  left_join(read_csv("Indicate_tables/Trat_Database.csv")) %>% 
  mutate(SPEAR_class = as.integer(Sensitivity > -0.36 & Generation >= 0.5 & Refuge == 1 & Exposed == 1)) %>% 
  group_by(Taxa) %>% 
  summarise(
    Sensitivity = mean(Sensitivity),
    Generation = mean(Generation),
    Refuge = mean(Refuge),
    Exposed = mean(Exposed),
    SPEAR_class = mean(SPEAR_class)) %>% 
  data.frame()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculate SPEAR-Index ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dat %>%
  left_join(read_csv("Indicate_tables/Trat_Database.csv")) %>%
  mutate(SPEAR_class = as.integer(Sensitivity > -0.36 &
                                    Generation >= 0.5 &
                                    Refuge == 1 &
                                    Exposed == 1)) %>%
  group_by(Name_1, Name_2) %>%
  summarise(SPEAR = round(100 * sum(log10(4 * Abundance + 1) * SPEAR_class) /
                            sum(log10(4 * Abundance + 1))  / 34, 2))


