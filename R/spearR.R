
rm(list=ls(all=TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)
library(readxl)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Read and prepare species data (export from Midat) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data
d <- read_excel("Data/Beispielsexport-MIDAT.xlsx") %>% 
  transmute(
    siteID = `Numéro station`,
    Datum = as.Date(paste(Année, Mois, Jour, sep = "-")) %>% as.character(),
    Phylum = Phylum,
    Klasse = Classe,
    Ordnung = Ordre,
    Familie = Famille,
    Ind = as.integer(Abondance)
  )

# Construct IBCH-Taxa 
d$Taxa_IBCH <- apply(d[, c("Phylum", "Klasse", "Ordnung", "Familie")], 1, function(x) x[last(which(!is.na(x)))])
d$Taxa_IBCH[d$Taxa_IBCH == "Psephenidae"] <- "Psephenidae (=Eubriidae)"
d$Taxa_IBCH[d$Taxa_IBCH == "Scirtidae"] <- "Scirtidae (=Helodidae)"
d$Taxa_IBCH[d$Taxa_IBCH == "Nematoda"] <- "Nemathelminthes"
d$Taxa_IBCH[d$Taxa_IBCH == "Acari"] <- "Hydracarina"
d$Taxa_IBCH[d$Taxa_IBCH == "Arachnida"] <- "Hydracarina"
d$Taxa_IBCH[d$Taxa_IBCH == "Prostigmata"] <- "Hydracarina"
d <- d %>% filter(Taxa_IBCH != "Diptera")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add spear class to data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load taxa list with Midat (DM) and spear taxa names and the traits to
# calculate the Spear class
li <- left_join(
  read_xlsx("Taxa_lists/1650_A32_Artangaben_EPT_v4.xlsx"),
  read_xlsx("Taxa_lists/1650_Inidicate_trait_database.xlsx"),
  by = c("Taxa_Spear_Calculator" = "Taxa")) %>% 
  mutate(SPEAR_class = as.integer(Sensitivity > -0.36 & Generation >= 0.5 & Refuge == 1 & Exposed == 1)
)

# Add SPEAR class to data
d <- 
  d %>% 
  left_join(
    li %>% 
      dplyr::select(Taxaname_BDM, SPEAR_class),
    by = c("Taxa_IBCH" = "Taxaname_BDM"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculate SPEAR  ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Aggregate on IBCH-taxa level and calcualte Spear-Index
d %>% 
  filter(!is.na(SPEAR_class)) %>%
  group_by(siteID, Datum, Taxa_IBCH, SPEAR_class) %>%
  summarise(Ind = sum(Ind)) %>%
  group_by(siteID, Datum) %>%
  dplyr::summarise(
    SPEAR = round(100 * sum(log10(4 * Ind + 1) * SPEAR_class) / sum(log10(4 * Ind + 1)), 2)) %>% 
  as.data.frame()

