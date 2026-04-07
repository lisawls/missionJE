###############################################################################
# PROJET TSE | Mobilité étudiante internationale
# Script : 02_clean_data.R
# Objet   : nettoyer, harmoniser et documenter les données importées
###############################################################################

# 0. PACKAGES ----
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(countrycode)
library(readr)
library(writexl)

# 1. CHEMINS ----
path_raw <- "data/data_raw"
path_processed <- "data/data_processed"
path_clean <- "data/data_clean"

# 2. CHARGEMENT DES DONNEES IMPORTEES ----
fr_effectifs_etudiants_etrangers_france <- readRDS(file.path(path_processed, "fr_effectifs_etudiants_etrangers_france.rds"))
fr_effectifs_etab <- readRDS(file.path(path_processed, "fr_effectifs_etab.rds"))
fr_doctorat_etranger <- readRDS(file.path(path_processed, "fr_doctorat_etranger.rds"))
uk_hesa_all <- readRDS(file.path(path_processed, "uk_hesa_all.rds"))
eu_type_institution <- readRDS(file.path(path_processed, "eu_type_institution.rds"))
eu_mobility_prev_diploma <- readRDS(file.path(path_processed, "eu_mobility_prev_diploma.rds"))
eu_mobility_citizenship <- readRDS(file.path(path_processed, "eu_mobility_citizenship.rds"))
unesco <- readRDS(file.path(path_processed, "unesco.rds"))

# 3. FONCTIONS UTILITAIRES ----
write_csv_list <- function(data_list, path_processed) {
  iwalk(
    data_list,
    ~ write_csv(.x, file.path(path_clean, paste0(.y, ".csv"))))}


# 3. NETTOYAGE FRANCE | EFFECTIFS ETUDIANTS ETRANGERS
custom_dict <- c(
  "BIRMANIE" = "MMR",                # Myanmar
  "KOSOVO" = "XKX",                 # code non officiel mais utilisé
  "REPUBLIQUE TCHEQUE" = "CZE",     # Czechia
  "GUINEE EQUATORIALE" = "GNQ",
  "ACORES" = "PRT",                 # région du Portugal
  "AUTRE PAYS, ETRANGER SANS AUTRE INDICATION" = NA,
  "SANS NATIONALITE" = NA
)

fr_effectifs_etudiants_etrangers_france_clean <- fr_effectifs_etudiants_etrangers_france %>% 
  mutate(across(
    matches("^(total_|mob_|etr_)"),
    ~ as.numeric(ifelse(.x == "<5", "2.5", .x))
  ), rentree = as.numeric(rentree)) %>% 
  filter(rentree >= 2021) %>% 
  mutate(
    iso3 = countrycode(
      nationalite,
      origin = "country.name.fr",
      destination = "iso3c",
      custom_match = custom_dict))

# 3. NETTOYAGE FRANCE | EFFECTIFS PAR ETABLISSEMENT ----


# 4. NETTOYAGE FRANCE | DOCTORAT ----

# 5. NETTOYAGE UK | HESA ----

# 6. NETTOYAGE EUROSTAT | TYPE D'INSTITUTION ----

# 7. NETTOYAGE EUROSTAT | MOBILITE PAR DIPLOME PRECEDENT ----

# 8. NETTOYAGE EUROSTAT | MOBILITE PAR CITOYENNETE ----

# NETTOTAGE UNESCO ----

unesco_clean <- unesco %>%
  mutate(indicator_id = case_when(
    indicator_id == "26637" ~ "inbound_total",
    indicator_id == "MOR.5T8.40510" ~ "outbound_mobility_ratio",
    indicator_id == "OE.5T8.40510" ~ "outbound_total",
    indicator_id == "MSEP.5T8" ~ "inbound_mobility_rate",
    TRUE ~ indicator_id
  )) %>% 
  select(indicator_id, geo_unit, year, value) %>%
  pivot_wider(
    names_from = indicator_id,
    values_from = value) %>% 
  filter(year > 2021) %>%
  filter(str_detect(geo_unit, "^[A-Z]{3}$"))


# 14. TABLE DE DICTIONNAIRE MINIMALE ----
data_dictionary <- tibble(
  object_name = c(
    "fr_effectifs_etudiants_etrangers_france_clean",
    "fr_effectifs_etab_clean",
    "fr_doctorat_etranger_clean",
    "uk_hesa_table1_clean",
    "eu_type_institution_clean",
    "eu_mobility_prev_diploma_clean",
    "eu_mobility_citizenship_clean",
    "unesco_clean"
  ),
  description = c(
    "France | effectifs d'étudiants étrangers, années récentes, variables numériques harmonisées, code ISO3 ajouté",
    "France | effectifs par établissement, années récentes, part d'étudiants internationaux calculée",
    "France | doctorat, nettoyage texte de base",
    "UK | HESA table 1 nettoyée",
    "Eurostat | inscriptions par type d'institution, années récentes",
    "Eurostat | mobilité selon pays du diplôme précédent, années récentes",
    "Eurostat | mobilité selon citoyenneté, années récentes",
    "UNESCO | fusion des fichiers entrants, sortants et ratio de mobilité sortante, format large"
  )
)
write_xlsx(list(data_dictionary = data_dictionary), path = file.path(path_processed, "data_dictionary_clean.xlsx"))


# 6. SAUVEGARDE ----
clean_datasets <- list(
  fr_effectifs_etudiants_etrangers_france_clean = fr_effectifs_etudiants_etrangers_france_clean,
  # fr_effectifs_etab_clean = fr_effectifs_etab_clean,
  # fr_doctorat_etranger_clean = fr_doctorat_etranger_clean,
  # uk_hesa_all_clean = uk_hesa_all_clean,
  # eu_type_institution_clean = eu_type_institution_clean,
  # eu_mobility_prev_diploma_clean = eu_mobility_prev_diploma_clean,
  # eu_mobility_citizenship_clean = eu_mobility_citizenship_clean,
  unesco_clean = unesco_clean
)

write_csv_list(clean_datasets, path_processed)

# PISTES D'ANALYSE A AJOUTER ENSUITE
###############################################################################
# 1. Isoler autant que possible les niveaux master et licence
# 2. Distinguer mobilité diplômante et mobilité d'échange
# 3. Construire un focus Toulouse / Occitanie / France
# 4. Comparer TSE à des établissements cibles
# 5. Vérifier l'offre de formations en anglais au niveau master
# 6. Construire un focus Royaume-Uni post-Covid