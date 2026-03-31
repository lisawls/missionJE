###############################################################################
# PROJET TSE | Mobilité étudiante internationale
# Script : 01_import_data.R
# Objet   : importer les données brutes depuis fichiers locaux et APIs
###############################################################################

# 0. PACKAGES ----
library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(readr)
library(eurostat)

# 1. CHEMINS ----
path_raw <- "data/data_raw"
path_processed <- "data/data_processed"

# 2. IMPORT DES DONNEES FRANCE ----

# 2.1 Effectifs d'étudiants étrangers dans les établissements et les formations de l'enseignement supérieur
fr_effectifs_etudiants_etrangers_france <- GET(
  "https://www.data.gouv.fr/api/1/datasets/r/7dfbef75-4fd8-464c-875d-38c4e067969b"
) %>% content(as = "text", encoding = "UTF-8") %>% 
  fromJSON()

# 2.2 Effectifs étudiants par établissement
fr_effectifs_etab <- read_delim(
  file.path(path_raw, "fr-esr-statistiques-sur-les-effectifs-d-etudiants-inscrits-par-etablissement-hcp.csv"),
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  show_col_types = FALSE
) %>% 
  clean_names()

# 2.3 Nouveaux inscrits en doctorat selon diplôme de plus haut niveau
fr_doctorat_etranger <- read_delim(
  file.path(path_raw, "fr-esr-nouveaux-inscrits-doctorat-diplome-de-plus-haut-niveau-france-etranger.csv"),
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  show_col_types = FALSE
) |>
  clean_names()

# 3. IMPORT DES DONNEES ROYAUME-UNI ----

uk_hesa_table1 <- read_csv(
  file.path(path_raw, "hesa_uk_data/table-1-(2024-25).csv"),
  skip = 13,
  show_col_types = FALSE
) %>% 
  clean_names()

# 4. IMPORT DES DONNEES EUROSTAT ----

# 4.1 Inscriptions par type d'institution
eu_type_institution <- get_eurostat(
  "educ_uoe_enrt01",
  time_format = "num"
) %>% clean_names()

# 4.2 Mobilité des étudiants par pays de diplôme précédent
eu_mobility_prev_diploma <- get_eurostat(
  "educ_uoe_mobs02",
  time_format = "num"
) %>% clean_names()

# 4.3 Mobilité des étudiants par citoyenneté
eu_mobility_citizenship <- get_eurostat(
  "educ_uoe_mobs01",
  time_format = "num"
) %>% clean_names()

# 5. SAUVEGARDE INTERMEDIAIRE ----
saveRDS(fr_effectifs_etudiants_etrangers_france, file.path(path_processed, "fr_effectifs_etudiants_etrangers_france.rds"))
saveRDS(fr_effectifs_etab, file.path(path_processed, "fr_effectifs_etab.rds"))
saveRDS(fr_doctorat_etranger, file.path(path_processed, "fr_doctorat_etranger.rds"))
saveRDS(uk_hesa_table1, file.path(path_processed, "uk_hesa_table1.rds"))
saveRDS(eu_type_institution, file.path(path_processed, "eu_type_institution.rds"))
saveRDS(eu_mobility_prev_diploma, file.path(path_processed, "eu_mobility_prev_diploma.rds"))
saveRDS(eu_mobility_citizenship, file.path(path_processed, "eu_mobility_citizenship.rds"))

# 6. TABLE DE SUIVI DES SOURCES ----

sources_table <- tibble(
  source_name = c(
    "Effectifs d'étudiants étrangers dans les établissements et les formations de l'enseignement supérieur",
    "Effectifs d'étudiants inscrits dans les établissements et les formations de l'enseignement supérieur",
    "Nouveaux inscrits doctorat",
    "HESA UK table 1",
    "Eurostat educ_uoe_enrt01",
    "Eurostat educ_uoe_mobs02",
    "Eurostat educ_uoe_mobs01"
  ),
  path_or_id = c(
    "https://www.data.gouv.fr/api/1/datasets/r/7dfbef75-4fd8-464c-875d-38c4e067969b",
    "data/raw/fr-esr-statistiques-sur-les-effectifs-d-etudiants-inscrits-par-etablissement-hcp.csv",
    "data/raw/fr-esr-nouveaux-inscrits-doctorat-diplome-de-plus-haut-niveau-france-etranger.csv",
    "https://www.hesa.ac.uk/data-and-analysis/students/table-1",
    "educ_uoe_enrt01",
    "educ_uoe_mobs02",
    "educ_uoe_mobs01"
  ))

write_csv(sources_table, file.path(path_processed, "sources_table.csv"))