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
library(writexl)

# 1. CHEMINS ----
path_raw <- "data/data_raw"
path_processed <- "data/data_processed"

# 2. FONCTIONS UTILITAIRES ----

clean_base <- function(df) {
  df %>%
    clean_names() %>%
    remove_empty(which = c("rows", "cols")) %>%
    mutate(
      across(
        where(is.character),
        ~ .x %>%
          str_squish() %>%
          na_if("") %>%
          na_if("...") %>%
          na_if("NA") %>%
          na_if("N/A")
      ))}

read_csv_clean <- function(path, ...) {
  read_csv(path, show_col_types = FALSE, ...) %>%
    clean_base()}

read_delim_clean <- function(path, delim = ";", ...) {
  read_delim(
    path,
    delim = delim,
    escape_double = FALSE,
    trim_ws = TRUE,
    show_col_types = FALSE,
    ...
  ) %>%
    clean_base()}

get_eurostat_clean <- function(id) {
  get_eurostat(id, time_format = "num") %>%
    clean_base()}

# 3. IMPORT DES DONNEES ----

fr_effectifs_etudiants_etrangers_france <- GET(
  "https://www.data.gouv.fr/api/1/datasets/r/7dfbef75-4fd8-464c-875d-38c4e067969b"
) %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON()

fr_effectifs_etab <- read_delim_clean(
  file.path(path_raw, "fr-esr-statistiques-sur-les-effectifs-d-etudiants-inscrits-par-etablissement-hcp.csv"))

fr_doctorat_etranger <- read_delim_clean(
  file.path(path_raw, "fr-esr-nouveaux-inscrits-doctorat-diplome-de-plus-haut-niveau-france-etranger.csv"))

years <- c("2021-22", "2022-23", "2023-24", "2024-25")
uk_hesa_all <- data.frame()
for (y in years) {
  temp <- read_csv_clean(
    file.path(path_raw, paste0("hesa_uk_data/table-1-(", y, ").csv")),
    skip = 13
  )
  uk_hesa_all <- rbind(uk_hesa_all, temp)}


eu_type_institution <- get_eurostat_clean("educ_uoe_enrt01")
eu_mobility_prev_diploma <- get_eurostat_clean("educ_uoe_mobs02")
eu_mobility_citizenship <- get_eurostat_clean("educ_uoe_mobs01")

unesco <- read_csv_clean(file.path(path_raw, "unesco", "data.csv"))

# 4. SAUVEGARDE INTERMEDIAIRE ----

datasets <- list(
  fr_effectifs_etudiants_etrangers_france = fr_effectifs_etudiants_etrangers_france,
  fr_effectifs_etab = fr_effectifs_etab,
  fr_doctorat_etranger = fr_doctorat_etranger,
  uk_hesa_all = uk_hesa_all,
  eu_type_institution = eu_type_institution,
  eu_mobility_prev_diploma = eu_mobility_prev_diploma,
  eu_mobility_citizenship = eu_mobility_citizenship,
  unesco = unesco
)

iwalk(
  datasets,
  ~ saveRDS(.x, file.path(path_processed, paste0(.y, ".rds")))
)

# 5. TABLE DE SUIVI DES SOURCES ----

sources_table <- tribble(
  ~source_name, ~object_name, ~source_type, ~project_path, ~source_link,
  "Effectifs d'étudiants étrangers dans les établissements et les formations de l'enseignement supérieur",
  "fr_effectifs_etudiants_etrangers_france", "api_download", NA,
  "https://www.data.gouv.fr/api/1/datasets/r/7dfbef75-4fd8-464c-875d-38c4e067969b",
  
  "Effectifs d'étudiants inscrits dans les établissements et les formations de l'enseignement supérieur",
  "fr_effectifs_etab", "local_csv",
  "data/data_raw/fr-esr-atlas_regional-effectifs-d-etudiants-inscrits_agregeables.csv",
  "https://data.enseignementsup-recherche.gouv.fr/explore/dataset/fr-esr-atlas_regional-effectifs-d-etudiants-inscrits_agregeables/information/?disjunctive.rgp_formations_ou_etablissements",
  
  "Nouveaux inscrits doctorat",
  "fr_doctorat_etranger", "local_csv",
  "data/data_raw/fr-esr-nouveaux-inscrits-doctorat-diplome-de-plus-haut-niveau-france-etranger.csv",
  "https://data.enseignementsup-recherche.gouv.fr/explore/assets/fr-esr-nouveaux-inscrits-doctorat-diplome-de-plus-haut-niveau-france-etranger/",
  
  "HESA UK",
  "uk_hesa_all", "local_csv",
  "data/data_raw/hesa_uk_data/table-1-202*-**.csv",
  "https://www.hesa.ac.uk/data-and-analysis/students/table-1",
  
  "Eurostat educ_uoe_enrt01",
  "eu_type_institution", "eurostat_api", NA,
  "https://ec.europa.eu/eurostat/databrowser/view/educ_uoe_enrt01/default/table?lang=en",
  
  "Eurostat educ_uoe_mobs02",
  "eu_mobility_prev_diploma", "eurostat_api", NA,
  "https://ec.europa.eu/eurostat/databrowser/view/educ_uoe_mobs02/default/table?lang=en",
  
  "Eurostat educ_uoe_mobs01",
  "eu_mobility_citizenship", "eurostat_api", NA,
  "https://ec.europa.eu/eurostat/databrowser/view/educ_uoe_mobs01/default/table?lang=en",
  
  "UNESCO data (outbound and inbound students, outbound mobility ratio, inbound mobility rate)",
  "unesco", "local_csv",
  "data/data_raw/unesco/data.csv",
  "https://databrowser.uis.unesco.org/browser/EDUCATION/UIS-EducationOPRI/int-stud",
  
)
write_xlsx(list(sources_table = sources_table), path = file.path(path_processed, "sources_table.xlsx"))