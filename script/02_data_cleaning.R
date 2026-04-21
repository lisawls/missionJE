###############################################################################
# PROJET TSE | Mobilité étudiante internationale
# Script : 02_clean_data.R
# Objet   : nettoyer, harmoniser et documenter les données importées
###############################################################################

# PACKAGES ----
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(countrycode)
library(readr)
library(writexl)

# CHEMINS ----
path_raw <- "data/data_raw"
path_processed <- "data/data_processed"
path_clean <- "data/data_clean"

# CHARGEMENT DES DONNEES IMPORTEES ----
fr_effectifs_etudiants_etrangers_france <- readRDS(file.path(path_processed, "fr_effectifs_etudiants_etrangers_france.rds"))
fr_effectifs_etablissement_2022 <- readRDS(file.path(path_processed, "fr_effectifs_etablissement_2022.rds"))
fr_effectifs_etablissement_2023 <- readRDS(file.path(path_processed, "fr_effectifs_etablissement_2023.rds"))
fr_effectifs_etablissement_2024 <- readRDS(file.path(path_processed, "fr_effectifs_etablissement_2024.rds"))
# uk_hesa_all <- readRDS(file.path(path_processed, "uk_hesa_all.rds"))
# eu_type_institution <- readRDS(file.path(path_processed, "eu_type_institution.rds"))
# eu_mobility_prev_diploma <- readRDS(file.path(path_processed, "eu_mobility_prev_diploma.rds"))
# eu_mobility_citizenship <- readRDS(file.path(path_processed, "eu_mobility_citizenship.rds"))
unesco <- readRDS(file.path(path_processed, "unesco.rds"))

# FONCTIONS UTILITAIRES ----
write_csv_list <- function(data_list, path_processed) {
  iwalk(
    data_list,
    ~ write_csv(.x, file.path(path_clean, paste0(.y, ".csv"))))}


# NETTOYAGE FRANCE | EFFECTIFS ETUDIANTS ETRANGERS
fr_effectifs_etudiants_etrangers_france_clean <- fr_effectifs_etudiants_etrangers_france %>% 
  filter(rentree >= 2021) %>% 
  mutate(
    iso3 = case_when(
      nationalite == "ACORES" ~ "PRT",
      nationalite == "KOSOVO" ~ "XKX",
      TRUE ~ iso3),
    name_fr = case_when(
      nationalite == "ACORES" ~ "Portugal",
      nationalite == "KOSOVO" ~ "Kosovo",
      TRUE ~ name_fr),
    zone_geographique_d_origine = case_when(
      nationalite == "ACORES" ~ "Europe",
      TRUE ~ zone_geographique_d_origine), 
    bologne = case_when(
      nationalite == "ACORES" ~ TRUE,
      nationalite == "KOSOVO" ~ FALSE,
      TRUE ~ as.logical(bologne)),
    oecd_members = case_when(
      nationalite == "ACORES" ~ TRUE,
      nationalite == "KOSOVO" ~ FALSE,
      TRUE ~ as.logical(oecd_members)),
    ue27 = case_when(
      nationalite == "ACORES" ~ TRUE,
      nationalite == "KOSOVO" ~ FALSE,
      TRUE ~ as.logical(ue27)), 
    across(
    matches("^(total_|mob_|etr_)"),
    ~ as.numeric(ifelse(.x == "<5", "2", .x))), 
    rentree = as.numeric(rentree))

# NETTOYAGE FRANCE | EFFECTIFS ETUDIANTS ETRANGERS ETABLISSEMENTS
etabs_cibles <- c("Sciences Po", "Université Paris Dauphine - PSL", "Université Toulouse Capitole", "Université Paris 1 - Panthéon Sorbonne")
fr_effectifs_etablissement_2022 <- fr_effectifs_etablissement_2022 %>% filter(etablissement %in% etabs_cibles)
fr_effectifs_etablissement_2023 <- fr_effectifs_etablissement_2023 %>% filter(etablissement %in% etabs_cibles)
fr_effectifs_etablissement_2024 <- fr_effectifs_etablissement_2024 %>% filter(etablissement %in% etabs_cibles)


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

# SAUVEGARDE ----
clean_datasets <- list(
  fr_effectifs_etudiants_etrangers_france_clean = fr_effectifs_etudiants_etrangers_france_clean,
  fr_effectifs_etablissement_2022 = fr_effectifs_etablissement_2022,
  fr_effectifs_etablissement_2023 = fr_effectifs_etablissement_2023,
  fr_effectifs_etablissement_2024 = fr_effectifs_etablissement_2024,
  # uk_hesa_all_clean = uk_hesa_all_clean,
  # eu_type_institution_clean = eu_type_institution_clean,
  # eu_mobility_prev_diploma_clean = eu_mobility_prev_diploma_clean,
  # eu_mobility_citizenship_clean = eu_mobility_citizenship_clean,
  unesco_clean = unesco_clean)

write_csv_list(clean_datasets, path_processed)