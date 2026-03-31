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

# 1. CHEMINS ----
path_raw <- "data/data_raw"
path_processed <- "data/data_processed"

# 2. CHARGEMENT DES DONNEES IMPORTEES ----
fr_effectifs_etudiants_etrangers_france <- readRDS(file.path(path_processed, "fr_effectifs_etudiants_etrangers_france.rds"))
fr_effectifs_etab <- readRDS(file.path(path_processed, "fr_effectifs_etab.rds"))
fr_doctorat_etranger <- readRDS(file.path(path_processed, "fr_doctorat_etranger.rds"))
uk_hesa_table1 <- readRDS(file.path(path_processed, "uk_hesa_table1.rds"))
eu_type_institution <- readRDS(file.path(path_processed, "eu_type_institution.rds"))
eu_mobility_prev_diploma <- readRDS(file.path(path_processed, "eu_mobility_prev_diploma.rds"))
eu_mobility_citizenship <- readRDS(file.path(path_processed, "eu_mobility_citizenship.rds"))


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

cols_counts <- c(
  "total_mobiles", "mob_univ", "mob_ec_commerce", "mob_inge_univ",
  "mob_inge_hors_univ", "mob_ec_art", "mob_autres_formations",
  "mob_idf", "mob_auvergnerhonealpes", "mob_occitanie", "mob_grandest",
  "mob_hautsdefrance", "mob_autres_regions", "total_etrangers",
  "etr_univ", "etr_ec_commerce", "etr_inge_univ", "etr_inge_hors_univ",
  "etr_ec_art", "etr_autres_formations", "etr_idf",
  "etr_auvergnerhonealpes", "etr_occitanie", "etr_grandest",
  "etr_hautsdefrance", "etr_autres_regions")

fr_effectifs_etudiants_etrangers_france_clean <- fr_effectifs_etudiants_etrangers_france %>% 
  mutate(across(
    matches("^(total_|mob_|etr_)"),
    ~ as.numeric(ifelse(.x == "<5", "2.5", .x))
  ), rentree = as.numeric(rentree)) %>% 
  filter(rentree > 2021) %>% 
  mutate(
    iso3 = countrycode(
      nationalite,
      origin = "country.name.fr",
      destination = "iso3c",
      custom_match = custom_dict))

# 3. NETTOYAGE FRANCE | EFFECTIFS PAR ETABLISSEMENT ----
fr_effectifs_etab_clean <- fr_effectifs_etab %>%
  filter(annee > 2021) %>% 
  select(annee, annee_universitaire, etablissement, type_detablissement,
         region_du_siege_de_l_etablissement, cycle_universitaire_cursus_lmd_l_1er_cycle,
         cycle_universitaire_cursus_lmd_m_2eme_cycle, cycle_universitaire_cursus_lmd_d_3eme_cycle,
         grande_discipline_droit_sciences_economiques_aes, discipline_sciences_economiques_gestion,
         nombre_total_detudiants_inscrits_inscriptions_principales_et_secondes_hors_doubles_inscriptions_cpge,
         attractivite_internationale_etudiants_de_nationalite_etrangere_issus_de_systeme_educatif_etranger) %>%
  mutate(
    part_internationaux = if_else(
      nombre_total_detudiants_inscrits_inscriptions_principales_et_secondes_hors_doubles_inscriptions_cpge > 0,
      attractivite_internationale_etudiants_de_nationalite_etrangere_issus_de_systeme_educatif_etranger /
        nombre_total_detudiants_inscrits_inscriptions_principales_et_secondes_hors_doubles_inscriptions_cpge,
      NA_real_
    )
  )


# 4. NETTOYAGE FRANCE | DOCTORAT ----

# 5. NETTOYAGE UK | HESA ----

# 6. NETTOYAGE EUROSTAT | TYPE D'INSTITUTION ----

# 7. NETTOYAGE EUROSTAT | MOBILITE PAR DIPLOME PRECEDENT ----

# 8. NETTOYAGE EUROSTAT | MOBILITE PAR CITOYENNETE ----

# 14. TABLE DE DICTIONNAIRE MINIMALE ----
data_dictionary <- tibble(
  object_name = c(
    "fr_effectifs_etab_clean",
    "fr_doctorat_etranger_clean",
    "uk_hesa_table1_clean",
    "eu_type_institution_clean",
    "eu_mobility_prev_diploma_clean",
    "eu_mobility_citizenship_clean",
    "eu_recent_mobility",
    "eu_recent_master"
  ),
  description = c(
    "France | effectifs par établissement, noms harmonisés, année si détectée",
    "France | doctorat, noms harmonisés, année si détectée",
    "UK | HESA table 1 nettoyée, année standardisée",
    "Eurostat | inscriptions par type d'institution",
    "Eurostat | mobilité selon pays du diplôme précédent",
    "Eurostat | mobilité selon citoyenneté",
    "Eurostat | base mobilité récente sur 3 ans",
    "Eurostat | base mobilité récente avec focus master si possible"
  )
)

write_csv(data_dictionary, file.path(path_processed, "data_dictionary_clean.csv"))

# 6. SAUVEGARDE ----
saveRDS(
  fr_effectifs_etudiants_etrangers_france_clean,
  file.path(path_processed, "fr_effectifs_etudiants_etrangers_france_clean.rds")
)

saveRDS(
  fr_effectifs_etab_clean,
  file.path(path_processed, "fr_effectifs_etab_clean.rds")
)

# 7. EXPORT CSV POUR VERIFICATION RAPIDE ----
write_csv(
  fr_effectifs_etudiants_etrangers_france_clean,
  file.path(path_processed, "fr_effectifs_etudiants_etrangers_france_clean.csv")
)

write_csv(
  fr_effectifs_etab_clean,
  file.path(path_processed, "fr_effectifs_etab_clean.csv")
)

# PISTES D'ANALYSE A AJOUTER ENSUITE
###############################################################################
# 1. Isoler autant que possible les niveaux master et licence
# 2. Distinguer mobilité diplômante et mobilité d'échange
# 3. Construire un focus Toulouse / Occitanie / France
# 4. Comparer TSE à des établissements cibles
# 5. Vérifier l'offre de formations en anglais au niveau master
# 6. Construire un focus Royaume-Uni post-Covid