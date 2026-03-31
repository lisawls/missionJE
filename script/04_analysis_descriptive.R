###############################################################################
# PROJET TSE | Mobilité étudiante internationale
# Script : 04_analysis_descriptive.R
# Objet   : produire les premiers résultats descriptifs sur les flux récents
###############################################################################

# 0. PACKAGES ----
library(tidyverse)
library(readr)
library(scales)

# 1. CHEMINS ----
path_processed <- "data/processed"
path_outputs <- "outputs"

# 2. CHARGEMENT DES DONNEES ----
fr_effectifs_etudiants_etrangers_france <- readRDS(
  file.path(path_processed, "fr_effectifs_etudiants_etrangers_france_clean.rds")
)

fr_effectifs_etab <- readRDS(
  file.path(path_processed, "fr_effectifs_etab_clean.rds")
)


# 3. DESCRIPTIF GLOBAL DES FLUX ----
# 3.1 Top nationalités sur la période récente
# Top nationalités sur la période récente
top_pays <- fr_effectifs_etudiants_etrangers_france %>% 
  group_by(nationalite) %>% 
  summarise(total_mobiles_3ans = sum(total_mobiles, na.rm = TRUE)) %>% 
  arrange(desc(total_mobiles_3ans))
print(top_pays, n = 20)

# 3.2 Evolution des 10 premières nationalités
top10 <- top_pays %>% 
  slice_head(n = 10) %>% 
  pull(nationalite)

evol_top10 <- fr_effectifs_etudiants_etrangers_france %>% 
  filter(nationalite %in% top10) %>% 
  group_by(rentree, nationalite) %>% 
  summarise(total_mobiles = sum(total_mobiles, na.rm = TRUE), .groups = "drop")

ggplot(evol_top10, aes(x = rentree, y = total_mobiles, group = nationalite, color = nationalite)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Evolution récente des étudiants étrangers en mobilité internationale",
    x = "Rentrée universitaire",
    y = "Effectifs"
  ) +
  theme_minimal()

# 4. DESCRIPTIF PAR TYPE DE FORMATION ----
formations <- fr_effectifs_etudiants_etrangers_france %>%
  summarise(
    universite = sum(mob_univ, na.rm = TRUE),
    commerce = sum(mob_ec_commerce, na.rm = TRUE),
    inge_univ = sum(mob_inge_univ, na.rm = TRUE),
    inge_hors_univ = sum(mob_inge_hors_univ, na.rm = TRUE),
    ec_art = sum(mob_ec_art, na.rm = TRUE),
    autres = sum(mob_autres_formations, na.rm = TRUE)
  ) %>% 
  pivot_longer(everything(), names_to = "filiere", values_to = "effectif") %>% 
  mutate(part = effectif / sum(effectif))

ggplot(formations, aes(x = reorder(filiere, effectif), y = effectif)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Répartition des étudiants mobiles par type de formation",
    x = "",
    y = "Effectifs"
  ) +
  theme_minimal()

# 5. DESCRIPTIF TERRITORIAL ----
# 6.1 Répartition régionale agrégée
regions <- fr_effectifs_etudiants_etrangers_france %>% 
  summarise(
    idf = sum(mob_idf, na.rm = TRUE),
    auvergne_rhone_alpes = sum(mob_auvergnerhonealpes, na.rm = TRUE),
    occitanie = sum(mob_occitanie, na.rm = TRUE),
    grand_est=sum(mob_grandest, na.rm = TRUE),
    haut_de_france=sum(mob_hautsdefrance, na.rm = TRUE),
    autres_regions = sum(mob_autres_regions, na.rm = TRUE)
  ) %>% 
  pivot_longer(everything(), names_to = "region", values_to = "effectif") |>
  mutate(part = effectif / sum(effectif))

print(regions)

# 6.2 Comparaison des régions à partir des établissements
region_analysis <- fr_effectifs_etab %>%
  group_by(region_du_siege_de_l_etablissement) %>%
  summarise(
    total_students = sum(nombre_total_d_etudiants_inscrits_inscriptions_principales_et_secondes_hors_doubles_inscriptions_cpge, na.rm = TRUE),
    total_internationaux = sum(attractivite_internationale_etudiants_de_nationalite_etrangere_issus_de_systeme_educatif_etranger, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    part = if_else(total_students > 0, total_internationaux / total_students, NA_real_)
  ) %>%
  arrange(desc(part))

# 7. PROFILS PAR PAYS ----
profils_pays <- fr_effectifs_etudiants_etrangers_france %>% 
  group_by(nationalite) %>% 
  summarise(
    total_mobiles = sum(total_mobiles, na.rm = TRUE),
    part_univ = sum(mob_univ, na.rm = TRUE) / total_mobiles,
    part_commerce = sum(mob_ec_commerce, na.rm = TRUE) / total_mobiles,
    part_inge = (sum(mob_inge_univ, na.rm = TRUE) + sum(mob_inge_hors_univ, na.rm = TRUE)) / total_mobiles,
    part_art = sum(mob_ec_art, na.rm = TRUE) / total_mobiles,
    part_autres = sum(mob_autres_formations, na.rm = TRUE) / total_mobiles) %>% 
  filter(total_mobiles > 500) %>%
  arrange(desc(total_mobiles))

print(profils_pays, n = 30)

# 8. CROISSANCE RECENTE PAR PAYS ----
croissance <- fr_effectifs_etudiants_etrangers_france %>% 
  select(rentree, nationalite, total_mobiles) %>% 
  filter(total_mobiles > 500) %>%
  pivot_wider(names_from = rentree, values_from = total_mobiles) %>% 
  mutate(
    croissance_22_24 = ifelse(
      `2022` > 0,
      100*(`2024` - `2022`) / `2022`,
      NA
    )
  ) %>% 
  arrange(desc(croissance_22_24))

print(croissance, n = 30)


# 9. EXPORTS ----
write_csv(top_pays, file.path(path_outputs, "top_pays.csv"))
write_csv(region_analysis, file.path(path_outputs, "region_analysis.csv"))

ggsave(
  filename = file.path(path_outputs, "evol_top10.png"),
  plot = plot_evol_top10,
  width = 10,
  height = 6
)

ggsave(
  filename = file.path(path_outputs, "repartition_formations.png"),
  plot = plot_formations,
  width = 8,
  height = 6
)