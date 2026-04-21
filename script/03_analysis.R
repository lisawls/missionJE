###############################################################################
# PROJET TSE | Mobilité étudiante internationale
# Script : 04_analysis_descriptive.R
# Objet   : produire les premiers résultats descriptifs sur les flux récents
###############################################################################

# 0. PACKAGES ----
library(tidyverse)
library(readr)
library(scales)
library(writexl)

# 1. CHEMINS ----
path_outputs <- "outputs"
path_clean <- "data/data_clean"

# 2. CHARGEMENT DES DONNEES ----
fr_effectifs_etudiants_etrangers_france <- read_csv(file.path(path_clean, "fr_effectifs_etudiants_etrangers_france_clean.csv"))
fr_effectifs_etablissement_2022 <- read_csv(file.path(path_clean, "fr_effectifs_etablissement_2022.csv"))
fr_effectifs_etablissement_2023 <- read_csv(file.path(path_clean, "fr_effectifs_etablissement_2023.csv"))
fr_effectifs_etablissement_2024 <- read_csv(file.path(path_clean, "fr_effectifs_etablissement_2024.csv"))


# uk_hesa_all <- read_csv(file.path(path_clean, "uk_hesa_clean.csv"))
# eu_type_institution <- read_csv(file.path(path_clean, "eu_type_institution_clean.csv"))
# eu_mobility_prev_diploma <- read_csv(file.path(path_clean, "eu_mobility_prev_diploma_clean.csv"))
# eu_mobility_citizenship <- read_csv(file.path(path_clean, "eu_mobility_citizenship_clean.csv"))
unesco <- read_csv(file.path(path_clean, "unesco_clean.csv"))


# 3. DESCRIPTIF GLOBAL DES FLUX ----
# 3.1 Top nationalités sur la période récente
# Top nationalités sur la période récente
top_pays <- fr_effectifs_etudiants_etrangers_france_clean %>% 
  group_by(nationalite) %>% 
  summarise(total_mobiles_3ans = sum(total_mobiles, na.rm = TRUE)) %>% 
  arrange(desc(total_mobiles_3ans))

top_pays_2024_percent <- fr_effectifs_etudiants_etrangers_france_clean %>% 
  filter(rentree == "2024") %>% 
  mutate(total_mobiles_general = sum(total_mobiles, na.rm = TRUE),
          part = total_mobiles / total_mobiles_general) %>% 
  select(nationalite, part) %>%
  arrange(desc(part)) %>%
  head(n = 10)



top_pays_univ_vs_com_inge_2024 <- fr_effectifs_etudiants_etrangers_france_clean %>% 
  filter(rentree == "2024") %>% 
  group_by(nationalite) %>% 
  mutate(mob_com_inge = mob_ec_commerce + mob_inge_hors_univ) %>% 
  select(nationalite, mob_univ,mob_com_inge) %>%
  arrange(desc(mob_com_inge)) %>%
  head(n = 10) %>% 
  pivot_longer(
    cols = c(mob_univ, mob_com_inge),
    names_to = "type",
    values_to = "effectif") %>%
  mutate(
    type = recode(type,
      "mob_univ" = "Université",
      "mob_com_inge" = "Ecole d'ingénieur hors université et école de commerce"))

ggplot(top_pays_univ_vs_com_inge_2024, aes(x = nationalite, y = effectif, fill = type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Top 10 des nationalités (école de commerce et ingénieur) en 2024-2025",
    subtitle = "Comparaison Université vs Ingé hors université + Commerce",
    x = "Nationalité",
    y = "Effectif",
    fill = ""
  ) +
  theme_minimal()

top_pays_univ_vs_com_inge_2024_percent <- fr_effectifs_etudiants_etrangers_france_clean %>% 
  filter(rentree == "2024") %>% 
  mutate(mob_com_inge = mob_ec_commerce + mob_inge_hors_univ,
         total_mob_univ = sum(mob_univ, na.rm = TRUE),
         part_univ = mob_univ / total_mob_univ,
         total_mob_com_inge = sum(mob_com_inge, na.rm = TRUE),
         part_com_inge = mob_com_inge / total_mob_com_inge) %>% 
  arrange(desc(part_com_inge)) %>%
  head(10) %>%
  select(nationalite, part_univ, part_com_inge) %>%
  pivot_longer(
    cols = c(part_univ, part_com_inge),
    names_to = "type",
    values_to = "part") %>%
  mutate(
    type = recode(type,
                  "part_univ" = "Université",
                  "part_com_inge" = "Ingé + Commerce"))

ggplot(top_pays_univ_vs_com_inge_2024_percent, aes(x = reorder(nationalite, part), y = part, fill = type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Part des nationalités dans les étudiants en mobilité en 2024-2025",
    subtitle = "Université vs Ingé + Commerce",
    x = "Nationalité",
    y = "Part (%)",
    fill = ""
  ) +
  theme_minimal()

top_zone_geo <- fr_effectifs_etudiants_etrangers_france %>% 
  group_by(zone_geographique_d_origine) %>% 
  summarise(total_mobiles_3ans = sum(total_mobiles, na.rm = TRUE)) %>% 
  arrange(desc(total_mobiles_3ans))

top_pays_com_inge <- fr_effectifs_etudiants_etrangers_france_clean %>%
  group_by(nationalite) %>%
  summarise(
    mob_commerce  = sum(mob_ec_commerce,    na.rm = TRUE),
    mob_inge      = sum(mob_inge_hors_univ, na.rm = TRUE),
    com_inge = mob_commerce + mob_inge,
    .groups = "drop")

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



unesco %>%
  arrange(desc(outbound_mobility_ratio)) %>%
  slice_head(n = 15)


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




# 
summarise_mobilite <- function(data, cursus = NULL) {
  if (!is.null(cursus)) data <- filter(data, cursus_lmd == cursus)
  data %>%
    filter(mobilite_internationale == "Etudiants étrangers en mobilité internationale") %>%
    summarise(
      effectif_total = sum(nombre_detudiants_inscrits_inscriptions_principales_hors_etudiants_inscrits_en_parallele_en_cpge, na.rm = TRUE),
      nb_ue27        = sum(nombre_detudiants_inscrits_issus_des_pays_membres_de_l_ue27_hors_etudiants_inscrits_en_parallele_en_cpge, na.rm = TRUE),
      nb_ocde        = sum(nombre_detudiants_inscrits_issus_des_pays_membres_de_l_ocde_hors_etudiants_inscrits_en_parallele_en_cpge, na.rm = TRUE),
      nb_brics       = sum(nombre_detudiants_inscrits_issus_des_pays_composant_le_brics_hors_etudiants_inscrits_en_parallele_en_cpge, na.rm = TRUE),
      nb_bologne     = sum(nombre_detudiants_inscrits_issus_des_pays_engages_dans_le_processus_de_bologne_hors_etudiants_inscrits_en_parallele_en_cpge, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      part_ue27    = nb_ue27    / effectif_total,
      part_ocde    = nb_ocde    / effectif_total,
      part_brics   = nb_brics   / effectif_total,
      part_bologne = nb_bologne / effectif_total
    )
}

make_etablissements <- function(data, cursus = NULL) {
  bind_rows(
    data %>%
      filter(type_detablissement == "Grand établissement", etablissement == "Sciences Po") %>%
      summarise_mobilite(cursus) %>% mutate(label = "Sciences Po"),
    
    data %>%
      filter(decomposition_des_universites_a_statuts_experimentaux == "Toulouse School of Economics") %>%
      summarise_mobilite(cursus) %>% mutate(label = "Toulouse School of Economics"),
    
    data %>%
      filter(decomposition_des_universites_a_statuts_experimentaux == "Université Paris Dauphine - PSL", secteur_disciplinaire == "Sciences économiques") %>%
      summarise_mobilite(cursus) %>% mutate(label = "Université Paris Dauphine - PSL (Sciences Economiques)"),
    
    data %>%
      filter(etablissement == "Université Paris 1 - Panthéon Sorbonne", secteur_disciplinaire == "Sciences économiques") %>%
      summarise_mobilite(cursus) %>% mutate(label = "Université Paris 1 - Panthéon Sorbonne  (Sciences Economiques)")
  ) %>%
    relocate(label)
}

annees <- list(
  "2022-2023" = fr_effectifs_etablissement_2022,
  "2023-2024" = fr_effectifs_etablissement_2023,
  "2024-2025" = fr_effectifs_etablissement_2024)


etablissement_all <- annees %>%
  imap(~ bind_rows(
    make_etablissements(.x)              %>% mutate(annee = .y, cursus = "Total"),
    make_etablissements(.x, cursus = "M") %>% mutate(annee = .y, cursus = "Master")
  )) %>%
  bind_rows() %>%
  relocate(annee, label, cursus)  %>%
  mutate(across(starts_with("part_"), ~ round(. * 100, 2)))

write_xlsx(etablissement_all, "output/mobilite_internationale_etablissements.xlsx")