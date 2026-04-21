###############################################################################
# PROJET TSE | Mobilité étudiante internationale
# Script : 03_analysis_descriptive.R
# Objet   : produire les premiers résultats descriptifs sur les flux récents
###############################################################################

# 0. PACKAGES ----
library(tidyverse)
library(readr)
library(scales)
library(writexl)

# 1. CHEMINS ----
path_outputs <- "output"
path_clean <- "data/data_clean"

# 2. FONCTIONS UTILITAIRES ----

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
      filter(decomposition_des_universites_a_statuts_experimentaux == "Université Paris Dauphine - PSL",
             secteur_disciplinaire == "Sciences économiques") %>%
      summarise_mobilite(cursus) %>% mutate(label = "Université Paris Dauphine - PSL (Sciences Economiques)"),
    
    data %>% filter(etablissement == "Université Paris 1 - Panthéon Sorbonne",
             secteur_disciplinaire == "Sciences économiques") %>%
      summarise_mobilite(cursus) %>% mutate(label = "Université Paris 1 - Panthéon Sorbonne (Sciences Economiques)")
  ) %>% relocate(label)}


# 3. CHARGEMENT DES DONNEES ----
fr_effectifs_etudiants_etrangers_france <- read_csv(file.path(path_clean, "fr_effectifs_etudiants_etrangers_france_clean.csv"))
fr_effectifs_etablissement_2022 <- read_csv(file.path(path_clean, "fr_effectifs_etablissement_2022.csv"))
fr_effectifs_etablissement_2023 <- read_csv(file.path(path_clean, "fr_effectifs_etablissement_2023.csv"))
fr_effectifs_etablissement_2024 <- read_csv(file.path(path_clean, "fr_effectifs_etablissement_2024.csv"))
# uk_hesa_all <- read_csv(file.path(path_clean, "uk_hesa_clean.csv"))
# eu_type_institution <- read_csv(file.path(path_clean, "eu_type_institution_clean.csv"))
# eu_mobility_prev_diploma <- read_csv(file.path(path_clean, "eu_mobility_prev_diploma_clean.csv"))
# eu_mobility_citizenship <- read_csv(file.path(path_clean, "eu_mobility_citizenship_clean.csv"))
unesco <- read_csv(file.path(path_clean, "unesco_clean.csv"))


# 4. DESCRIPTIF GLOBAL DES FLUX ----

# 4.1 Classement des nationalités par effectif moyen sur les 3 dernières rentrées
top_pays <- fr_effectifs_etudiants_etrangers_france %>% 
  group_by(nationalite) %>% 
  summarise(moy_mobiles = mean(total_mobiles, na.rm = TRUE)) %>% 
  arrange(desc(moy_mobiles))

# 4.2 Part de chaque nationalité dans le total des étudiants mobiles en 2024
top_pays_2024_percent <- fr_effectifs_etudiants_etrangers_france %>% 
  filter(rentree == "2024") %>% 
  mutate(part = total_mobiles / sum(total_mobiles, na.rm = TRUE)) %>% 
  select(nationalite, part) %>%
  arrange(desc(part)) %>%
  head(n = 10)

# 4.3 Evolution de la part des top 10 nationalités (2024) sur toutes les rentrées
evol_top10 <- fr_effectifs_etudiants_etrangers_france %>%
  group_by(rentree) %>%
  mutate(part = total_mobiles / sum(total_mobiles, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(nationalite %in% (top_pays_2024_percent %>% 
                             pull(nationalite)))

labels_evol <- evol_top10 %>%
  filter(rentree == min(rentree) | rentree == max(rentree))

plot_evol_top10 <- ggplot(evol_top10, aes(x = rentree, y = part, group = nationalite, color = nationalite)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_text(
    data = labels_evol,
    aes(label = scales::percent(part, accuracy = 0.1)),
    vjust = -0.8,
    size = 3,
    show.legend = FALSE
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Paired") +
  labs(
    title = "Evolution de la part des top 10 nationalités (en 2024-2025) dans la mobilité entrante",
    subtitle = "Part calculée sur l'ensemble des étudiants étrangers en mobilité",
    x = "Rentrée universitaire",
    y = "Part (%)",
    color = ""
  ) +
  theme_minimal()

# 4.4 Top 10 des nationalités en écoles d'ingénieurs et de commerce en 2024 avec comparaison université pour ces mêmes pays
top_pays_univ_vs_com_inge_2024 <- fr_effectifs_etudiants_etrangers_france %>% 
  filter(rentree == "2024") %>% 
  mutate(mob_com_inge = mob_ec_commerce + mob_inge_hors_univ) %>% 
  arrange(desc(mob_com_inge)) %>%
  slice_head(n = 10) %>% 
  select(nationalite, mob_univ, mob_com_inge) %>%
  pivot_longer(
    cols = c(mob_univ, mob_com_inge),
    names_to = "type",
    values_to = "effectif") %>%
  mutate(type = recode(type,
                       "mob_univ" = "Université",
                       "mob_com_inge" = "Ecole d'ingénieur hors université et école de commerce"))

plot_effectifs_univ_vs_com_inge_2024 <- ggplot(top_pays_univ_vs_com_inge_2024, aes(x = nationalite, y = effectif, fill = type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::number(effectif, big.mark = " ")),
            position = position_dodge(width = 0.9),
            hjust = -0.1,
            size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 des nationalités (école de commerce et ingénieur) en 2024-2025",
    subtitle = "Comparaison Université vs école d'ingénieur hors université et école de commerce",
    x = "Nationalité",
    y = "Effectif",
    fill = ""
  ) +
  theme_minimal()

# 4.5 Part des mêmes nationalités dans le total université et ingé+commerce
top_pays_univ_vs_com_inge_2024_percent <- fr_effectifs_etudiants_etrangers_france %>% 
  filter(rentree == "2024") %>% 
  mutate(
    mob_com_inge = mob_ec_commerce + mob_inge_hors_univ,
    part_univ = mob_univ / sum(mob_univ, na.rm = TRUE),
    part_com_inge = mob_com_inge / sum(mob_com_inge, na.rm = TRUE)
  ) %>% 
  arrange(desc(part_com_inge)) %>%
  slice_head(n = 10) %>%
  select(nationalite, part_univ, part_com_inge) %>%
  pivot_longer(
    cols = c(part_univ, part_com_inge),
    names_to = "type",
    values_to = "part") %>%
  mutate(type = recode(type,
                       "part_univ" = "Université",
                       "part_com_inge" = "Ecole d'ingénieur hors université et école de commerce"))

plot_parts_univ_vs_com_inge_2024 <- ggplot(top_pays_univ_vs_com_inge_2024_percent, aes(x = reorder(nationalite, part), y = part, fill = type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::percent(part, accuracy = 0.1)),
            position = position_dodge(width = 0.9),
            hjust = -0.1,
            size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Part des nationalités dans les étudiants en mobilité en 2024-2025",
    subtitle = "Université vs école d'ingénieur hors université et école de commerce",
    x = "Nationalité",
    y = "Part (%)",
    fill = "") +
  theme_minimal()

# 5. DESCRIPTIF PAR TYPE DE FORMATION ----
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

# 6. DESCRIPTIF TERRITORIAL ----
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

# 7. DESCRIPTIF UNESCO ----
unesco %>%
  arrange(desc(outbound_mobility_ratio)) %>% slice_head(n = 15)


# 8. ANALYSE PAR ETABLISSEMENT ----
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


# 9. EXPORTS ----

# Graphiques
ggsave(file.path(path_outputs, "evol_top10_parts.png"),              plot_evol_top10,                      width = 12, height = 7, bg = "white")
ggsave(file.path(path_outputs, "effectifs_univ_vs_com_inge_2024.png"), plot_effectifs_univ_vs_com_inge_2024, width = 10, height = 6, bg = "white")
ggsave(file.path(path_outputs, "parts_univ_vs_com_inge_2024.png"),   plot_parts_univ_vs_com_inge_2024,     width = 10, height = 6, bg = "white")

# Tableaux Excel
write_xlsx(
  list(
    "top_pays_moy_mobiles"          = top_pays,
    "top_pays_parts_2024"           = top_pays_2024_percent,
    "top10_effectifs_univ_com_inge" = top_pays_univ_vs_com_inge_2024,
    "top10_parts_univ_com_inge"     = top_pays_univ_vs_com_inge_2024_percent
  ),
  file.path(path_outputs, "descriptif_nationalites_2024.xlsx")
)

write_xlsx(etablissement_all, file.path(path_outputs, "mobilite_internationale_etablissements.xlsx"))