library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(scales)
library(readr)
library(dplyr)
library(eurostat)

data <- fromJSON(content(GET("https://www.data.gouv.fr/api/1/datasets/r/7dfbef75-4fd8-464c-875d-38c4e067969b"),
                         "text"))
cols_counts <- c(
  "total_mobiles", "mob_univ", "mob_ec_commerce", "mob_inge_univ",
  "mob_inge_hors_univ", "mob_ec_art", "mob_autres_formations",
  "mob_idf", "mob_auvergnerhonealpes", "mob_occitanie", "mob_grandest",
  "mob_hautsdefrance", "mob_autres_regions", "total_etrangers",
  "etr_univ", "etr_ec_commerce", "etr_inge_univ", "etr_inge_hors_univ",
  "etr_ec_art", "etr_autres_formations", "etr_idf",
  "etr_auvergnerhonealpes", "etr_occitanie", "etr_grandest",
  "etr_hautsdefrance", "etr_autres_regions")

data <- as.data.frame(data) %>% 
  clean_names() %>%
  mutate(across(
    matches("^(total_|mob_|etr_)"),
    ~ as.numeric(ifelse(.x == "<5", "2.5", .x))
  ), rentree = as.numeric(rentree)) %>% 
  filter(rentree > 2021)

# Top nationalités sur la période récente
top_pays <- data %>% 
  group_by(nationalite) %>% 
  summarise(total_mobiles_3ans = sum(total_mobiles, na.rm = TRUE)) %>% 
  arrange(desc(total_mobiles_3ans))
print(top_pays, n = 20)

# 6. Evolution des principaux pays
top10 <- top_pays %>% 
  slice_head(n = 10) %>% 
  pull(nationalite)

evol_top10 <- data %>% 
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

# 7. Répartition par type de formation
formations <- data %>%
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

# 8. Répartition régionale
regions <- data %>% 
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

# 9. Part de chaque filière pour chaque pays
profils_pays <- data %>% 
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

# 10. Croissance récente par pays
croissance <- data %>% 
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

stat <- read_delim("original_data/fr-esr-statistiques-sur-les-effectifs-d-etudiants-inscrits-par-etablissement-hcp.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

stat_clean <- stat %>%
  select(
    annee,
    `Année universitaire`,
    `Établissement`,
    `Type d'établissement`,
    `Région du siège de l’établissement`,
    `Cycle universitaire (cursus LMD) : L (1er cycle)`,
    `Cycle universitaire (cursus LMD) : M (2ème cycle)`,
    `Cycle universitaire (cursus LMD) : D (3ème cycle)`,
    `Grande discipline : Droit, sciences économiques, AES`,
    `Discipline : Sciences économiques, gestion`,
    `Nombre total d'étudiants inscrits (inscriptions principales et secondes) hors doubles inscriptions CPGE`,
    `Attractivité internationale : Etudiants de nationalité étrangère issus de système éducatif étranger`
  ) %>%
  mutate(
    part_internationaux =
      `Attractivité internationale : Etudiants de nationalité étrangère issus de système éducatif étranger` /
      `Nombre total d'étudiants inscrits (inscriptions principales et secondes) hors doubles inscriptions CPGE`
  )


region_analysis <- stat_clean %>%
  group_by(`Région du siège de l’établissement`) %>%
  summarise(
    total_students = sum(`Nombre total d'étudiants inscrits (inscriptions principales et secondes) hors doubles inscriptions CPGE`, na.rm = TRUE),
    total_internationaux = sum(`Attractivité internationale : Etudiants de nationalité étrangère issus de système éducatif étranger`, na.rm = TRUE)
  ) %>%
  mutate(part = total_internationaux / total_students) %>%
  arrange(desc(part))



mobile <- get_eurostat("educ_uoe_mobs02", time_format = "date")

top_origines <- mobile %>%
  filter(geo == "FR") %>%
  group_by(partner) %>%
  summarise(total = sum(values)) %>%
  arrange(desc(total))


part_france <- mobile %>%
  group_by(partner) %>%
  mutate(part_france = values[geo == "FR"] / sum(values))