library(dplyr)
library(tidyr)
library(tibble)



setwd("/home/unam/Escritorio/cacti_in_Oaxaca/bin/")
## Cargar matriz 
cactus_df <- read.csv("../data/occ_data/Dataset_04_10_26.csv")

# Una fila por combinación especie-provincia
species_province <- cactus_df %>%
  distinct(Species, Province) %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = Province,
    values_from = present,
    values_fill = 0
  )

# Nombre exacto de la provincia: verifica antes con unique()
unique(cactus_df$Province)

pacific <- "Pacific Lowlands province"

# Especies presentes en las Tierras Bajas del Pacífico
pacific_sp <- species_province %>%
  filter(.data[[pacific]] == 1) %>%
  pull(Species)

# Número de provincias donde aparece cada especie
species_province <- species_province %>%
  mutate(
    n_provinces = rowSums(
      across(-Species, ~ replace_na(.x, 0))
    )
  )
# Identificar las columnas que corresponden a provincias
province_names <- setdiff(
  names(species_province),
  c("Species", "n_provinces")
)

# Preparar la matriz de presencia/ausencia
comm <- species_province %>%
  select(Species, all_of(province_names)) %>%
  distinct(Species, .keep_all = TRUE) %>%
  mutate(
    across(all_of(province_names), ~ replace_na(.x, 0))
  )

# Generar todas las parejas posibles
pairs <- combn(province_names, 2, simplify = FALSE)

# Calcular Jaccard para cada pareja
jaccard_results <- lapply(pairs, function(pair) {
  
  A <- comm[[pair[1]]] == 1
  B <- comm[[pair[2]]] == 1
  
  shared <- sum(A & B)
  union_n <- sum(A | B)
  
  tibble(
    Provincia_1 = pair[1],
    Provincia_2 = pair[2],
    Especies_compartidas = shared,
    Riqueza_1 = sum(A),
    Riqueza_2 = sum(B),
    Especies_union = union_n,
    Jaccard = ifelse(union_n > 0, shared / union_n, NA_real_)
  )
}) %>%
  bind_rows() %>%
  arrange(desc(Jaccard))

# Tabla con tres decimales
jaccard_results %>%
  mutate(Jaccard = round(Jaccard, 3))



tabla_cartel <- jaccard_results %>%
  transmute(
    `Provincia 1` = Provincia_1,
    `Provincia 2` = Provincia_2,
    `Especies compartidas` = Especies_compartidas,
    `Jaccard` = round(Jaccard, 3)
  )

tabla_cartel



write.csv(
  tabla_cartel,
  "../results/Tables/From_Database_04-10-26/Jaccard_pareado_provincias.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
