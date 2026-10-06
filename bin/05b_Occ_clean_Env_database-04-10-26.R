library(terra)
library(dplyr)
library(stringr)

############################################################
# 1. Occurrence dataset
############################################################
#Dataset with no coordinates uncertained 
cactus <- read.csv(
  "../data/occ_data/Dataset_04_10_26.csv"
)

############################################################
# 2. Remove duplicates
# Keep coexistence of species
############################################################

cactus_clean <- cactus %>%
  filter(
    abs(lon - round(lon, 2)) > 0,
    abs(lat  - round(lat, 2)) > 0
  )


###########################################################
# Verificar qué se eliminó
###########################################################
occurrences_per_species <- cactus %>%
  count(
    Species,
    name = "n_occurrences"
  ) %>%
  arrange(
    desc(n_occurrences)
  )


occurrences_per_species_clean <- cactus_clean %>%
  count(
    Species,
    name = "n_occurrences"
  ) %>%
  arrange(
    desc(n_occurrences)
  )


setdiff(occurrences_per_species$Species, occurrences_per_species_clean$Species)


############################################################
# 2. Remove duplicates
# Keep coexistence of species
############################################################
cactus_df1 <- cactus_clean %>%
  distinct(
    Species,
    lon,
    lat,
    .keep_all = TRUE
  )

unique(cactus_df1$Species) # 113 spp, 3863 registros. 

############################################################
# 6. Rare vs common species
############################################################

sp_counts <- table(
  cactus_df1$Species
)

rare_species <- names(
  sp_counts[sp_counts < 10]
)

common_species <- names(
  sp_counts[sp_counts >= 10]
)

# rare species: keep all
rare_df <- cactus_df1 %>%
  filter(
    Species %in% rare_species
  )

# common species: thinning
common_df <- cactus_df1 %>%
  filter(
    Species %in% common_species
  )


############################################################
# 7. Environmental thinning
############################################################

bio1 <- rast(
  "../data/layers/climate/bio_01.tif"
)

pts <- vect(
  common_df,
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

cell_ids <- cellFromXY(
  bio1,
  crds(pts)
)

common_df$cell_id <- cell_ids

# Keep one occurrence
# per species per raster cell

cactus_common_envthin <- common_df %>%
  filter(!is.na(cell_id)) %>%
  distinct(
    Species,
    cell_id,
    .keep_all = TRUE
  )

############################################################
# 8. Merge rare + common
############################################################

cactus_envthin <- bind_rows(
  rare_df,
  cactus_common_envthin
)


cactus_envthin$Genus <- word(
  cactus_envthin$Species,
  1
)

cactus_envthin <- cactus_envthin %>% select(-c(cell_id))

write.csv(
  cactus_envthin,
  "../data/env_data/from_database-04-10-26/database_04-10-26_envthin.csv",
  row.names = FALSE
) #3575 registros. 

############################################################
# END
############################################################
