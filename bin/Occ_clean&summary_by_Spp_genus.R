library(terra)
library(dplyr)
library(stringr)

setwd("/media/delil/ADATA_Nestor_Blue/Nestor_phD/Spatial_Analysis/Oax_Cactaceae/bin")

############################################################
# 1. Oaxaca polygon
############################################################

mex <- vect(
  "F:/Nestor_phD/Spatial_Analysis/layers/DivisionPolitica_Mx/dest_2010gw.shp"
)

oax <- mex[mex$ENTIDAD == "OAXACA", ]

plot(oax)

############################################################
# 1. Province polygones
############################################################

Neotropic <- vect(
  "F:/Nestor_phD/Spatial_Analysis/layers/bioprovincias/Neotropical-Morrone/NeotropicMap_Geo.shp"
)

plot(Neotropic)
names(Neotropic)


############################################################
# 2. Occurrence dataset
############################################################
#Dataset with no coordinates uncertained 
cactus <- read.csv(
  "../occ_data/Oax_cactus.csv"
)

# Convert to spatial points
cactus_vect <- vect(
  cactus,
  geom = c("Long", "Lat"),
  crs = "EPSG:4326"
)

############################################################
# 3. Keep only Oaxaca points
############################################################

inside <- is.related( cactus_vect, oax, relation = "intersects" )


cactus_oax <- cactus_vect[inside, ]

plot(oax)
points(cactus_oax,
       pch = 20,
       col = "red")

############################################################
# 4. Convert to dataframe
############################################################

cactus_df <- as.data.frame(
  cactus_oax,
  geom = "XY"
)

names(cactus_df)[
  names(cactus_df) == "x"
] <- "lon"

names(cactus_df)[
  names(cactus_df) == "y"
] <- "lat"

############################################################
# 5. Remove duplicates
# Keep coexistence of species
############################################################

cactus_df <- cactus_df %>%
  distinct(
    Species,
    lon,
    lat,
    .keep_all = TRUE
  )

############################################################
# 6. Rare vs common species
############################################################

sp_counts <- table(
  cactus_df$Species
)

rare_species <- names(
  sp_counts[sp_counts < 10]
)

common_species <- names(
  sp_counts[sp_counts >= 10]
)

# rare species: keep all
rare_df <- cactus_df %>%
  filter(
    Species %in% rare_species
  )

# common species: thinning
common_df <- cactus_df %>%
  filter(
    Species %in% common_species
  )

############################################################
# 7. Environmental thinning
############################################################

bio1 <- rast(
  "../layers/climate/bio_01.tif"
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



############################################################
# 8.1 Add biogeographic province
############################################################

# Convert final dataframe to spatial points
cactus_vect_final <- vect(
  cactus_envthin,
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

# Make sure CRS matches
Neotropic <- project(
  Neotropic,
  crs(cactus_vect_final)
)

# Spatial intersection
province_extract <- extract(
  Neotropic,
  cactus_vect_final
)

# Add province column
cactus_envthin$Province <-
  province_extract$Provincias

############################################################
# 9. Summary table
############################################################

summary_table <- cactus_df %>%
  count(
    Species,
    name = "original_records"
  ) %>%
  left_join(
    cactus_envthin %>%
      count(
        Species,
        name = "filtered_records"
      ),
    by = "Species"
  ) %>%
  mutate(
    filtered_records =
      ifelse(
        is.na(filtered_records),
        0,
        filtered_records
      )
  )

print(summary_table)

############################################################
# 10. Table: occurrences per species
############################################################

occurrences_per_species <- cactus_envthin %>%
  count(
    Species,
    name = "n_occurrences"
  ) %>%
  arrange(
    desc(n_occurrences)
  )

############################################################
# 11. Table: richness by genus
############################################################

cactus_envthin$Genus <- word(
  cactus_envthin$Species,
  1
)

richness_by_genus <- cactus_envthin %>%
  distinct(
    Genus,
    Species
  ) %>%
  count(
    Genus,
    name = "species_richness"
  ) %>%
  arrange(
    desc(species_richness)
  )

############################################################
# 12. Table: richness summary
############################################################

richness_summary <- tibble(
  n_genera =
    n_distinct(
      cactus_envthin$Genus
    ),
  
  n_species =
    n_distinct(
      cactus_envthin$Species
    ),
  
  total_occurrences =
    nrow(cactus_envthin)
)

print(richness_summary)

############################################################
# 12. Table: richness by provinces
############################################################

richness_by_province <- cactus_envthin %>%
  distinct(
    Province,
    Species
  ) %>%
  count(
    Province,
    name = "species_richness"
  ) %>%
  arrange(
    desc(species_richness)
  )

############################################################
# 12. Table: richness by genus by provinces
############################################################
richness_genus_province <- cactus_envthin %>%
  distinct(
    Province,
    Genus,
    Species
  ) %>%
  count(
    Province,
    Genus,
    name = "species_richness"
  ) %>%
  arrange(
    Province,
    desc(species_richness)
  )


############################################################
# 12. Table: occurances by provinces
############################################################

occurrences_by_province <- cactus_envthin %>%
  count(
    Province,
    name = "n_occurrences"
  ) %>%
  arrange(
    desc(n_occurrences)
  )

############################################################
# 13. Save outputs
############################################################

write.csv(
  cactus_envthin,
  "../occ_data/Oax_cactus_EnvThin.csv",
  row.names = FALSE
)

write.csv(
  summary_table,
  "../occ_data/Oax_cactus_EnvThin_summary.csv",
  row.names = FALSE
)

write.csv(
  occurrences_per_species,
  "../occ_data/Oax_occurrences_per_species.csv",
  row.names = FALSE
)

write.csv(
  richness_by_genus,
  "../occ_data/Oax_richness_by_genus.csv",
  row.names = FALSE
)

write.csv(
  richness_summary,
  "../occ_data/Oax_richness_summary.csv",
  row.names = FALSE
)

write.csv(
  richness_by_province,
  "../occ_data/Oax_richness_by_province.csv",
  row.names = FALSE
)

write.csv(
  richness_genus_province,
  "../occ_data/Oax_richness_genus_province.csv",
  row.names = FALSE
)

write.csv(
  occurrences_by_province,
  "../occ_data/Oax_occurrences_by_province.csv",
  row.names = FALSE
)

############################################################
# END
############################################################