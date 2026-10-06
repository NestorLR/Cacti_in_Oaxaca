library(terra)
library(dplyr)
library(stringr)

setwd("/media/delil/ADATA_Nestor_Blue/Nestor_phD/Spatial_Analysis/Oax_Cactaceae/bin")

############################################################
# 1. Oaxaca polygon
############################################################

mex <- vect(
  "/media/delil/ADATA_Nestor_Blue/Nestor_phD/Spatial_Analysis/layers/DivisionPolitica_Mx/dest_2010gw.shp"
)

oax <- mex[mex$ENTIDAD == "OAXACA", ]

plot(oax)

############################################################
# 1. Province polygones
############################################################

Neotropic <- vect(
  "/media/delil/ADATA_Nestor_Blue/Nestor_phD/Spatial_Analysis/layers/bioprovincias/Neotropical-Morrone/NeotropicMap_Geo.shp"
)

plot(Neotropic)
names(Neotropic)


# Provincias presentes en Oaxaca
prov_oax <- intersect(Neotropic, oax)
plot(prov_oax)

############################################################
# 2. Occurrence dataset
############################################################
#Dataset with no uncertain coordinates 
cactus <- read.csv(
  "../occ_data/Oax_cactus.csv"
)

#raw Dataset with uncertain coordinates 
cactus_raw <- read.csv(
  "../occ_data/Oax_original.csv"
)


metadata <- cactus_raw %>%
  select(c(GBIF.ID, coordinateUncertaintyInMeters, day, month, year, institutionCode, catalogNumber, identifiedBy))

#merge datasets
cactus_complete <- merge(cactus, metadata, by="GBIF.ID")

#### Datos daltantes
solo_df1 <- setdiff(cactus$GBIF.ID, metadata$GBIF.ID)
solo_df1 #corresponden a regiostros de tres especies. 

### Especies extras para añadir
extra_spp <- read.csv("../occ_data/extra_species_oax.csv")
unique(extra_spp$species)

extra <- extra_spp %>% 
  select(c(GBIF.ID, coordinateUncertaintyInMeters, day, month, year, institutionCode, 
           catalogNumber, identifiedBy, species, stateProvince, decimalLatitude, decimalLongitude))

# Cambiar nombre
extra <- extra %>%
  rename(Long = decimalLongitude) %>%
  rename(Lat = decimalLatitude) %>%
  rename(Species = species)
names(extra)[names(extra) == "stateProvince"] <- "State.Province"

# Reordenar columnas igual que cactus_complete
extra <- extra[, names(cactus_complete)]

##### Unir
cactus_complete2 <- rbind(cactus_complete, extra)

cactus_complete2$Lat <- as.numeric(cactus_complete2$Lat)
cactus_complete2$Long <- as.numeric(cactus_complete2$Long)

# Convert to spatial points
cactus_vect <- vect(
  cactus_complete2,
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
# 8.1 Add biogeographic province
############################################################

# Convert final dataframe to spatial points
cactus_vect_final <- vect(
  cactus_df,
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

# Make sure CRS matches
prov_oax <- project(
  prov_oax,
  crs(cactus_vect_final)
)

# Spatial intersection
province_extract <- extract(
  prov_oax,
  cactus_vect_final
)

# Add province column
cactus_df$Province <-
  province_extract$Provincias

unique(cactus_df$Province)
cactus_df$Province[is.na(cactus_df$Province)] <- 
  "Pacific Lowlands province"
unique(cactus_df$Species)



cactus_df2 <- cactus_df %>%
  mutate(
    Species = recode(
      Species,
      "Opuntia olmeca" = "Opuntia tehuacana",
      "Pachycereus fulviceps" = "Cephalocereus fulviceps",
      "Pachycereus hollianus" = "Lemaireocereus hollianus",
      "Pachycereus militaris" = "Mitrocereus militaris",
      "Weberocereus alliodorus" = "Selenicereus alliodorus"
    )
  )
species_list <- unique(cactus_df2$Species)

write.csv(species_list, "../occ_data/data_with_taxonomic_changes/Species_list.csv")
write.csv(cactus_df2, "../occ_data/data_with_taxonomic_changes/cactus_oax_complete.csv")

############################################################
# 9. Summary table
############################################################


summary_table <- cactus_df2 %>%
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

occurrences_per_species <- cactus_df2 %>%
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

cactus_df2$Genus <- word(
  cactus_df2$Species,
  1
)

richness_by_genus <- cactus_df2 %>%
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
      cactus_df2$Genus
    ),
  
  n_species =
    n_distinct(
      cactus_df2$Species
    ),
  
  total_occurrences =
    nrow(cactus_df2)
)

print(richness_summary)

############################################################
# 12. Table: richness by provinces
############################################################

richness_by_province <- cactus_df2 %>%
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
# 12. Table: genus by provinces
############################################################
genus_by_province <- cactus_df2 %>%
  distinct(
    Province,
    Genus
  ) %>%
  count(
    Province,
    name = "genus_richness"
  ) %>%
  arrange(
    desc(genus_richness)
  )


############################################################
# 12. Table: richness by genus by provinces
############################################################
richness_genus_province <- cactus_df2 %>%
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

occurrences_by_province <- cactus_df2 %>%
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
  "../occ_data/data_with_taxonomic_changes/Oax_cactus_EnvThin.csv",
  row.names = FALSE
)

write.csv(
  summary_table,
  "../occ_data/data_with_taxonomic_changes/Oax_cactus_EnvThin_rsummary.csv",
  row.names = FALSE
)

write.csv(
  occurrences_per_species,
  "../occ_data/data_with_taxonomic_changes/Oax_occurrences_per_species.csv",
  row.names = FALSE
)

write.csv(
  richness_by_genus,
  "../occ_data/data_with_taxonomic_changes/Oax_richness_by_genus.csv",
  row.names = FALSE
)

write.csv(
  richness_summary,
  "../occ_data/data_with_taxonomic_changes/Oax_richness_summary.csv",
  row.names = FALSE
)

write.csv(
  richness_by_province,
  "../occ_data/data_with_taxonomic_changes/Oax_richness_by_province.csv",
  row.names = FALSE
)

write.csv(
  richness_genus_province,
  "../occ_data/data_with_taxonomic_changes/Oax_richness_genus_province.csv",
  row.names = FALSE
)

write.csv(
  occurrences_by_province,
  "../occ_data/data_with_taxonomic_changes/Oax_occurrences_by_province.csv",
  row.names = FALSE
)


write.csv(
  genus_by_province,
  "../occ_data/data_with_taxonomic_changes/Oax_genus_by_province.csv",
  row.names = FALSE
)



############################################################
# END
############################################################
