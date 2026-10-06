library(terra)
library(dplyr)
library(stringr)

setwd("/media/delil/ADATA_Nestor_Blue/Nestor_phD/Spatial_Analysis/Oax_Cactaceae/bin")


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

cactus_df <- cactus %>%
  distinct(
    Species,
    lon,
    lat,
    .keep_all = TRUE
  )

unique(cactus_df$Species)

cactus_df2 <- cactus_df %>% 
  filter(
    coordinateUncertaintyInMeters < 1000 |
      is.na(coordinateUncertaintyInMeters)
  )
unique(cactus_df2$Species)

setdiff(
  unique(cactus_df$Species),
  unique(cactus_df2$Species)
)

############################################################
# 6. Rare vs common species
############################################################

sp_counts <- table(
  cactus_df2$Species
)

rare_species <- names(
  sp_counts[sp_counts < 10]
)

common_species <- names(
  sp_counts[sp_counts >= 10]
)

# rare species: keep all
rare_df <- cactus_df2 %>%
  filter(
    Species %in% rare_species
  )

# common species: thinning
common_df <- cactus_df2 %>%
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


cactus_envthin$Genus <- word(
  cactus_envthin$Species,
  1
)

cactus_envthin <- cactus_envthin %>% select(-c(cell_id, X))

write.csv(
  cactus_envthin,
  "../env_data/cactus_oax_envthin.csv",
  row.names = FALSE
)

############################################################
# END
############################################################