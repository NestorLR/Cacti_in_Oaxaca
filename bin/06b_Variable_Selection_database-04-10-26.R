library(terra)
library(dplyr)
library(corrplot)
library(usdm)
library(caret)

setwd("/media/delil/ADATA_Nestor_Blue/Nestor_phD/Spatial_Analysis/Oax_Cactaceae/bin")

############################################################
# 1. Load occurrence data
############################################################

cactus_envthin <- read.csv(
  "../data/env_data/from_database-04-10-26/database_04-10-26_envthin.csv"
)


pts <- vect(
  cactus_envthin,
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)


############################################################
# 2. Load climate variables
############################################################

clim_files <- list.files(
  "../data/layers/climate/",
  pattern = "\\.tif$",
  full.names = TRUE
)

clim <- rast(clim_files)


############################################################
# 3. Extract climate
############################################################

clim_vals <- extract(
  clim,
  pts
)

############################################################
# 4. Load topographic variables
############################################################

elev <- rast("../data/layers/DEM_OAX_COP30.tif")
slope <- rast("../data/layers/slope_30m.tif")
tpi <- rast("../data/layers/tpi_30m.tif")
roughness <- rast("../data/layers/roughness_30m.tif")

############################################################
# 5. Project points to DEM CRS
############################################################

pts_topo <- project(
  pts,
  crs(elev)
)

############################################################
# 6. Extract topography
############################################################

topo_vals <- data.frame(
  elev = extract(
    elev,
    pts_topo
  )[,2],
  
  slope = extract(
    slope,
    pts_topo
  )[,2],
  
  tpi = extract(
    tpi,
    pts_topo
  )[,2],

  
  roughness = extract(
    roughness,
    pts_topo
  )[,2]
  
  
)

############################################################
# 7. Merge all data
############################################################

env_data <- bind_cols(
  cactus_envthin,
  clim_vals %>%
    select(-ID),
  topo_vals
)

############################################################
# 8. Remove missing values
############################################################
names(env_data)
env_complete <- env_data %>%
  tidyr::drop_na(,15:31)

############################################################
# 9. Correlation matrix
############################################################

env_numeric <- env_complete %>%  select(, 15:31)


cor_matrix  <- cor(
  env_numeric,
  method = "spearman"
)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black"
)

############################################################
# 10. Remove high correlation
############################################################

high_cor <- findCorrelation(
  cor_matrix,
  cutoff = 0.8,
  names = TRUE
)

high_cor
selected_vars <- setdiff(
  names(env_numeric),
  high_cor
)

selected_vars


env_final_Pearson_0.8 <- env_complete %>%
  select(
    Species,
    lon,
    lat,
    Province,
    Genus, 
    Endemismo,
    all_of(selected_vars)
  )

############################################################
# 11. VIF
############################################################

vif_result <- vifstep(
  env_numeric,
  th = 10
)

selected_vars <-
  vif_result@results$Variables

selected_vars

############################################################
# 12. Final PCA dataset
############################################################

env_final_VIF_10 <- env_complete %>%
  select(
    Species,
    lon,
    lat,
    Province,
    Genus,
    Endemismo,
    all_of(selected_vars)
  )

############################################################
# 13. Save
############################################################

write.csv(
  env_final_VIF_10,
  "../data/env_data/from_database-04-10-26/EnvDataset_VIF10.csv",
  row.names = FALSE
)

write.csv(
  env_final_Pearson_0.8,
  "../data/env_data/from_database-04-10-26/EnvDataset_Pearson0.8.csv",
  row.names = FALSE
)
