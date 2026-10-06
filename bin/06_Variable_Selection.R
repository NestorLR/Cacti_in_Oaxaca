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

candidate_clim <- c(
  "bio_01",
  "bio_04",
  "bio_06",
  "bio_12",
  "bio_17"
)

clim <- clim[[candidate_clim]]

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

elev <- rast("../layers/DEM_OAX_COP30.tif")
#slope <- rast("../layers/slope_30m.tif")
tri <- rast("../layers/tri_30m.tif")

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
  
  #slope = extract(
  #  slope,
  #  pts_topo
  #)[,2],
  
  tri = extract(
    tri,
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
  tidyr::drop_na(
    bio_01,
    bio_04,
    bio_06,
    bio_12,
    bio_17,
    elev,
    tri
  )

############################################################
# 9. Correlation matrix
############################################################

candidate_vars <- c(
  "bio_01",
  "bio_04",
  "bio_06",
  "bio_12",
  "bio_17",
  "elev",
  "tri"
)

env_numeric <- env_complete %>%  select(all_of(candidate_vars))
#env_numeric <- env_complete %>%  select(-c(Species, lon,))

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

env_reduced <- env_numeric %>%
  select(
    -slope
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
    all_of(selected_vars)
  )

############################################################
# 13. Save
############################################################

write.csv(
  env_final_VIF_10,
  "../env_data/Oax_env_selected_variables_VIF10.csv",
  row.names = FALSE
)

write.csv(
  env_complete,
  "../env_data/Oax_env_selected_variables_complete.csv",
  row.names = FALSE
)

write.csv(
  cor_matrix,
  "../env_data/Oax_variable_correlation_Pearson_0.8.csv"
)






