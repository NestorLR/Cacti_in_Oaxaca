library(terra)
library(geodata)
library(elevatr)
library(sf)

setwd("F:/Nestor_phD/Spatial_Analysis/Oax_Cactaceae/bin")

#Shape de Oaxaca
mex <- vect("F:/Nestor_phD/Spatial_Analysis/layers/DivisionPolitica_Mx/dest_2010gw.shp")
names(mex)
plot(mex) #esto hace el gráfico 
unique(mex$ENTIDAD)
oax <- mex[mex$ENTIDAD == "OAXACA", ]
plot(oax)
# convertir a sf
oax_sf <- st_as_sf(oax)

## DEM a 30 m
dem30 <- rast(
  "F:/Nestor_phD/Spatial_Analysis/Oax_Cactaceae/layers/DEM_OAXACA_COP30.tif"
)

plot(dem30)
res(dem30) #[1] 0.0002777778 0.0002777778, ca. 30 m 

#Recortar al shape de Oaxaca
# reproyectar shape al CRS del DEM
oax <- project(oax, crs(dem30))

dem30_oax <- crop(dem30, oax)
dem30_oax <- mask(dem30_oax, oax)

plot(dem30_oax)
res(dem30_oax) #[1] 0.0002777778 0.0002777778, ca. 30 m 
crs(dem30_oax)

## Proyectar a metros 
dem_proj <- project(
  dem30_oax,
  "EPSG:6372"
)


# Derivar vairables topográficas
slope <- terrain(
  dem_proj,
  v = "slope",
  unit = "degrees"
)

tri <- terrain(
  dem_proj,
  v = "TRI"
)

rough <- terrain(
  dem_proj,
  v = "roughness"
)

aspect <- terrain(dem_proj, v = "aspect",  unit = "degrees")

tpi    <- terrain(dem_proj, v = "TPI")



crs(dem_proj)
crs(slope)

# 6) Guardar GEOTiff
out <- "../layers/"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# DEM
writeRaster(
  dem_proj,
  paste0(out, "DEM_OAX_COP30.tif"),
  overwrite = TRUE,
  filetype = "GTiff",
  gdal = c("COMPRESS=LZW")
)

# slope
writeRaster(
  slope,
  paste0(out, "slope_30m.tif"),
  overwrite = TRUE,
  filetype = "GTiff",
  gdal = c("COMPRESS=LZW")
)

# aspect
writeRaster(
  aspect,
  paste0(out, "aspect_30m.tif"),
  overwrite = TRUE,
  filetype = "GTiff",
  gdal = c("COMPRESS=LZW")
)

# TRI
writeRaster(
  tri,
  paste0(out, "tri_30m.tif"),
  overwrite = TRUE,
  filetype = "GTiff",
  gdal = c("COMPRESS=LZW")
)

# TPI
writeRaster(
  tpi,
  paste0(out, "tpi_30m.tif"),
  overwrite = TRUE,
  filetype = "GTiff",
  gdal = c("COMPRESS=LZW")
)

# roughness
writeRaster(
  rough,
  paste0(out, "roughness_30m.tif"),
  overwrite = TRUE,
  filetype = "GTiff",
  gdal = c("COMPRESS=LZW")
)
