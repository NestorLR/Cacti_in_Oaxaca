library(terra)

setwd("F:/Nestor_phD/Spatial_Analysis/Oax_Cactaceae/bin")

# Oaxaca
mex <- vect(
  "F:/Nestor_phD/Spatial_Analysis/layers/DivisionPolitica_Mx/dest_2010gw.shp"
)

oax <- mex[mex$ENTIDAD == "OAXACA", ]

# Variables climáticas
pca_path <- list.files(
  "../../layers/wc2.1_30s_bio/",
  pattern = "\\.tif$",
  full.names = TRUE
)

wc_2.1 <- rast(pca_path)

# Recortar
cropped_oax <- crop(wc_2.1, oax)
masked_oax  <- mask(cropped_oax, oax)

plot(masked_oax)

# Guardar
out <- "../layers/"
dir.create(out,
           recursive = TRUE,
           showWarnings = FALSE)

lapply(names(masked_oax), function(x){
  
  writeRaster(
    masked_oax[[x]],
    filename = file.path(out,
                         paste0(x, ".tif")),
    overwrite = TRUE,
    filetype = "GTiff",
    gdal = c("COMPRESS=LZW")
  )
  
})
