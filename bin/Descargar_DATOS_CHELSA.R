############################################################
# Download monthly CHELSA humidity layers
############################################################

# Directorio de salida
out_dir <- "/media/delil/ADATA_Nestor_Blue/Nestor_phD/Spatial_Analysis/layers/CHELSA_hurs"

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Meses
months <- sprintf("%02d", 1:12)

# Construir URLs
urls <- paste0(
  "https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/climatologies/hurs/1981-2010/",
  "CHELSA_hurs_",
  months,
  "_1981-2010_V.2.1.tif"
)

# Nombres de archivos
dest_files <- file.path(
  out_dir,
  basename(urls)
)

############################################################
# Descargar
############################################################

for(i in seq_along(urls)) {
  
  cat(
    "Downloading:",
    basename(urls[i]),
    "\n"
  )
  
  download.file(
    url = urls[i],
    destfile = dest_files[i],
    mode = "wb"
  )
}

cat("Download complete!\n")