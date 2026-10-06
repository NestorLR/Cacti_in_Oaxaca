################## Obtención de datos ###################################

### Este script descargar datos de presencia del GBIF mediante R. 

library(rgbif) #https://docs.ropensci.org/rgbif/
#https://www.gbif.org/es/tool/81747/rgbif
library(bit64)
library(dplyr)

################## Set Working Directory
setwd("/media/delil/ADATA_Nestor_Blue/Nestor_phD/Spatial_Analysis/Oax_Cactaceae/bin/") 

###############################################################################
################### GBIF credentials ##########################################
## Crear una cuenta en GBIF, obtener user, password y API

# correr este comando una sola vez
#install.packages("usethis")
usethis::edit_r_environ()
#En la ventana emergente, añadir la información con tus datos
GBIF_USER = "nestorlopez"
GBIF_PWD = "Mammillaria"
GBIF_EMAIL = "nestorlopezruiz99@gmail.com"
#Guarda el archivo y reinicia R. 

##Nota: Ten en mente que en algún momento (en la publicación del artículo), te pedirán 
## el DOI de los datos. 



################################################################################
##################EXTRA: Download muliple taxa at the same time ################


#Esto crea una tabla con los nombres científicos que están escritos aqui 
long_checklist <- data.frame("Scientific name" = c('Selenicereus purpusii', 'Isolatocereus dumortieri', 
                                                   'Selenicereus vagans'))

gbif_taxon_keys <- long_checklist %>% 
  name_backbone_checklist() %>% # match to backbone 
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey) 

# download the data
occ_download(
  type="and",
  pred_in("taxonKey", gbif_taxon_keys),
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"),
  pred_or(  
    pred_lt("coordinateUncertaintyInMeters",1000),
    pred_isnull("coordinateUncertaintyInMeters")
  ),
  format = "SIMPLE_CSV"
)
occ_download_wait('0055754-260519110011954')

extra_spp_oax <- occ_download_get('0055754-260519110011954') %>%
  occ_download_import()

#END

