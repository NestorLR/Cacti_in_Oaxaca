library(terra)
library(dplyr)


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


############################################################
# 1. Área total de Oaxaca (km²)
############################################################

# Reproyectar a un CRS proyectado en metros
oax_proj <- project(oax, "EPSG:6372")  # México ITRF2008 / LCC

# Área total
area_oax <- expanse(oax_proj, unit = "km")

############################################################
# 2. Provincias recortadas a Oaxaca
############################################################

# recorte preliminar
prov_crop <- crop(
  Neotropic,
  oax
)

# intersección espacial real
prov_oax <- terra::intersect(
  prov_crop,
  oax
)

class(prov_oax)
# Reproyectar
prov_oax_proj <- project(
  prov_oax,
  "EPSG:6372"
)

writeVector(
  prov_oax,
  "../layers/Provinces_Oaxaca.shp",
  overwrite = TRUE
)
############################################################
# 3. Calcular área por fragmento
############################################################

prov_oax_proj$area_km2 <- expanse(
  prov_oax_proj,
  unit = "km"
)
############################################################
# 4. Calcular área por provincia
############################################################
area_oax <- sum(expanse(oax_proj, unit = "km"))




############################################################
# 5. Sumar fragmentos de la misma provincia
############################################################
tabla_area <- prov_oax_proj %>%
  as.data.frame() %>%
  group_by(Provincias) %>%   # o Province, revisa el nombre real
  summarise(
    area_km2 = sum(area_km2, na.rm = TRUE)
  ) %>%
  mutate(
    porcentaje_oaxaca = (area_km2 / area_oax) * 100
  ) %>%
  arrange(desc(area_km2))

tabla_area
sum(tabla_area$porcentaje_oaxaca)


############################################################
# 5. Load richness tables
############################################################

richness_by_province <- read.csv(
  "../occ_data/data_with_taxonomic_changes/Oax_richness_by_province.csv"
)

genus_by_province <- read.csv(
  "../occ_data/data_with_taxonomic_changes/Oax_genus_by_province.csv"
)

############################################################
# 6. Merge area + richness
############################################################

province_area_richness <- tabla_area %>%
  left_join(
    richness_by_province,
    by = c(
      "Provincias" = "Province"
    )
  ) %>%
  left_join(
    genus_by_province,
    by = c(
      "Provincias" = "Province"
    )
  )

province_area_richness
write.csv(province_area_richness, "../env_data/Provinces_area_richnessw.csv")
############################################################
# 7. Correlation: Area vs species richness
############################################################

cor_species <- cor.test(
  province_area_richness$area_km2,
  province_area_richness$species_richness,
  method = "spearman",
  exact = FALSE
)

cor_species

############################################################
# 8. Correlation: Area vs genus richness
############################################################

cor_genus <- cor.test(
  province_area_richness$area_km2,
  province_area_richness$genus_richness,
  method = "spearman",
  exact = FALSE
)

cor_genus


library(ggplot2)

############################################################
# Species richness vs area
############################################################

ggplot(
  province_area_richness,
  aes(
    area_km2,
    species_richness
  )
) +
  geom_point(
    size = 4
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  geom_text(
    aes(label = Provincias),
    nudge_y = 3
  ) +
  theme_bw() +
  labs(
    x = expression(
      Area~(km^2)
    ),
    y = "Species richness"
  )

############################################################
# Genus richness vs area
############################################################

ggplot(
  province_area_richness,
  aes(
    area_km2,
    genus_richness
  )
) +
  geom_point(
    size = 4
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  geom_text(
    aes(label = Provincias),
    nudge_y = 1
  ) +
  theme_bw() +
  labs(
    x = expression(
      Area~(km^2)
    ),
    y = "Genus richness"
  )


############################################################
# 9. Species-area relationship
############################################################

sar_model <- lm(
  log10(species_richness) ~
    log10(area_km2),
  data =
    province_area_richness
)

summary(sar_model)

ggplot(
  province_area_richness,
  aes(
    log10(area_km2),
    log10(species_richness)
  )
) +
  geom_point(
    size = 4
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  geom_text(
    aes(label = Provincias),
    nudge_y = 0.03
  ) +
  theme_bw() +
  labs(
    x = expression(
      log[10](Area~km^2)
    ),
    y = expression(
      log[10](Species~richness)
    )
  )
