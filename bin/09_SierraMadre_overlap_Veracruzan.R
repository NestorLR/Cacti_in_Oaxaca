library(dplyr)




pca_data <- read.csv("../data/env_data/from_database-04-10-26/EnvDataset_VIF10.csv")
str(pca_data)

pca_data$bio_14 <- as.numeric(pca_data$bio_14)
pca_data$bio_18 <- as.numeric(pca_data$bio_18)
pca_data$bio_19 <- as.numeric(pca_data$bio_19)

#scale
env_scaled <- pca_data %>%
  select(-c(Species, lon, lat, Province, Genus, Endemismo))

pca <- prcomp(
  env_scaled,
  center = TRUE,
  scale. = TRUE
)


summary(pca)
scores <- as.data.frame(pca$x)

pca_results <- bind_cols(
  pca_data,
  scores
)

## Calcular una elpise del 95% del espacio ambiental por medio de la distancia de Mahalanobis
veracruz_pca <- pca_results %>%
  filter(
    Province == "Veracruzan province"
  ) %>%
  select(PC1, PC2)

centro_ver <- colMeans(veracruz_pca)

cov_ver <- cov(veracruz_pca)

cutoff <- qchisq(
  0.95,
  df = 2
)

### Identificamos los registros de la SIerra Madre del Sur que caen dentro de la elipse
sierra_pca <- pca_results %>%
  filter(
    Province == "Sierra Madre del Sur province"
  )

sierra_pca$mahal_ver <- mahalanobis(
  sierra_pca[, c("PC1", "PC2")],
  center = centro_ver,
  cov = cov_ver
)

sierra_overlap_ver <- sierra_pca %>%
  filter(
    mahal_ver <= cutoff
  )
nrow(sierra_overlap_ver)
nrow(sierra_pca)
100 * nrow(sierra_overlap_ver) / nrow(sierra_pca)


write.csv(sierra_overlap_ver, "../results/Tables/From_Database_04-10-26/SierraMadre_overlap_Ver.csv")
