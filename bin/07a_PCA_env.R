library(factoextra)
library(ggplot2)
library(dplyr)
library(vegan)
library(pairwiseAdonis)
library(ggrepel)

setwd("/media/delil/ADATA_Nestor_Blue/Nestor_phD/Spatial_Analysis/Oax_Cactaceae/bin")


################################################################################
##### Variables VIF<10

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
write.csv(pca_results, "../results/Tables/From_Database_04-10-26/PCA_results_all.csv")
### loadings de las variables
round(
  pca$rotation,
  2
)

loadings <- as.data.frame(pca$rotation)

loadings$Variable <- rownames(loadings)
arrow_scale <- 10
loadings <- loadings %>%
  mutate(
    PC1_arrow = PC1 * arrow_scale,
    PC2_arrow = PC2 * arrow_scale
  )

loadings$Variable_label <- gsub(
  "bio_",
  "BIO",
  loadings$Variable
)



contrib_pct <- sweep(
  pca$rotation^2,
  2,
  colSums(pca$rotation^2),
  "/"
) * 100

write.csv(contrib_pct, "../results/Tables/From_Database_04-10-26/Loadings_PCA.csv")



# Porcentaje de varianza explicada
var_exp <- (pca$sdev^2 / sum(pca$sdev^2)) * 100
var_exp
pc1_label <- paste0("PC1 (", round(var_exp[1], 2), "%)")
pc2_label <- paste0("PC2 (", round(var_exp[2], 2), "%)")

# Tabla egigenvalue + variance
pca_variance <- data.frame(
  PC = paste0("PC", seq_along(var_exp)),
  Eigenvalue = pca$sdev^2,
  Variance = var_exp,
  Cumulative = cumsum(var_exp)
)

# Tabla para destacar los registros de Chiapas
small_province <- pca_results %>%
  filter(
    Province ==
      "Chiapas Highlands province"
  )

### PCA por provincia con los loadigs

ggplot(
  pca_results,
  aes(PC1, PC2, color = Province)
) +
  # Vectores de los loadings
  geom_segment(
    data = loadings,
    aes(
      x = 0,
      y = 0,
      xend = PC1_arrow,
      yend = PC2_arrow
    ),
    inherit.aes = FALSE,
    arrow = arrow(
      length = unit(0.5, "cm")
    ),
    linewidth = 0.5,
    color = "dimgrey"
  ) +
  # Elipses de las provincias
  stat_ellipse(
    linewidth = 0.5
  ) +
  
  # Observaciones
  geom_point(
    alpha = 0.25,
    size = 0.8
  ) +
  geom_text_repel(
    data = loadings,
    aes(
      x = PC1_arrow,
      y = PC2_arrow,
      label = Variable_label
    ),
    inherit.aes = FALSE,
    color = "dimgrey",
    size = 4,
    nudge_x = 0.05,
    nudge_y = 0.05
  ) +
  scale_color_manual(
    values = c(
      "Sierra Madre del Sur province" = "#6f9e43",
      "Balsas Basin province" = "#FFA500",
      "Pacific Lowlands province" = "#5fafc0",
      "Veracruzan province" = "#483D8B",
      "Chiapas Highlands province" = "#FF4500"
    ),
    labels = c(
      "Sierra Madre del Sur province" = "Sierra Madre del Sur",
      "Balsas Basin province" = "Depresión del Balsas",
      "Pacific Lowlands province" = "Tierras Bajas del Pacífico",
      "Veracruzan province" = "Provincia Veracruzana",
      "Chiapas Highlands province" = "Altos de Chiapas"
    )
  ) +
  # resaltar provincia de Altos de Chiapas
  geom_point(
    data = small_province,
    shape = 21,
    fill = "#FF4500",
    color = "black",
    size = 1.0,
    stroke = 0.1
  ) +
  theme_bw() +
  labs(
    x = pc1_label,
    y = pc2_label,
    color = "Provincias"
  ) +
  theme(
    legend.position = c(0.86, 0.88),
    legend.background = element_rect(
      fill = "white",
      color = "dimgrey"
    ),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm")
  )

ggsave(
  "../results/figures/from_database-04-10-26/PCA_provincias_complete.png",
  plot = last_plot(),
  width = 8,
  height = 6,
  dpi = 600
)

ggsave(
  "../results/figures/from_database-04-10-26/PCA_provincias_complete.pdf",
  plot = last_plot(),
  width = 8,
  height = 6,
  dpi = 600
)

#### Prueva estadística: PERMANOVA

env_test <- pca_results %>%
  select(
    bio_07,
    bio_11,
    bio_14,
    bio_15,
    bio_18,
    bio_19,
    tpi,
    slope
  )

province <- pca_results$Province

permanova <- adonis2(
  env_test ~ province,
  method = "euclidean",
  permutations = 999
)

permanova


## Prueba estadística: DIspersión --> ¿Las diferencias detectadas en PERMANOVA se deben a que la dispersión de mis datos es demasiada?
#Prueba de homogeneidad multivariada
dist_env <- dist(
  scale(env_test)
)

disp <- betadisper(
  dist_env,
  province
)

anova(disp)

permutest(disp)

#### Comparación por pares
pairwise.adonis2(
  env_test,
  factors = province,
  perm = 999
)