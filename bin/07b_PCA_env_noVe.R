library(factoextra)
library(ggplot2)
library(dplyr)
library(vegan)
library(pairwiseAdonis)
library(ggrepel)

################################################################################
##### PCA sin veracruz
pca_data <- read.csv("../data/env_data/from_database-04-10-26/EnvDataset_VIF10.csv")
str(pca_data)
names(pca_data)

pca_data_noVe <- pca_data %>%
  filter(
    Province != "Veracruzan province"
  )


#scale
env_scaled_noVe <- pca_data_noVe %>%
  select(c(bio_07, bio_11, bio_14, bio_15, bio_18, bio_19, slope, tpi))

pca_noVe <- prcomp(
  env_scaled_noVe,
  center = TRUE,
  scale. = TRUE
)

summary(pca_noVe)
scores <- as.data.frame(pca_noVe$x)

pca_results_noVe <- bind_cols(
  pca_data_noVe,
  scores
)


### loadings de las variables
round(
  pca_noVe$rotation,
  2
)

loadings_noVe <- as.data.frame(pca_noVe$rotation)

loadings_noVe$Variable <- rownames(loadings_noVe)
arrow_scale <- 10
loadings_noVe <- loadings_noVe %>%
  mutate(
    PC1_arrow = PC1 * arrow_scale,
    PC2_arrow = PC2 * arrow_scale
  )

loadings_noVe$Variable_label <- gsub(
  "bio_",
  "BIO",
  loadings_noVe$Variable
)



contrib_pct_noVe <- sweep(
  pca_noVe$rotation^2,
  2,
  colSums(pca_noVe$rotation^2),
  "/"
) * 100

write.csv(contrib_pct_noVe, "../results/Tables/From_Database_04-10-26/Loadings_PCA_noVe.csv")



# Porcentaje de varianza explicada
var_exp_noVe <- (pca_noVe$sdev^2 / sum(pca_noVe$sdev^2)) * 100
var_exp_noVe
pc1_label_noVe <- paste0("PC1 (", round(var_exp_noVe[1], 2), "%)")
pc2_label_noVe <- paste0("PC2 (", round(var_exp_noVe[2], 2), "%)")

# Tabla egigenvalue + variance
pca_variance_noVe <- data.frame(
  PC = paste0("PC", seq_along(var_exp_noVe)),
  Eigenvalue = pca_noVe$sdev^2,
  Variance = var_exp_noVe,
  Cumulative = cumsum(var_exp_noVe)
)

small_province <- pca_results_noVe %>%
  filter(
    Province ==
      "Chiapas Highlands province"
  )

### PCA por provincia con los loadigs

ggplot(
  pca_results_noVe,
  aes(PC1, PC2, color = Province)
) +
  # Vectores de los loadings
  geom_segment(
    data = loadings_noVe,
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
    data = loadings_noVe,
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
      "Chiapas Highlands province" = "#FF4500"
    ),
    labels = c(
      "Sierra Madre del Sur province" = "Sierra Madre del Sur",
      "Balsas Basin province" = "Depresión del Balsas",
      "Pacific Lowlands province" = "Tierras Bajas del Pacífico",
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
    x = pc1_label_noVe,
    y = pc2_label_noVe,
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
  "../results/figures/from_database-04-10-26/PCA_provincias_noVe.png",
  plot = last_plot(),
  width = 8,
  height = 6,
  dpi = 600
)



#### Prueva estadística: PERMANOVA

env_test_noVe <- pca_env_noVera %>%
  select(c(bio_01, bio_04, bio_12, bio_17, bio_06, elev, tri))

province_noVe <- pca_env_noVera$Province

permanova <- adonis2(
  env_test_noVe ~ province_noVe,
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
install.packages("pairwiseAdonis")
pairwise.adonis2(
  env_test,
  factors = province,
  perm = 999
)
