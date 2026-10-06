library(factoextra)
library(ggplot2)
library(dplyr)
library(vegan)
library(pairwiseAdonis)

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
  select(-c(Species, lon, lat, Province, Genus, Endemismo)) %>%
  scale()

pca <- prcomp(
  env_scaled,
  center = TRUE,
  scale. = TRUE
)

summary(pca)

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

round(pca_variance, 2)


## PCA por provincia
PCA_provincias_complete <- ggplot(
  pca_results,
  aes(PC1, PC2,
      color = Province)
) +
  stat_ellipse(
    linewidth = 1.2
  ) +
  geom_point(
    alpha = 0.15,
    size = 0.8
  ) +
  theme_bw() +
  labs(
    x = pc1_label,
    y = pc2_label
  )

ggsave(
  "../results/figures/from_database-04-10-26/PCA_provincias_complete.png",
  plot = PCA_provincias_complete,
  width = 8,
  height = 6,
  dpi = 600
)



### PCA por provincia con los loadigs

ggplot(
  pca_results,
  aes(PC1, PC2, color = Province)
) +
  
  # Elipses de las provincias
  stat_ellipse(
    linewidth = 1.2
  ) +
  
  # Observaciones
  geom_point(
    alpha = 0.15,
    size = 0.8
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
    color = "black"
  ) +
  
  # Nombres de las variables
  geom_text(
    data = loadings,
    aes(
      x = PC1_arrow,
      y = PC2_arrow,
      label = Variable
    ),
    inherit.aes = FALSE,
    color = "black",
    size = 4
  ) +
  theme_bw() +
  labs(
    x = pc1_label,
    y = pc2_label,
    color = "Province"
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




############################
# PCA 
small_province <- pca_results_comple %>%
  filter(
    Province ==
      "Chiapas Highlands province"
  )

chiapas <- ggplot(
  pca_results_comple,
  aes(
    PC1,
    PC2,
    color = Province
  )
) +
  
  stat_ellipse(
    linewidth = 1.2
  ) +
  
  # todos los puntos
  geom_point(
    alpha = 0.15,
    size = 0.8
  ) +
  
  # resaltar provincia rara
  geom_point(
    data = small_province,
    shape = 21,
    fill = "yellow",
    color = "black",
    size = 4,
    stroke = 1.2
  ) +
  
  theme_bw()

ggsave(
  "../figures/PCA_Chiapas_complete.png",
  plot = chiapas,
  width = 8,
  height = 6,
  dpi = 600
)



## Biplot de variables
library(factoextra)

variables_PCA_completo <-fviz_pca_var(
  pca_comple,
  repel = TRUE
)


ggsave(
  "../figures/PCA_variables.png",
  plot = variables_PCA_completo,
  width = 8,
  height = 6,
  dpi = 600
)
################################################################################
##### Variables completas sin veracruz

pca_data_comple <- read.csv("../env_data/Oax_env_selected_variables_complete.csv")
names(pca_data_comple)


env_noVera <- pca_data_comple %>%
  filter(
    Province != "Veracruzan province"
  )


#scale
env_scaled_noVe <- env_noVera %>%
  select(c(bio_01, bio_04, bio_12, bio_17, bio_06, elev, tri)) %>%
  scale()

pca_comple_noVe <- prcomp(
  env_scaled_noVe,
  center = TRUE,
  scale. = TRUE
)

summary(pca_comple_noVe)

round(
  pca_comple_noVe$rotation,
  2
)




######## PCA con densidad
# scores PCA
scores <- as.data.frame(pca_comple_noVe$x)

pca_env_noVera <- bind_cols(
  env_noVera,
  scores
)

ggplot(
  pca_env_noVera,
  aes(PC1, PC2)
) +
  stat_density_2d(
    aes(fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.7
  ) +
  scale_fill_viridis_c() +
  theme_bw() +
  labs(
    x = "PC1",
    y = "PC2"
  )

## Biplot de variables
library(factoextra)

fviz_pca_var(
  pca_comple_noVe,
  repel = TRUE
)


## PCA por provincia
ggplot(
  pca_env_noVera,
  aes(PC1, PC2,
      color = Province)
) +
  stat_ellipse(
    linewidth = 1.2
  ) +
  geom_point(
    alpha = 0.8,
    size = 0.8
  ) +
  theme_bw()





############################
# PCA 
small_province <- pca_env_noVera %>%
  filter(
    Province ==
      "Chiapas Highlands province"
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
