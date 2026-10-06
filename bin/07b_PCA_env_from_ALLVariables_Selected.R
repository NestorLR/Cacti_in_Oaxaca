library(factoextra)
library(ggplot2)
library(dplyr)
library(vegan)
library(pairwiseAdonis)

setwd("/media/delil/ADATA_Nestor_Blue/Nestor_phD/Spatial_Analysis/Oax_Cactaceae/bin")


################################################################################
##### Variables VIF<10

pca_data <- read.csv("../env_data/Oax_env_selected_variables_fromALL_VIF10.csv")

#scale
env_scaled <- pca_data %>%
  select(-c(Species, lon, lat, Province)) %>%
  scale()

pca <- prcomp(
  env_scaled,
  center = TRUE,
  scale. = TRUE
)

summary(pca)

round(
  pca$rotation,
  2
)




######## PCA con densidad
# scores PCA
scores <- as.data.frame(pca$x)

pca_results <- bind_cols(
  pca_data,
  scores
)

ggplot(
  pca_results,
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

## PCA por provincia
ggplot(
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
  theme_bw()


ggsave(
  "../figures/PCA_provincias_complete.png",
  plot = PCA_provincias_complete,
  width = 8,
  height = 6,
  dpi = 600
)



## Biplot de variables
library(factoextra)

variables_PCA_completo <-fviz_pca_var(
  pca,
  repel = TRUE
)


ggsave(
  "../figures/PCA_variables.png",
  plot = variables_PCA_completo,
  width = 8,
  height = 6,
  dpi = 600
)


#### Prueva estadística: PERMANOVA

env_test <- pca_results %>%
  select(
    bio_01,
    bio_04,
    bio_12,
    bio_15,
    tri
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
install.packages("pairwiseAdonis")
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



################################################################################
##### Variables Pearson <0.8

pca_data_Pearson <- read.csv("../env_data/Oax_env_selected_variables_fromALL_Pearson0.8.csv")
names(pca_data_comple)


#scale
env_scaled <- pca_data_Pearson %>%
  select(,5:13) %>%
  scale()

pca_comple <- prcomp(
  env_scaled,
  center = TRUE,
  scale. = TRUE
)

summary(pca_comple)

round(
  pca_comple$rotation,
  2
)




######## PCA con densidad
# scores PCA
scores <- as.data.frame(pca_comple$x)

pca_env <- bind_cols(
  pca_data_Pearson,
  scores
)

ggplot(
  pca_env,
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
  pca_comple,
  repel = TRUE
)


## PCA por provincia
ggplot(
  pca_env,
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
