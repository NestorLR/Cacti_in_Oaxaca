library(tidyr)
library(dplyr)
library(ggplot2)

## Loadings
loadigs <- read.csv("../results/Tables/From_Database_04-10-26/Loadings_PCA.csv")


## data
pca_results <- read.csv("../results/Tables/From_Database_04-10-26/PCA_results_all.csv")

# Las cinco variables con mayor variación acumulada del PC1 y PC2 según la tabla de loadings
variables_boxplot <- c(
  "bio_18",
  "bio_19",
  "bio_14",
  "bio_15",
  "bio_07",
  "bio_11"
)

boxplot_data <- pca_results %>%
  select(
    Province,
    all_of(variables_boxplot)
  ) %>%
  pivot_longer(
    cols = all_of(variables_boxplot),
    names_to = "Variable",
    values_to = "Value"
  )

boxplot_data <- boxplot_data %>%
  mutate(
    Variable = gsub(
      "bio_",
      "BIO",
      Variable
    )
  )


variable_labels <- c(
  "BIO18" = "BIO18 — Precipitación del trimestre más cálido",
  "BIO19" = "BIO19 — Precipitación del trimestre más frío",
  "BIO14" = "BIO14 — Precipitación del mes más seco",
  "BIO15" = "BIO15 — Estacionalidad de la precipitación",
  "BIO07" = "BIO07 — Intervalo anual de temperatura", 
  "BIO11" = "BIO11 — Temperatura media del trimestre más frío"
)

boxplot_data <- boxplot_data %>%
  mutate(
    Variable = recode(
      Variable,
      !!!variable_labels
    )
  )

## boxplot
ggplot(
  boxplot_data,
  aes(
    x = Province,
    y = Value,
    fill = Province
  )
) +
  
  geom_boxplot(
    alpha = 0.75,
    outlier.alpha = 0.4,
    linewidth = 0.4
  ) +
  
  facet_wrap(
    ~ Variable,
    scales = "free_y",
    ncol = 2
  ) +
  
  scale_fill_manual(
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
  
  theme_bw() +
  
  labs(
    x = NULL,
    y = "Valor ambiental",
    fill = "Provincia:"
  ) +
  
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 8
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 7)
  )
ggsave(
  "../results/figures/from_database-04-10-26/Boxplot_provincias.png",
  plot = last_plot(),
  width = 9,
  height = 6,
  dpi = 600
)

ggsave(
  "../results/figures/from_database-04-10-26/Boxplot_provincias.pdf",
  plot = last_plot(),
  width = 9,
  height = 6,
  dpi = 600
)


#### tabla de datos
summary_env <- boxplot_data %>%
  group_by(Province, Variable) %>%
  summarise(
    n = sum(!is.na(Value)),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = IQR(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  summary_env,
  "../results/Tables/From_Database_04-10-26/Summary_environmental_variables.csv",
  row.names = FALSE
)
