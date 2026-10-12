library(dplyr)
library(tidyr)

cactus_df <- read.csv("../data/env_data/from_database-04-10-26/database_04-10-26_env_3511.csv")

especies_compartidas <- cactus_df %>%
  filter(Province %in% c(
    "Pacific Lowlands province",
    "Sierra Madre del Sur province"
  )) %>%
  count(Species, Province) %>%
  pivot_wider(
    names_from = Province,
    values_from = n,
    values_fill = 0
  )


library(dplyr)
library(purrr)
library(broom)

comparar_especie <- function(datos, variable) {
  
  datos %>%
    group_by(Species) %>%
    group_modify(~ {
      d <- .x
      
      if (n_distinct(d$Province) != 2 ||
          any(table(d$Province) < 2)) {
        return(tibble(
          diferencia = NA_real_,
          p = NA_real_
        ))
      }
      
      prueba <- t.test(
        d[[variable]] ~ d$Province
      )
      
      tibble(
        diferencia = unname(
          diff(rev(prueba$estimate))
        ),
        p = prueba$p.value
      )
    }) %>%
    ungroup() %>%
    mutate(
      p_ajustado = p.adjust(p, method = "BH"),
      significativo = p_ajustado < 0.05
    )
}
