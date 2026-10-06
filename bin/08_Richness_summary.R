library(terra)
library(dplyr)
library(stringr)


############################################################
# 1. Occurrence dataset
############################################################
#Dataset with no coordinates uncertained 
cactus <- read.csv(
  "../data/occ_data/Dataset_04_10_26.csv"
)


############################################################
# 10. Table: occurrences per species
############################################################

occurrences_per_species <- cactus %>%
  count(
    Species,
    name = "n_occurrences"
  ) %>%
  arrange(
    desc(n_occurrences)
  )

############################################################
# 11. Table: richness by genus
############################################################

cactus$Genus <- word(
  cactus$Species,
  1
)
richness_by_genus <- cactus %>%
  distinct(
    Genus,
    Species
  ) %>%
  count(
    Genus,
    name = "species_richness"
  ) %>%
  arrange(
    desc(species_richness)
  )

############################################################
# 12. Table: richness summary
############################################################

richness_summary <- tibble(
  n_genera =
    n_distinct(
      cactus$Genus
    ),
  
  n_species =
    n_distinct(
      cactus$Species
    ),
  
  total_occurrences =
    nrow(cactus)
)

print(richness_summary)

############################################################
# 12. Table: richness by provinces
############################################################

richness_by_province <- cactus %>%
  distinct(
    Province,
    Species
  ) %>%
  count(
    Province,
    name = "species_richness"
  ) %>%
  arrange(
    desc(species_richness)
  )

############################################################
# 12. Table: genus by provinces
############################################################
genus_by_province <- cactus %>%
  distinct(
    Province,
    Genus
  ) %>%
  count(
    Province,
    name = "genus_richness"
  ) %>%
  arrange(
    desc(genus_richness)
  )


############################################################
# 12. Table: richness by genus by provinces
############################################################
richness_genus_province <- cactus %>%
  distinct(
    Province,
    Genus,
    Species
  ) %>%
  count(
    Province,
    Genus,
    name = "species_richness"
  ) %>%
  arrange(
    Province,
    desc(species_richness)
  )


############################################################
# 12. Table: occurances by provinces
############################################################

occurrences_by_province <- cactus %>%
  count(
    Province,
    name = "n_occurrences"
  ) %>%
  arrange(
    desc(n_occurrences)
  )


############################################################
# 13. Endemic results
############################################################

cactus_oax <- cactus %>% filter(Endemismo == "Oaxaca")

cactus_oax$Genus <- word(
  cactus_oax$Species,
  1
)

richness_summary_oax <- tibble(
  n_genera =
    n_distinct(
      cactus_oax$Genus
    ),
  
  n_species =
    n_distinct(
      cactus_oax$Species
    ),
  
  total_occurrences =
    nrow(cactus_oax)
)

print(richness_summary_oax)


richness_by_province_oax <- cactus_oax %>%
  distinct(
    Province,
    Species
  ) %>%
  count(
    Province,
    name = "species_richness"
  ) %>%
  arrange(
    desc(species_richness)
  )


richness_genus_province_oax <- cactus_oax %>%
  distinct(
    Province,
    Genus,
    Species
  ) %>%
  count(
    Province,
    Genus,
    name = "species_richness"
  ) %>%
  arrange(
    Province,
    desc(species_richness)
  )


############################################################
# 13. Save outputs
############################################################
write.csv(
  occurrences_per_species,
  "../results/Tables/From_Database_04-10-26/All_occurrences_per_species.csv",
  row.names = FALSE
)

write.csv(
  richness_by_genus,
  "../results/Tables/From_Database_04-10-26/All_richness_by_genus.csv",
  row.names = FALSE
)

write.csv(
  richness_summary,
  "../results/Tables/From_Database_04-10-26/All_richness_summary.csv",
  row.names = FALSE
)

write.csv(
  richness_by_province,
  "../results/Tables/From_Database_04-10-26/All_richness_by_province.csv",
  row.names = FALSE
)

write.csv(
  richness_genus_province,
  "../results/Tables/From_Database_04-10-26/All_richness_genus_province.csv",
  row.names = FALSE
)


write.csv(
  genus_by_province,
  "../results/Tables/From_Database_04-10-26/All_genus_by_province.csv",
  row.names = FALSE
)




#endemic data


write.csv(
  richness_by_genus_oax,
  "../results/Tables/From_Database_04-10-26/Oax_richness_by_genus.csv",
  row.names = FALSE
)

write.csv(
  richness_summary_oax,
  "../results/Tables/From_Database_04-10-26/Oax_richness_summary.csv",
  row.names = FALSE
)

write.csv(
  richness_by_province_oax,
  "../results/Tables/From_Database_04-10-26/Oax_richness_by_province.csv",
  row.names = FALSE
)

write.csv(
  richness_genus_province_oax,
  "../results/Tables/From_Database_04-10-26/Oax_richness_genus_province.csv",
  row.names = FALSE
)


