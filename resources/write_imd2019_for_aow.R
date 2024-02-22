####
# Author: Aidan Watmuff
# Date: 14/02/2024
# Purpose: Reshape IMD 2019 data from Department for Communities and Local Government to wide format for use in AoW IMD variables. 
# Note: run in aow_datarelease.Rroj
####

# Set up ----
library(tidyverse)#data wrangling

# paths
resources_path <- "./resources" 

# read in data 
imd2019_dclg_data <- read.csv(paste0(resources_path, "/imd2019lsoa.csv"))

# format ----
# data is in long format. need to extract one table per indices of deprivation
imd2019_dclg_data %>%
  count(Indices.of.Deprivation, Measurement)

# format imd2019 data
imd2019_dclg <- imd2019_dclg_data %>%
  rename(LSOA11CD = FeatureCode)

# select IMD decile for all lsoa
imd_2019_dec <- imd2019_dclg %>%
  filter(Indices.of.Deprivation == "a. Index of Multiple Deprivation (IMD)" & Measurement == "Decile ") %>%
  select(LSOA11CD, IMD_2019_decile = Value)

# select IMD score for all lsoa
imd_2019_score <- imd2019_dclg %>%
  filter(Indices.of.Deprivation == "a. Index of Multiple Deprivation (IMD)" & Measurement == "Score") %>%
  select(LSOA11CD, IMD_2019_score = Value)

# join tables together - this gives IMD in wide format, 1 record per LSOA
imd2019_data <- imd_2019_dec %>%
  left_join(imd_2019_score, by = "LSOA11CD")

# write to csv
write.csv(imd2019_data, paste0(resources_path, "/imd2019_for_aow.csv"), row.names = F)
