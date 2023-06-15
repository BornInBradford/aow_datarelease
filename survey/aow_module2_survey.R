# Appending AoW Online and Offline Module 1 Survey
# Amy Hough
# 04/04/2023

# packages
library(tidyverse)
library(haven)
library(labelled)

# revised or added varname regexp
ra_exp <- "_[ar][0-9]{1,2}$|_[ar][0-9]{1,2}__[0-9]{1,3}$"
# revised varname regexp
r_exp <- "_r[0-9]{1,2}$|_r[0-9]{1,2}__[0-9]{1,3}$"
# added varname regexp
a_exp <- "_a[0-9]{1,2}$|_a[0-9]{1,2}__[0-9]{1,3}$"

# data
online <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\tmpSurvey_Module2_Online.dta")
offline <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\tmpSurvey_Module2_Offline.dta")

# data dictionary
online_dict <- read_csv("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data_dictionary\\AoWModule2OnlineSurvey_DataDictionary_2023-06-15.csv")
offline_dict <- read_csv("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data_dictionary\\AoWModule2OfflineForm_DataDictionary_2023-06-15.csv")

# which columns are in online but not offline
online_only <- online_dict |> anti_join(offline_dict, by = "Variable / Field Name")
offline_only <- offline_dict |> anti_join(online_dict, by = "Variable / Field Name")

# which columns have value label conflicts
vlabel_conflict <- online_dict |> inner_join(select(offline_dict, `Variable / Field Name`, off_labs = `Choices, Calculations, OR Slider Labels`),
                                             by = "Variable / Field Name") |>
  filter(`Choices, Calculations, OR Slider Labels` != off_labs) |>
  select(`Variable / Field Name`,
         on_labs = `Choices, Calculations, OR Slider Labels`,
         off_labs)

# there's an extra space in one of the offline labels so this is fine

# add survey indicator to each dataset --- label values once appended 
online <- online %>% mutate(survey_type = 1) # 1=online
offline <- offline %>% mutate(survey_type = 2) # 2=offline

# merge online and offline
mod2_allcols <- online |> bind_rows(offline)

# check conflicting value labels
warnings()

# get revised/added column names
revadd_offline <- offline_dict |> filter(`Variable / Field Name` %in% grep(ra_exp, online_dict$`Variable / Field Name`, value = TRUE))
revadd_online <- online_dict |> filter(`Variable / Field Name` %in% grep(ra_exp, offline_dict$`Variable / Field Name`, value = TRUE))
rev_offline <- offline_dict |> filter(`Variable / Field Name` %in% grep(r_exp, online_dict$`Variable / Field Name`, value = TRUE))
rev_online <- online_dict |> filter(`Variable / Field Name` %in% grep(r_exp, offline_dict$`Variable / Field Name`, value = TRUE))
add_offline <- offline_dict |> filter(`Variable / Field Name` %in% grep(a_exp, online_dict$`Variable / Field Name`, value = TRUE))
add_online <- online_dict |> filter(`Variable / Field Name` %in% grep(a_exp, offline_dict$`Variable / Field Name`, value = TRUE))

