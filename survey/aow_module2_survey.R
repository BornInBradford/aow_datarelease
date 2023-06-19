# Module 2 survey online/offline merge and tidy up

# packages
library(tidyverse)
library(haven)
library(labelled)

# revised or added varname regexp
ra_exp <- "_[ar][0-9]{1,2}$|_[ar][0-9]{1,2}___[0-9]{1,3}$"
# revised or added select-one varname regexp
ra_exp_1 <- "_[ar][0-9]{1,2}$"
# revised or added select-any varname regexp
ra_exp_n <- "_[ar][0-9]{1,2}___[0-9]{1,3}$"
# revised varname regexp
r_exp <- "_r[0-9]{1,2}$|_r[0-9]{1,2}___[0-9]{1,3}$"
# revised select-one varname regexp
r_exp_1 <- "_r[0-9]{1,2}$"
# revised select-any varname regexp
r_exp_n <- "_r[0-9]{1,2}___[0-9]{1,3}$"
# added varname regexp
a_exp <- "_a[0-9]{1,2}$|_a[0-9]{1,2}___[0-9]{1,3}$"
# added select-one varname regexp
a_exp_1 <- "_a[0-9]{1,2}$"
# added select-any varname regexp
a_exp_n <- "_a[0-9]{1,2}___[0-9]{1,3}$"

# new data dictionary column names
dict_name <- c("variable", "form", "section", "type", "label", "categories", "note", "validation_type", "validation_min", "validation_max",
               "identifier", "branching", "required", "alignment", "question_num", "matrix_name", "matrix_rank", "annotation")
# data
online <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\tmpSurvey_Module2_Online.dta")
offline <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\tmpSurvey_Module2_Offline.dta")

# data dictionary
online_dict <- read_csv("survey/redcap/AoWModule2OnlineSurvey_DataDictionary_2023-06-15.csv",
                        col_names = dict_name)
offline_dict <- read_csv("survey/redcap/AoWModule2OfflineForm_DataDictionary_2023-06-15.csv",
                         col_names = dict_name)

# which columns have value label conflicts
vlabel_conflict <- online_dict |> inner_join(select(offline_dict, variable, off_categories = categories),
                                             by = "variable") |>
  filter(categories != off_categories) |>
  select(variable,
         on_categories = categories,
         off_categories)

# there's an extra space in one of the offline labels so this is fine

# add survey indicator to each dataset --- label values once appended 
online <- online %>% mutate(survey_type = 1) # 1=online
offline <- offline %>% mutate(survey_type = 2) # 2=offline

# merge online and offline
mod2_allcols <- online |> bind_rows(offline)

# check conflicting value labels
warnings()

# get revised/added column names
rev_offline <- offline_dict |> filter(variable %in% grep(r_exp, online_dict$variable, value = TRUE))
rev_online <- online_dict |> filter(variable %in% grep(r_exp, offline_dict$variable, value = TRUE))
add_offline <- offline_dict |> filter(variable %in% grep(a_exp, online_dict$variable, value = TRUE))
add_online <- online_dict |> filter(variable %in% grep(a_exp, offline_dict$variable, value = TRUE))


# the second expression using ?_exp_n would only be needed if processing actual variable names
# it removes the ___n from the end of the name
# only the stem is in the data dictionary i.e. not including the ___ and checkbox value
online_dict <- online_dict |> mutate(added = case_when(grepl(a_exp_1, variable) ~ substr(variable, nchar(variable), nchar(variable)),
                                                       grepl(a_exp_n, variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                                     revised = case_when(grepl(r_exp_1, variable) ~ substr(variable, nchar(variable), nchar(variable)),
                                                         grepl(r_exp_n, variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                                     hidden = case_when(grepl("hidden", note, ignore.case = TRUE) ~ str_extract(note, "[\\d]+")),
                                     year_group = case_when(grepl("year_group", branching) ~ str_extract(branching, "[\\d]+")),
                                     online_only = case_when(!variable %in% offline_dict$variable ~ "online only",
                                                             TRUE ~ "in both")
)
offline_dict <- offline_dict |> mutate(added = case_when(grepl(a_exp_1, variable) ~ substr(variable, nchar(variable), nchar(variable)),
                                                         grepl(a_exp_n, variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                                       revised = case_when(grepl(r_exp_1, variable) ~ substr(variable, nchar(variable), nchar(variable)),
                                                           grepl(r_exp_n, variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                                       hidden = case_when(grepl("hidden", note, ignore.case = TRUE) ~ str_extract(note, "[\\d]+")),
                                       year_group = case_when(grepl("year_group", branching) ~ str_extract(branching, "[\\d]+")),
                                       offline_only = case_when(!variable %in% online_dict$variable ~ "offline only",
                                                               TRUE ~ "in both")
)

# process categories to add missing values for added from, revised from, removed from, not in yr group, not online
# for checkbox variables add a new variable named for the stem just containing the added/revised/etc categories
# > maybe add a present category to these ones

