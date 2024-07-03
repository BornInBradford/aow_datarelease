# Module 1 survey online/offline merge and tidy up

source("tools/aow_survey_functions.R")

redcap_project_name <- "aow_module_231_online_survey"

# data
online <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\Survey_Module231_Online.dta")
offline <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\Survey_Module231_Offline.dta")

# set min and max survey versions in this data
# NB if version var is missing it sets min/max to Inf/-Inf and all modified vars will be removed
min_online_version <- min(online$mod1_version, na.rm = TRUE)
max_online_version <- max(online$mod1_version, na.rm = TRUE)
min_offline_version <- min(offline$mod1_version, na.rm = TRUE)
max_offline_version <- max(offline$mod1_version, na.rm = TRUE)

# data dictionary
online_dict <- read_csv("survey/redcap/AoWModule231OnlineSurvey_DataDictionary_2024-07-02.csv",
                        col_names = aow_dict_colnames(), skip = 1)
offline_dict <- read_csv("survey/redcap/AoWModule231OfflineForm_DataDictionary_2024-07-02.csv",
                         col_names = aow_dict_colnames(), skip = 1)

# drop validation columns that have unpredictable value types
online_dict <- online_dict |> select(-validation_max, -validation_min)
offline_dict <- offline_dict |> select(-validation_max, -validation_min)

# get year group from denominator
yrgp_lkup <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/denom/data/denom_pseudo.rds")
yrgp_lkup <- yrgp_lkup |> select(aow_recruitment_id, year_group)

# get survey timestamps lookup
timestamps <- readRDS("U:\\Born in Bradford - AOW Raw Data\\sql\\survey_process\\data\\AOW_Survey_Timestamps.rds")
timestamps <- timestamps |> filter(project_name == redcap_project_name) |> select(-project_id, -project_name)


# which columns have value label conflicts
vlabel_conflict <- online_dict |> inner_join(select(offline_dict, variable, off_categories = categories),
                                             by = "variable") |>
  filter(categories != off_categories) |>
  select(variable,
         on_categories = categories,
         off_categories)

# no label conflicts

# add survey indicator to each dataset --- label values once appended 
online <- online %>% mutate(survey_mode = 1) # 1=online
offline <- offline %>% mutate(survey_mode = 2) # 2=offline

# value types incorrectly read as numeric
# process here


# add missing/changed question indicators to data dictionaries
online_dict <- online_dict |> aow_add_dict_cols()
offline_dict <- offline_dict |> aow_add_dict_cols() 


# trim data down to match in-version data dict vars
online <- online |> aow_trim_var_versions_data(online_dict, min_online_version, max_online_version)
offline <- offline |> aow_trim_var_versions_data(offline_dict, min_offline_version, max_offline_version)

# trim version changes that are out of range of the data
online_dict <- online_dict |> aow_trim_var_versions(min_online_version, max_online_version) |>
  mutate(online_only = case_when(!variable %in% offline_dict$variable ~ "online only", TRUE ~ "in both"))
offline_dict <- offline_dict |> aow_trim_var_versions(min_offline_version, max_offline_version) |>
  mutate(offline_only = case_when(!variable %in% online_dict$variable ~ "offline only", TRUE ~ "in both"))



# merge online and offline, do some renaming needed for processing
mod_allcols <- online |> bind_rows(offline) |>
  set_value_labels(survey_mode = c("Online" = 1, "Offline" = 2)) |>
  rename(survey_version = mod1_version)

# tidy up recruitment ids
mod_allcols <- mod_allcols |> mutate(aow_recruitment_id = coalesce(aow_id, redcap_survey_identifier),
                                     aow_recruitment_id = gsub("[^aowAOW0-9]", "", aow_recruitment_id),
                                     aow_recruitment_id = tolower(aow_recruitment_id))

# preserve order of columns before any processing occurs
mod_allcols_order <- names(mod_allcols)

# replace year_group with more complete variable from denominator
mod_allcols$year_group <- NULL
mod_allcols <- mod_allcols |> left_join(yrgp_lkup)

# merge all survey timestamps - might creates duplicates but we'll deal with these later
mod_allcols <- mod_allcols |> left_join(timestamps)

# check conflicting value labels
warnings()


# add checkbox options to value labels

checkboxes <- offline_dict |> bind_rows(online_dict) |>
  select(variable, type, categories)
checkboxes <- checkboxes |> filter(type %in% aow_redcap_chk_type()) |>
  select(variable, categories) |> unique()


# loop through variables
if(nrow(checkboxes > 0)) {
  
  # parse category labels
  chk_labels <- str_split(checkboxes$categories, fixed("|"))
  chk_labels <- map(chk_labels, trimws)
  names(chk_labels) <- checkboxes$variable
  chk_labels <- map(chk_labels, str_split, pattern = ",", n = 2)
  for(v in 1:length(chk_labels)) chk_labels[[v]] <- map(chk_labels[[v]], trimws)
  
  for(v in checkboxes$variable) {
    mod_allcols <- mod_allcols |> aow_label_chk(v, chk_labels[[v]])
  }
}


# get revised/added column names
rev_offline <- offline_dict |> filter(variable %in% grep(aow_srv_regexp("rev_cat"), online_dict$variable, value = TRUE))
rev_online <- online_dict |> filter(variable %in% grep(aow_srv_regexp("rev_cat"), offline_dict$variable, value = TRUE))
add_offline <- offline_dict |> filter(variable %in% grep(aow_srv_regexp("add_cat"), online_dict$variable, value = TRUE))
add_online <- online_dict |> filter(variable %in% grep(aow_srv_regexp("add_cat"), offline_dict$variable, value = TRUE))



# add year group missing vars

year_group <- offline_dict |> bind_rows(online_dict) |>
  filter(!is.na(year_group)) |>
  select(variable, type, year_group) |>
  unique()
year_group_cat <- year_group |> filter(type %in% aow_redcap_cat_type()) |>
  select(variable, year_group) |> unique()
year_group_txt <- year_group |> filter(type %in% aow_redcap_txt_type()) |>
  select(variable, year_group) |> unique()

# loop through variables - categorical
if(nrow(year_group_cat > 0)) {
  for(v in 1:nrow(year_group_cat)) {
    mod_allcols <- mod_allcols |> aow_miss_cat_yrgrp(year_group_cat$variable[v], year_group_cat$year_group[v])
  }
}
# loop through variables - text
if(nrow(year_group_txt > 0)) {
  for(v in 1:nrow(year_group_txt)) {
    mod_allcols <- mod_allcols |> aow_miss_txt_yrgrp(year_group_txt$variable[v], year_group_txt$year_group[v])
  }
}


# add version when added missing vars
added <- offline_dict |> bind_rows(online_dict) |>
  filter(!is.na(added)) |>
  select(variable, type, added) |>
  unique()
added_cat <- added |> filter(type %in% aow_redcap_cat_type()) |>
  select(variable, added) |> unique()
added_txt <- added |> filter(type %in% aow_redcap_txt_type()) |>
  select(variable, added) |> unique()


# loop through variables - categorical
if(nrow(added_cat > 0)) {
  for(v in 1:nrow(added_cat)) {
    mod_allcols <- mod_allcols |> aow_miss_cat_added(added_cat$variable[v], added_cat$added[v])
  }
}
# loop through variables - text
if(nrow(added_txt > 0)) {
  for(v in 1:nrow(added_txt)) {
    mod_allcols <- mod_allcols |> aow_miss_txt_added(added_txt$variable[v], added_txt$added[v])
  }
}


# add version when revised missing vars
revised <- offline_dict |> bind_rows(online_dict) |>
  filter(!is.na(revised)) |>
  select(variable, type, revised) |>
  unique()
revised_cat <- revised |> filter(type %in% aow_redcap_cat_type()) |>
  select(variable, revised) |> unique()
revised_txt <- revised |> filter(type %in% aow_redcap_txt_type()) |>
  select(variable, revised) |> unique()


# loop through variables - categorical
if(nrow(revised_cat > 0)) {
  for(v in 1:nrow(revised_cat)) {
    mod_allcols <- mod_allcols |> aow_miss_cat_revised(revised_cat$variable[v], revised_cat$revised[v])
  }
}
# loop through variables - text
if(nrow(revised_txt > 0)) {
  for(v in 1:nrow(revised_txt)) {
    mod_allcols <- mod_allcols |> aow_miss_txt_revised(revised_txt$variable[v], revised_txt$revised[v])
  }
}



# add version when removed missing vars
removed <- offline_dict |> bind_rows(online_dict) |>
  filter(!is.na(hidden)) |>
  select(variable, type, removed = hidden) |>
  unique()
removed_cat <- removed |> filter(type %in% aow_redcap_cat_type()) |>
  select(variable, removed) |> unique()
removed_txt <- removed |> filter(type %in% aow_redcap_txt_type()) |>
  select(variable, removed) |> unique()


# loop through variables - categorical
if(nrow(removed_cat > 0)) {
  for(v in 1:nrow(removed_cat)) {
    mod_allcols <- mod_allcols |> aow_miss_cat_removed(removed_cat$variable[v], removed_cat$removed[v])
  }
}
# loop through variables - text
if(nrow(removed_txt > 0)) {
  for(v in 1:nrow(removed_txt)) {
    mod_allcols <- mod_allcols |> aow_miss_txt_removed(removed_txt$variable[v], removed_txt$removed[v])
  }
}


# restore column order
mod_allcols <- mod_allcols |> select(any_of(mod_allcols_order), everything())

# export
saveRDS(mod_allcols, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module231_merged.rds")

