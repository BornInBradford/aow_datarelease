# Module 2 survey online/offline merge and tidy up

source("tools/aow_survey_functions.R")

# data
online <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\tmpSurvey_Module2_Online.dta")
offline <- read_dta("U:\\Born in Bradford - AOW Raw Data\\redcap\\surveys\\data\\tmpSurvey_Module2_Offline.dta")

# data dictionary
online_dict <- read_csv("survey/redcap/AoWModule2OnlineSurvey_DataDictionary_2023-06-15.csv",
                        col_names = aow_dict_colnames(), skip = 1)
offline_dict <- read_csv("survey/redcap/AoWModule2OfflineForm_DataDictionary_2023-06-15.csv",
                         col_names = aow_dict_colnames(), skip = 1)

# which columns have value label conflicts
vlabel_conflict <- online_dict |> inner_join(select(offline_dict, variable, off_categories = categories),
                                             by = "variable") |>
  filter(categories != off_categories) |>
  select(variable,
         on_categories = categories,
         off_categories)

# there's an extra space in one of the offline labels so this is fine

# add survey indicator to each dataset --- label values once appended 
online <- online %>% mutate(survey_mode = 1) # 1=online
offline <- offline %>% mutate(survey_mode = 2) # 2=offline

# merge online and offline
mod_allcols <- online |> bind_rows(offline) |>
  set_value_labels(survey_mode = c("Online" = 1, "Offline" = 2))

# check conflicting value labels
warnings()

# get revised/added column names
rev_offline <- offline_dict |> filter(variable %in% grep(aow_srv_regexp("rev_cat"), online_dict$variable, value = TRUE))
rev_online <- online_dict |> filter(variable %in% grep(aow_srv_regexp("rev_cat"), offline_dict$variable, value = TRUE))
add_offline <- offline_dict |> filter(variable %in% grep(aow_srv_regexp("add_cat"), online_dict$variable, value = TRUE))
add_online <- online_dict |> filter(variable %in% grep(aow_srv_regexp("add_cat"), offline_dict$variable, value = TRUE))


# the second expression using ?_chk would only be needed if processing actual variable names
# it removes the ___n from the end of the name
# only the stem is in the data dictionary i.e. not including the ___ and checkbox value
online_dict <- online_dict |> mutate(added = case_when(grepl(aow_srv_regexp("add_rad"), variable) ~ substr(variable, nchar(variable), nchar(variable)),
                                                       grepl(aow_srv_regexp("add_chk"), variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                                     revised = case_when(grepl(aow_srv_regexp("rev_rad"), variable) ~ substr(variable, nchar(variable), nchar(variable)),
                                                         grepl(aow_srv_regexp("rev_chk"), variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                                     hidden = case_when(grepl("hidden", note, ignore.case = TRUE) ~ str_extract(note, "[\\d]+")),
                                     year_group = case_when(grepl("year_group", branching) ~ str_extract(branching, "[\\d]+")),
                                     online_only = case_when(!variable %in% offline_dict$variable ~ "online only",
                                                             TRUE ~ "in both")
)
offline_dict <- offline_dict |> mutate(added = case_when(grepl(aow_srv_regexp("add_rad"), variable) ~ substr(variable, nchar(variable), nchar(variable)),
                                                         grepl(aow_srv_regexp("add_chk"), variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                                       revised = case_when(grepl(aow_srv_regexp("rev_rad"), variable) ~ substr(variable, nchar(variable), nchar(variable)),
                                                           grepl(aow_srv_regexp("rev_chk"), variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                                       hidden = case_when(grepl("hidden", note, ignore.case = TRUE) ~ str_extract(note, "[\\d]+")),
                                       year_group = case_when(grepl("year_group", branching) ~ str_extract(branching, "[\\d]+")),
                                       offline_only = case_when(!variable %in% online_dict$variable ~ "offline only",
                                                               TRUE ~ "in both")
)

# add online/offline missing vars
off_only <- offline_dict |> 
  filter(!type == "descriptive" & offline_only != "in both") |> 
  select(variable, type)
on_only <- online_dict |> 
  filter(!type == "descriptive" & online_only != "in both") |> 
  select(variable, type)

off_only_txt <- off_only |> filter(type %in% aow_redcap_txt_type()) |> pull(variable)
on_only_txt <- on_only |> filter(type %in% aow_redcap_txt_type()) |> pull(variable)
off_only_cat <- off_only |> filter(type %in% aow_redcap_cat_type()) |> pull(variable)
on_only_cat <- on_only |> filter(type %in% aow_redcap_cat_type()) |> pull(variable)

# loop through variables - categorical
for(var in off_only_cat) {
  mod_allcols <- mod_allcols |> aow_miss_cat_offline(var)
}
for(var in on_only_cat) {
  mod_allcols <- mod_allcols |> aow_miss_cat_online(var)
}
# loop through variables - text
for(var in off_only_txt) {
  mod_allcols <- mod_allcols |> aow_miss_txt_offline(var)
}
for(var in on_only_txt) {
  mod_allcols <- mod_allcols |> aow_miss_txt_online(var)
}

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


# 
# # add "added" missing values
# # merge online and offline for this
# off_tmp <- offline_dict |> select(variable, added) |> filter(!is.na(added))
# on_tmp <- online_dict |> select(variable, added) |> filter(!is.na(added))
# added_vars <- off_tmp |> bind_rows(on_tmp) |> unique()
# 



# process categories to add missing values for on/off, year group, added, revised, removed
# for checkbox variables add a new variable named for the stem just containing the added/revised/etc categories
# > maybe add a present category to these ones
#
# may need to always add checkbox header variable at the start
# makes processing them simpler if they are always there
# also might be the only place for the question to go?

