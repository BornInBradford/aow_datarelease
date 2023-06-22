# packages
library(tidyverse)
library(haven)
library(labelled)
library(lubridate)

# set values that need to be controlled across survey scripts
aow_srv_regexp <- function(type) {
  
  # allowing _ before version number, i.e. a_3 and r_5 due to naming error in module 2
  exp <- case_when(type == "rev_cat" ~ "_r[_]{0,1}[0-9]{1,2}$|_r[_]{0,1}[0-9]{1,2}___[0-9]{1,3}$",
                   type == "rev_rad" ~ "_r[_]{0,1}[0-9]{1,2}$",
                   type == "rev_chk" ~ "_r[_]{0,1}[0-9]{1,2}___[0-9]{1,3}$",
                   type == "add_cat" ~ "_a[_]{0,1}[0-9]{1,2}$|_a[_]{0,1}[0-9]{1,2}___[0-9]{1,3}$",
                   type == "add_rad" ~ "_a[_]{0,1}[0-9]{1,2}$",
                   type == "add_chk" ~ "_a[_]{0,1}[0-9]{1,2}___[0-9]{1,3}$")
  
  if(is.na(exp)) stop(paste0("AoW survey regular expression type not found: ", type))
  
  return(exp)
  
}


aow_miss_label <- function(type) {
  
  lab <- case_when(type == "added_2" ~ c("Added in version 2" = -1),
                   type == "added_3" ~ c("Added in version 3" = -1),
                   type == "added_4" ~ c("Added in version 4" = -1),
                   type == "added_5" ~ c("Added in version 5" = -1),
                   type == "added_6" ~ c("Added in version 6" = -1),
                   type == "added_7" ~ c("Added in version 7" = -1),
                   type == "added-8" ~ c("Added in version 8" = -1),
                   type == "added_9" ~ c("Added in version 9" = -1),
                   type == "added_10" ~ c("Added in version 10" = -1),
                   type == "added_11" ~ c("Added in version 11" = -1),
                   type == "added_12" ~ c("Added in version 12" = -1),
                   type == "revised_2" ~ c("Revised in version 2" = -2),
                   type == "revised_3" ~ c("Revised in version 3" = -2),
                   type == "revised_4" ~ c("Revised in version 4" = -2),
                   type == "revised_5" ~ c("Revised in version 5" = -2),
                   type == "revised_6" ~ c("Revised in version 6" = -2),
                   type == "revised-7" ~ c("Revised in version 7" = -2),
                   type == "revised_8" ~ c("Revised in version 8" = -2),
                   type == "revised_9" ~ c("Revised in version 9" = -2),
                   type == "revised_10" ~ c("Revised in version 10" = -2),
                   type == "revised_11" ~ c("Revised in version 11" = -2),
                   type == "revised_12" ~ c("Revised in version 12" = -2),
                   type == "removed_2" ~ c("Removed in version 2" = -3),
                   type == "removed_3" ~ c("Removed in version 3" = -3),
                   type == "removed_4" ~ c("Removed in version 4" = -3),
                   type == "removed_5" ~ c("Removed in version 5" = -3),
                   type == "removed_6" ~ c("Removed in version 6" = -3),
                   type == "removed_7" ~ c("Removed in version 7" = -3),
                   type == "removed_8" ~ c("Removed in version 8" = -3),
                   type == "removed_9" ~ c("Removed in version 9" = -3),
                   type == "removed_10" ~ c("Removed in version 10" = -3),
                   type == "removed_11" ~ c("Removed in version 11" = -3),
                   type == "removed_12" ~ c("Removed in version 12" = -3),
                   type == "year_gp_8" ~ c("Only shown to year 8" = -4),
                   type == "year_gp_9" ~ c("Only shown to year 9" = -4),
                   type == "year_gp_10" ~ c("Only shown to year 10" = -4),
                   type == "on_only" ~ c("Not present in offline version" = -5),
                   type == "off_only" ~ c("Not present in online version" = -5)
  )
                   
  if(is.na(lab)) stop(paste0("AoW missing value label type not found: ", type))
  
  return(lab)
  
}

aow_dict_colnames <- function() {
  
  dict_name <- c("variable", "form", "section", "type", "label", "categories", "note", "validation_type", "validation_min", "validation_max",
                 "identifier", "branching", "required", "alignment", "question_num", "matrix_name", "matrix_rank", "annotation")
  
  return(dict_name)
  
}

aow_redcap_cat_type <- function() {
  
  types <- c("radio", "checkbox", "yesno", "dropdown")
  
  return(types)
  
}

aow_redcap_txt_type <- function() {
  
  types <- c("text", "notes")
  
  return(types)
  
}

aow_survey_drop_cols <- function() {
  
  cols <- c("awb1_1_id",
            "redcap_survey_identifier",
            "module_2_timestamp",
            "module_2_complete",
            "end_submit",
            "aow_id",
            "date_time_collection",
            "birth_date"
  )
   
  return(cols)
  
}

aow_survey_column_order <- function() {
  
  cols <- c("aow_person_id",
            "aow_recruitment_id",
            "recruitment_era",
            "age_recruitment_y",
            "age_recruitment_m",
            "gender",
            "ethnicity",
            "birth_year",
            "birth_month",
            "school_id",
            "year_group",
            "form_tutor_id",
            "age_survey_y",
            "age_survey_m",
            "survey_date",
            "survey_version",
            "survey_mode"
  )
  
  return(cols)
  
}


# process redcap data dictionaries
aow_add_dict_cols <- function(df) {
  
  df <- df |> mutate(added = case_when(grepl(aow_srv_regexp("add_rad"), variable) ~ substr(variable, nchar(variable), nchar(variable)),
                                                         grepl(aow_srv_regexp("add_chk"), variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                                       revised = case_when(grepl(aow_srv_regexp("rev_rad"), variable) ~ substr(variable, nchar(variable), nchar(variable)),
                                                           grepl(aow_srv_regexp("rev_chk"), variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                                       hidden = case_when(grepl("hidden", note, ignore.case = TRUE) ~ str_extract(note, "[\\d]+")),
                                       year_group = case_when(grepl("year_group", branching) ~ str_extract(branching, "[\\d]+"))
  )
  
  return(df)
  
}

# add column
aow_add_col <- function(df, name = "dummy_col", type = "int") {
  
  # if name already exists, do nothing
  if(name %in% names(df)) return(df)

  if(name == "dummy_col") warning("Using default name for added column: `dummy_col`")
  
  name <- sym(name)
  
  if(type == "int") {
    df <- df |> mutate(!!name := as.integer(NA))
  }
  
  return(df)
  
}


# add missing data indicators and labels
aow_miss_cat_offline <- function(df, var) {
  
  # add col if it doesn't exist already
  df <- df |> aow_add_col(var)
  
  var <- sym(var)
  df <- df |> mutate(!!var := case_when(survey_mode == 1 ~ aow_miss_label("off_only"),
                                        TRUE ~ !!var))
  df <- df |> add_value_labels(!!var := aow_miss_label("off_only"))
  
  return(df)
  
}

aow_miss_cat_online <- function(df, var) {
  
  # add col if it doesn't exist already
  df <- df |> aow_add_col(var)
  
  var <- sym(var)
  df <- df |> mutate(!!var := case_when(survey_mode == 2 ~ aow_miss_label("on_only"),
                                        TRUE ~ !!var))
  df <- df |> add_value_labels(!!var := aow_miss_label("on_only"))
  
  return(df)
  
}

aow_miss_txt_offline <- function(df, var) {
  
  var <- sym(var)
  df <- df |> mutate(!!var := as.character(!!var), # when all values are NA, can be imported as numeric
                     !!var := case_when(survey_mode == 1 & is.na(!!var) ~ paste0("[", names(aow_miss_label("off_only")) , "]"),
                                        TRUE ~ !!var))
  
  return(df)
  
}

aow_miss_txt_online <- function(df, var) {
  
  var <- sym(var)
  df <- df |> mutate(!!var := as.character(!!var), # when all values are NA, can be imported as numeric
                     !!var := case_when(survey_mode == 2 & is.na(!!var) ~ paste0("[", names(aow_miss_label("on_only")) , "]"),
                                        TRUE ~ !!var))
  
  return(df)
  
}

aow_miss_cat_yrgrp <- function(df, var, yrgrp) {
  
  # add col if it doesn't exist already
  df <- df |> aow_add_col(var)
  
  var <- sym(var)
  
  yrgrp_lab <- aow_miss_label(paste0("year_gp_", yrgrp))
                 
  df <- df |> mutate(!!var := case_when(as.integer(year_group) != as.integer(yrgrp) ~ yrgrp_lab,
                                        TRUE ~ !!var))
  df <- df |> add_value_labels(!!var := yrgrp_lab)
  
  return(df)
  
}

aow_miss_txt_yrgrp <- function(df, var, yrgrp) {

  var <- sym(var)
  df <- df |> mutate(!!var := as.character(!!var), # when all values are NA, can be imported as numeric
                     !!var := case_when(as.integer(year_group) != as.integer(yrgrp) ~ paste0("[", names(aow_miss_label(paste0("year_gp_", yrgrp))) , "]"),
                                        TRUE ~ !!var))
  
  return(df)
  
}


aow_miss_cat_added <- function(df, var, version) {
  
  # add col if it doesn't exist already
  df <- df |> aow_add_col(var)
  
  var <- sym(var)
  
  version_lab <- aow_miss_label(paste0("added_", version))
  
  df <- df |> mutate(!!var := case_when(as.integer(survey_version) < as.integer(version) ~ version_lab,
                                        TRUE ~ !!var))
  df <- df |> add_value_labels(!!var := version_lab)
  
  return(df)
  
}

aow_miss_txt_added <- function(df, var, version) {
  
  var <- sym(var)
  df <- df |> mutate(!!var := as.character(!!var), # when all values are NA, can be imported as numeric
                     !!var := case_when(as.integer(survey_version) < as.integer(version) ~ paste0("[", names(aow_miss_label(paste0("added_", version))) , "]"),
                                        TRUE ~ !!var))
  
  return(df)
  
}


aow_miss_cat_revised <- function(df, var, version) {
  
  # add col if it doesn't exist already
  df <- df |> aow_add_col(var)
  
  var <- sym(var)
  
  version_lab <- aow_miss_label(paste0("revised_", version))
  
  df <- df |> mutate(!!var := case_when(as.integer(survey_version) < as.integer(version) ~ version_lab,
                                        TRUE ~ !!var))
  df <- df |> add_value_labels(!!var := version_lab)
  
  return(df)
  
}

aow_miss_txt_revised <- function(df, var, version) {
  
  var <- sym(var)
  df <- df |> mutate(!!var := as.character(!!var), # when all values are NA, can be imported as numeric
                     !!var := case_when(as.integer(survey_version) < as.integer(version) ~ paste0("[", names(aow_miss_label(paste0("revised_", version))) , "]"),
                                        TRUE ~ !!var))
  
  return(df)
  
}


aow_miss_cat_removed <- function(df, var, version) {
  
  # add col if it doesn't exist already
  df <- df |> aow_add_col(var)
  
  var <- sym(var)
  
  version_lab <- aow_miss_label(paste0("removed_", version))
  
  df <- df |> mutate(!!var := case_when(as.integer(survey_version) >= as.integer(version) ~ version_lab,
                                        TRUE ~ !!var))
  df <- df |> add_value_labels(!!var := version_lab)
  
  return(df)
  
}

aow_miss_txt_removed <- function(df, var, version) {
  
  var <- sym(var)
  df <- df |> mutate(!!var := as.character(!!var), # when all values are NA, can be imported as numeric
                     !!var := case_when(as.integer(survey_version) >= as.integer(version) ~ paste0("[", names(aow_miss_label(paste0("removed_", version))) , "]"),
                                        TRUE ~ !!var))
  
  return(df)
  
}
