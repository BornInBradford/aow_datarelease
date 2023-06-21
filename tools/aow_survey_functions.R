# packages
library(tidyverse)
library(haven)
library(labelled)

# set values that need to be controlled across survey scripts
aow_srv_regexp <- function(type) {
  
  exp <- case_when(type == "rev_cat" ~ "_r[0-9]{1,2}$|_r[0-9]{1,2}___[0-9]{1,3}$",
                   type == "rev_rad" ~ "_r[0-9]{1,2}$",
                   type == "rev_chk" ~ "_r[0-9]{1,2}___[0-9]{1,3}$",
                   type == "add_cat" ~ "_a[0-9]{1,2}$|_a[0-9]{1,2}___[0-9]{1,3}$",
                   type == "add_rad" ~ "_a[0-9]{1,2}$",
                   type == "add_chk" ~ "_a[0-9]{1,2}___[0-9]{1,3}$")
  
  if(is.na(exp)) stop(paste0("AoW survey regular expression type not found: ", type))
  
  return(exp)
  
}


aow_miss_label <- function(type) {
  
  lab <- case_when(type == "added" ~ c("Added in version " = -1),
                   type == "revised" ~ c("Revised in version " = -2),
                   type == "removed" ~ c("Removed in version " = -3),
                   type == "year_gp_8" ~ c("Only shown to year 8" = -4),
                   type == "year_gp_9" ~ c("Only shown to year 9" = -4),
                   type == "year_gp_10" ~ c("Only shown to year 10" = -4),
                   type == "on_only" ~ c("Not present in offline version" = -5),
                   type == "off_only" ~ c("Not present in online version" = -5),
                   type == "chkbox" ~ c("Checkbox question placeholder" = -101)
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


