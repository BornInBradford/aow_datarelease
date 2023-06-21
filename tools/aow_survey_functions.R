# packages
library(tidyverse)
library(haven)
library(labelled)

aow_srv_regexp <- function(type = character(0)) {
  
  exp <- case_when(type == "rev_cat" ~ "_r[0-9]{1,2}$|_r[0-9]{1,2}___[0-9]{1,3}$",
                   type == "rev_rad" ~ "_r[0-9]{1,2}$",
                   type == "rev_chk" ~ "_r[0-9]{1,2}___[0-9]{1,3}$",
                   type == "add_cat" ~ "_a[0-9]{1,2}$|_a[0-9]{1,2}___[0-9]{1,3}$",
                   type == "add_rad" ~ "_a[0-9]{1,2}$",
                   type == "add_chk" ~ "_a[0-9]{1,2}___[0-9]{1,3}$")
  
  if(is.na(exp)) stop(paste0("AoW survey regular expression type not found: ", type))
  
  return(exp)
  
}


aow_miss_label <- function(type = character(0)) {
  
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

aow_miss_radio_offline <- function(df, var) {
  
  var <- sym(var)
  df <- df |> mutate(!!var := case_when(survey_mode == 1 ~ aow_miss_label("off_only"),
                                        TRUE ~ !!var))
  df <- df |> add_value_labels(!!var := aow_miss_label("off_only"))
  
  return(df)
  
}

aow_miss_radio_online <- function(df, var) {
  
  var <- sym(var)
  df <- df |> mutate(!!var := case_when(survey_mode == 2 ~ aow_miss_label("on_only"),
                                        TRUE ~ !!var))
  df <- df |> add_value_labels(!!var := aow_miss_label("on_only"))
  
  return(df)
  
}

aow_miss_checkbox_offline <- function(df, var) {
  
  var <- sym(var)
  df <- df |> mutate(!!var := case_when(survey_mode == 1 ~ aow_miss_label("off_only")))
  df <- df |> add_value_labels(!!var := aow_miss_label("off_only"))
  
  return(df)
  
}

aow_miss_checkbox_online <- function(df, var) {
  
  var <- sym(var)
  df <- df |> mutate(!!var := case_when(survey_mode == 2 ~ aow_miss_label("on_only")))
  df <- df |> add_value_labels(!!var := aow_miss_label("on_only"))
  
  return(df)
  
}

aow_miss_text_offline <- function(df, var) {
  
  var <- sym(var)
  df <- df |> mutate(!!var := case_when(survey_mode == 1 & is.na(!!var) ~ paste0("[", names(aow_miss_label("off_only")) , "]"),
                                        TRUE ~ !!var))
  
  return(df)
  
}

aow_miss_text_online <- function(df, var) {
  
  var <- sym(var)
  df <- df |> mutate(!!var := case_when(survey_mode == 2 & is.na(!!var) ~ paste0("[", names(aow_miss_label("on_only")) , "]"),
                                        TRUE ~ !!var))
  
  return(df)
  
}

aow_miss_radio_yrgrp <- function(df, var, yrgrp) {

  var <- sym(var)
  
  df <- df |> mutate(!!var := case_when(year_group != yrgrp ~ aow_miss_label(paste0("year_gp_", yrgrp)),
                                        TRUE ~ !!var))
  df <- df |> add_value_labels(!!var := yrgrp_lab)
  
  return(df)
  
}

aow_miss_checkbox_yrgrp <- function(df, var, yrgrp) {
  
  # Does this need to deal with the fact that the variable may or may not already exist?
  # Need a function to create it?
  # Then add TRUE condition back into checkbox function above - in fact can probably use radio functions again
  # or grab radio and checkbox types together?
  
}

aow_miss_text_yrgrp <- function(df, var) {

  var <- sym(var)
  df <- df |> mutate(!!var := case_when(year_group != yrgrp ~ paste0("[", names(aow_miss_label(paste0("year_gp_", yrgrp))) , "]"),
                                        TRUE ~ !!var))
  
  return(df)
  
}


