# packages
library(tidyverse)
library(haven)
library(labelled)
library(lubridate)
library(stringr)
library(purrr)

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
                   type == "revised_7" ~ c("Revised in version 7" = -2),
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

aow_redcap_chk_type <- function() {
  
  types <- c("checkbox")
  
  return(types)
  
}

aow_redcap_txt_type <- function() {
  
  types <- c("text", "notes")
  
  return(types)
  
}

aow_denom_col_labels <- function(full = TRUE) {
  
  col_labs <- list(aow_person_id = "Age of Wonder person ID",
                   BiBPersonID = "BiB cohort person ID",
                   is_bib = "Participant is in original BiB cohort",
                   aow_recruitment_id = "Age of Wonder year group recruitment ID",
                   birth_year = "Year of birth",
                   birth_month = "Month of birth",
                   recruitment_era = "Recruitment era (academic year)",
                   age_recruitment_y = "Age at recruitment in years",
                   age_recruitment_m = "Age at recruitment in months",
                   school_id = "Pseudo school ID",
                   year_group = "Year group at recruitment",
                   form_tutor_id = "Pseudo recruitment form tutor ID",
                   gender = "Gender reported by school",
                   ethnicity_1 = "Ethnicity reported by school - higher level category",
                   ethnicity_2 = "Ethnicity reported by school - lower level category")
  
  col_labs_full <- list(upn = "Unique Pupil Number",
                        birth_date = "Date of birth",
                        postcode = "Home postcode",
                        LSOA11CD = "Home LSOA code, 2011 boundaries",
                        IMD_2019_score = "Home IMD 2019 score",
                        IMD_2019_decile = "Home IMD 2019 decile, national scale",
                        recruitment_date = "Recruitment date (import of class list)",
                        recruitment_year = "Recruitment year",
                        recruitment_month = "Recruitment month",
                        school_establishment_no = "School local authority establishment number",
                        school = "School name",
                        form_tutor = "Form tutor at recruitment",
                        fsm = "Free school meals",
                        sen = "Special educational needs provision")
  
  if(full) col_labs <- append(col_labs, col_labs_full)
  
  return(col_labs)
  
}

aow_survey_process_labels <- function() {
  
  col_labs <- list(age_survey_y = "Age (years) at survey date",
                   age_survey_m = "Age (months) at survey date",
                   survey_date = "Date survey taken",
                   survey_version = "Survey version",
                   survey_mode = "Survey taken online or offline?"
                   )
  
  return(col_labs)
  
}

aow_survey_drop_cols <- function() {
  
  cols <- c("awb1_1_id",
            "redcap_survey_identifier",
            "module_1_timestamp",
            "module_1_complete",
            "module_2_timestamp",
            "module_2_complete",
            "module_3_timestamp",
            "module_3_complete",
            "module_4_timestamp",
            "module_4_complete",
            "end_submit",
            "aow_id",
            "date_time_collection",
            "birth_date",
            "start_time", 
            "first_submit_time", 
            "completion_time",
            "valid_values"
  )
   
  return(cols)
  
}

aow_survey_admin_cols <- function() {
  
  cols <- c("aow_id", 
            "date_time_collection",
            "year_group"
  )
  
  return(cols)
  
}

aow_survey_column_order <- function() {
  
  cols <- c("aow_person_id",
            "BiBPersonID",
            "is_bib",
            "aow_recruitment_id",
            "recruitment_era",
            "age_recruitment_y",
            "age_recruitment_m",
            "gender",
            "ethnicity_1",
            "ethnicity_2",
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

aow_pre_mod_drop_cols <- function() {
  
  cols <- c("aow_person_id",
            "BiBPersonID",
            "is_bib",
            "recruitment_era",
            "age_recruitment_y",
            "age_recruitment_m",
            "gender",
            "ethnicity_1",
            "ethnicity_2",
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


aow_pre_mod_admin_data <- function(pre_mod_admin) {
  
  pre_mod_admin <- pre_mod_admin |>
    group_by(aow_recruitment_id) |>
    summarise(aow_person_id = ifelse(all(is.na(aow_person_id)), NA, min(aow_person_id, na.rm = TRUE)),
              BiBPersonID = ifelse(all(is.na(BiBPersonID)), NA, min(BiBPersonID, na.rm = TRUE)),
              is_bib = ifelse(all(is.na(is_bib)), NA, min(is_bib, na.rm = TRUE)),
              recruitment_era = ifelse(all(is.na(recruitment_era)), NA, min(recruitment_era, na.rm = TRUE)),
              age_recruitment_y = ifelse(all(is.na(age_recruitment_y)), NA, min(age_recruitment_y, na.rm = TRUE)),
              age_recruitment_m = ifelse(all(is.na(age_recruitment_m)), NA, min(age_recruitment_m, na.rm = TRUE)),
              gender = ifelse(all(is.na(gender)), NA, min(gender, na.rm = TRUE)),
              ethnicity_1 = ifelse(all(is.na(ethnicity_1)), NA, min(ethnicity_1, na.rm = TRUE)),
              ethnicity_2 = ifelse(all(is.na(ethnicity_2)), NA, min(ethnicity_2, na.rm = TRUE)),
              birth_year = ifelse(all(is.na(birth_year)), NA, min(birth_year, na.rm = TRUE)),
              birth_month = ifelse(all(is.na(birth_month)), NA, min(birth_month, na.rm = TRUE)),
              school_id = ifelse(length(unique(school_id)) > 1, "various - see module specific data", min(school_id, na.rm = TRUE)),
              year_group = ifelse(length(unique(year_group)) > 1, -1, min(year_group, na.rm = TRUE)),
              form_tutor_id = ifelse(length(unique(form_tutor_id)) > 1, "various - see module specific data", min(form_tutor_id, na.rm = TRUE)),
              age_survey_y = min(age_survey_y, na.rm = TRUE),
              age_survey_m = min(age_survey_m, na.rm = TRUE),
              survey_date = min(survey_date, na.rm = TRUE),
              survey_version = ifelse(all(is.na(survey_version)), NA, ifelse(length(unique(survey_version)) > 1, -1, min(survey_version, na.rm = TRUE))),
              survey_mode = ifelse(length(unique(survey_mode)) > 1, 3, min(survey_mode, na.rm = TRUE))) |>
    ungroup() |>
    add_value_labels(survey_mode = c(Online = 1, Offline = 2, Mixed = 3),
                     survey_version = c("v01" = 1,
                                        "v02" = 2,
                                        "v03" = 3,
                                        "v04" = 4,
                                        "v05" = 5,
                                        "v06" = 6,
                                        "v07" = 7,
                                        "v08" = 8,
                                        "v09" = 9,
                                        "v10" = 10,
                                        "v11" = 11,
                                        "v12" = 12,
                                        "v13" = 13,
                                        "v14" = 14,
                                        "v15" = 15,
                                        "v16" = 16,
                                        "v17" = 17,
                                        "v18" = 18,
                                        "v19" = 19,
                                        "v20" = 20,
                                        "various - see module specific data" = -1)
    ) |>
    set_variable_labels(.labels = aow_denom_col_labels(full = FALSE)) 
  
  return(pre_mod_admin)

}

# process redcap data dictionaries
aow_add_dict_cols <- function(df) {
  
  df <- df |> mutate(added = case_when(grepl(aow_srv_regexp("add_rad"), variable) ~ str_split_i(variable, "_", -1) |> str_replace("\\D*", ""),
                                       grepl(aow_srv_regexp("add_chk"), variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                     revised = case_when(grepl(aow_srv_regexp("rev_rad"), variable) ~ str_split_i(variable, "_", -1) |> str_replace("\\D*", ""),
                                         grepl(aow_srv_regexp("rev_chk"), variable) ~ str_split(variable, "_") |> tail(n=4) |> head(n=1) |> str_replace("\\D*", "")),
                     hidden = case_when(grepl("hidden", note, ignore.case = TRUE) ~ str_extract(note, "[\\d]+")),
                     year_group = case_when(grepl("year_group", branching) ~ str_extract(branching, "[\\d]+"))
  )
  
  return(df)
  
}

# add suffix to named variable in data dictionary
aow_suffix_var_dict <- function(df, var, suffix) {
  
  suffix <- tolower(suffix)
  
  if(!var %in% df$variable) stop(paste0(var, " not found in data dictionary df"))
  
  newvar <- paste0(var, "_", suffix)
  
  df <- df |> mutate(variable = if_else(variable == var, newvar, variable))
  
  return(df)
  
}

# add suffix to named variable in data frame
aow_suffix_var <- function(df, var, suffix) {
  
  suffix <- tolower(suffix)
  
  if(!var %in% names(df)) stop(paste0(var, " not found in data frame"))
  
  newvar <- paste0(var, "_", suffix)
  
  var <- sym(var)
  
  df <- df |> rename(!!newvar := !!var)
  
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


# modify metadata

# add checkbox labels
aow_label_chk <- function(df, var, labels) {
  
  # check for online/offline pre-processing and remove for now
  if(grepl("_on$", var)) var <- substr(var, 1, nchar(var) - 3)
  if(grepl("_off$", var)) var <- substr(var, 1, nchar(var) - 4)
  
  for(label in labels) {
    
    lab_var <- paste0(var, "___", label[1])
    
    if(lab_var %in% names(df)) {
      lab_var <- sym(lab_var)
      new_label <- c(0L,1L)
      names(new_label) <- c("Unchecked", paste0("Checked: ", label[2]))
      df <- df |> set_value_labels(!!lab_var := new_label)
    } else {
      warning(paste0("Variable ", lab_var, " not found in data frame for checkbox labelling"))
    }
      
  }
  
  return(df)
  
}


# only keep vars in dict that are expected to have data in this version range

aow_trim_var_versions <- function(dict, min_version, max_version) {
  
  anti_dict <- dict |> aow_out_of_vrange(min_version, max_version)
  
  dict <- dict |> anti_join(anti_dict)
  
  return(dict)
  
}


# only keep vars in data that are expected to have data in version range
# needs data dict vars

aow_trim_var_versions_data <- function(dat, dict, min_version, max_version) {
  
  anti_dict <- dict |> aow_out_of_vrange(min_version, max_version)
  
  anti_dict_chk <- anti_dict |> filter(type %in% aow_redcap_chk_type()) |>
    pull(variable)
  
  anti_dict <- anti_dict |> filter(!type %in% aow_redcap_chk_type()) |>
    pull(variable)
  
  anti_dict_chk <- paste0(anti_dict_chk, "___")
  
  dat <- dat |> select(-any_of(anti_dict))
  
  dat <- dat |> select(-any_of(starts_with(anti_dict_chk)))
  
  return(dat)
  
}

# find vars in data dict that are out of version range

aow_out_of_vrange <- function(dict, min_version, max_version) {
  
  dict <- dict |> filter(as.numeric(added) > max_version |
                         as.numeric(revised) > max_version |
                         as.numeric(hidden) <= min_version)
  
  return(dict)
  
}


aow_version_by_date <- function(timestamp, launch_dates) {

  version <- character(0)
  
  for(d in 1:length(launch_dates)) {
    
    version <- ifelse(timestamp >= launch_dates[d], names(launch_dates[d]), version)
    
  }

  return(as.numeric(version))
 
}


aow_output_varlabels <- function(df, output = c("cat", "clip", "c")) {
  
  labs <- sapply(var_label(df), function(x) ifelse(is.null(x), "", x))
  
  labs <- paste0(names(labs), " = ", "\"", labs, "\",")

  if(output[1] == "cat") return(cat(paste0(labs, collapse = "\n")))
  if(output[1] == "c") return(labs)
  if(output[1] == "clip") writeClipboard(labs)
  
}




