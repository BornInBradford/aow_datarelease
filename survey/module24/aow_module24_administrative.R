# Module 24 survey administrative variables

source("tools/aow_survey_functions.R")

mod_allcols <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module24_merged.rds")

denom <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/denom/data/denom_identifiable.rds")

denom <- denom |> select(aow_person_id,
                         BiBPersonID,
                         is_bib,
                         aow_recruitment_id,
                         birth_date,
                         birth_year,
                         birth_month,
                         recruitment_era,
                         age_recruitment_y,
                         age_recruitment_m,
                         school_id,
                         form_tutor_id,
                         gender,
                         ethnicity_1,
                         ethnicity_2)

data_not_in_denom <- mod_allcols |> anti_join(denom, by = "aow_recruitment_id")

mod_allcols <- mod_allcols |> inner_join(denom, by = "aow_recruitment_id") |>
  mutate(survey_date = coalesce(as.Date(date_time_collection), 
                                as.Date(ifelse(module_1_timestamp == "[not completed]", NA, module_1_timestamp)),
                                as.Date(survey_date)),
         age_survey_y = (birth_date %--% survey_date) %/% years(1),
         age_survey_m = (birth_date %--% survey_date) %/% months(1))

# count !NAs to help deduplicate
mod_allcols$valid_values = rowSums(!is.na(mod_allcols)) # much faster than using rowwise()

# deduplicate
# - prefer records with dates
# - select most complete row
mod_allcols <- mod_allcols |> group_by(aow_recruitment_id) |>
  slice_max(n = 1, order_by = valid_values, with_ties = FALSE) |>
  ungroup()
  
mod_select <- mod_allcols |> 
  select(-any_of(aow_survey_drop_cols())) |>
  select(any_of(aow_survey_column_order()), everything())

# export
saveRDS(mod_select, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module24_linked.rds")
saveRDS(data_not_in_denom, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module24_notlinked.rds")



  