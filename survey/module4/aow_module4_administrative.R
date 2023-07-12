# Module 4 survey administrative variables

source("tools/aow_survey_functions.R")

mod_allcols <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module4_merged.rds")

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
                         ethnicity)

data_not_in_denom <- mod_allcols |> anti_join(denom, by = "aow_recruitment_id")

mod_allcols <- mod_allcols |> inner_join(denom, by = "aow_recruitment_id") |>
  mutate(survey_date = coalesce(date_time_collection, module_4_timestamp, survey_date) |> 
           as.Date(),
         age_survey_y = (birth_date %--% survey_date) %/% years(1),
         age_survey_m = (birth_date %--% survey_date) %/% months(1))

# count !NAs to help deduplicate
mod_allcols <- mod_allcols |>
  rowwise() |>
  mutate(valid_values = sum(!is.na(across(everything())))) |>
  ungroup()

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
saveRDS(mod_select, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module4_linked.rds")
saveRDS(data_not_in_denom, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module4_notlinked.rds")



  