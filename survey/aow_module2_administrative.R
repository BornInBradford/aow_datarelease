# Module 2 survey administrative variables

source("tools/aow_survey_functions.R")

mod_allcols <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module2_merged.rds")

denom <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/denom/data/denom_identifiable.rds")

denom <- denom |> select(aow_person_id,
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

# tidy up recruitment ids
mod_allcols <- mod_allcols |> mutate(aow_recruitment_id = coalesce(aow_id, redcap_survey_identifier),
                                     aow_recruitment_id = gsub("[^aowAOW0-9]", "", aow_recruitment_id),
                                     aow_recruitment_id = tolower(aow_recruitment_id))

data_not_in_denom <- mod_allcols |> anti_join(denom, by = "aow_recruitment_id")

mod_allcols <- mod_allcols |> inner_join(denom, by = "aow_recruitment_id") |>
  mutate(survey_date = coalesce(date_time_collection, module_2_timestamp) |> as.Date(),
         age_survey_y = (birth_date %--% survey_date) %/% years(1),
         age_survey_m = (birth_date %--% survey_date) %/% months(1))
         
mod_select <- mod_allcols |> 
  select(-any_of(aow_survey_drop_cols())) |>
  select(any_of(aow_survey_column_order()), everything())

# export
saveRDS(mod_select, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module2_linked.rds")
saveRDS(data_not_in_denom, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module2_notlinked.rds")



  