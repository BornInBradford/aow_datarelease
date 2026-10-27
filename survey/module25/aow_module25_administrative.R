# Module 25 survey administrative variables

source("tools/aow_survey_functions.R")

mod_allcols <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module25_merged.rds")

denom <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/denom/data/denom_identifiable.rds")

# Set expected survey start and end date to prevent survey period from going out of bounds
collection_start <- as.Date("2025-09-01")
collection_end <- as.Date("2026-08-31")

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
                         ethnicity_2
                         )

data_not_in_denom <- mod_allcols |> anti_join(denom, by = "aow_recruitment_id")

# Function to remove survey collection date outliers
rm_survey_date_outlier <- function(x) {
  x <- as.Date(x)
  if_else(is.na(x) | x < collection_start | x > collection_end, as.Date(NA), x)
}

mod_allcols <- mod_allcols |> 
  inner_join(denom, by = "aow_recruitment_id") |>
  mutate(date_collection = as.Date(date_time_collection),
         date_timestamp = as.Date(na_if(module_1_timestamp, "[not completed]")),
         date_existing = as.Date(survey_date),
         survey_date = coalesce(rm_survey_date_outlier(date_collection),
                                rm_survey_date_outlier(date_timestamp),
                                rm_survey_date_outlier(date_existing)),
         age_survey_y = (birth_date %--% survey_date) %/% years(1),
         age_survey_m = (birth_date %--% survey_date) %/% months(1))

# Check the records with any survey date outlier
survey_date_outlier <- mod_allcols |>
  filter((!is.na(date_collection) & is.na(rm_survey_date_outlier(date_collection))) |
           (!is.na(date_timestamp) & is.na(rm_survey_date_outlier(date_timestamp))) |
           (!is.na(date_existing) & is.na(rm_survey_date_outlier(date_existing)))) |>
  select(aow_recruitment_id, survey_mode, date_collection, date_timestamp,
         date_existing, survey_date)

message(nrow(survey_date_outlier), " records had at least one date outside the collection period")

# how many are missing a survey date?
missdate <- length(which(is.na(mod_allcols$survey_date)))
missdate

# drop these if small
if(missdate < 50) {
  
  mod_allcols <- mod_allcols |> filter(!is.na(survey_date))
  
} else {
  
  stop("Too many records are missing dates: n missing = ", missdate)
  
}


# count !NAs to help deduplicate
mod_allcols$valid_values = rowSums(!is.na(mod_allcols)) # much faster than using rowwise()

# deduplicate
# - select most complete row
mod_allcols <- mod_allcols |> group_by(aow_recruitment_id) |>
  slice_max(n = 1, order_by = valid_values, with_ties = FALSE) |>
  ungroup()
  
mod_select <- mod_allcols |> 
  select(-any_of(aow_survey_drop_cols())) |>
  select(any_of(aow_survey_column_order()), everything())

mod_select <- mod_select |> select(-record, -record_id)

# export
saveRDS(mod_select, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module25_linked.rds")
saveRDS(data_not_in_denom, "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module25_notlinked.rds")


  