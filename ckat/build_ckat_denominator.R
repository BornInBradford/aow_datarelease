# ckat denominator cleaning

library(tidyr)
library(dplyr)

# get denominator
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

ckat_input <- file.path("U:/Born in Bradford - AOW Raw Data/sql/ckat/data")

participants <- readRDS(file.path(ckat_input, "ParticipantInfo.rds"))

# duplicates check
participants |>
  dplyr::summarise(n = dplyr::n(), .by = c(FilePath, UploadTime, variable)) |>
  dplyr::filter(n > 1L) 

# drop "empty" variable names as we get a duplicate on this
participants <- participants |> filter(variable != "empty")

# duplicates check
participants |>
  dplyr::summarise(n = dplyr::n(), .by = c(FilePath, UploadTime, variable)) |>
  dplyr::filter(n > 1L) 

# do initial pivot
participants <- participants |> pivot_wider(id_cols = c("FilePath", "UploadTime"), names_from = "variable", values_from = "value")

# format AOW_ID
participants <- participants |> mutate(AOW_ID = gsub("[^aowAOW0-9]", "", AOW_ID),
                                      AOW_ID = tolower(AOW_ID))
# do linkage
unlinked_records <- participants |> anti_join(denom, by = c("AOW_ID" = "aow_recruitment_id"))
participants <- denom |> inner_join(participants, by = c("aow_recruitment_id" = "AOW_ID"))

# still getting some duplicates that appear to be due to data being uploaded twice
participants |> duplicated() |> which() |> length()
participants |> select(-UploadTime) |> duplicated() |> which() |> length()

# take the first recorded upload where duplicated
participants <- participants |> arrange(UploadTime)
participants <- participants[!(participants |> select(-UploadTime) |> duplicated()), ]

