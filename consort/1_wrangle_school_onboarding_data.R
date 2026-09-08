

# school contact
# little info on dates
# can be used to get a list of contacted schools
# assume presence of record indicates contact made?
# ID: school_fcon_r2

# ht meeting
# ID: school_meethead_r2
# outcome_meethead: 1=Proceed, 2=Decline, 3=Other (not always completed)
# date patchy, could use:
#   - year_meethead_a4
#   - date_meethead
# and would have to backfill the categorical from the date, would still be gaps
# are there even dupes?
# does this add anything? if we have info in school_background and no records of consent discussion, what does knowing we contacted add?

# school info
# assume this is complete, all targeted schools, but test this against data collected and check with team
# ID: school_name_r3
# school_year
# looks like record can be dropped if no school_year as it's not completed - check this
# std_n_y8, std_n_y9, std_n_y10 <- number enrolled

# if this all checks out, school_info gives us total schools and total on roll each year
# - can take the first and work out e.g. assume Y8s all go into Y9 in same school next year

# school consent
# msr_withdrawn_consent__x <- optout from certain measures
# x=1: survey
# x=2: cog
# x=3: ht/wt
# x=5: bp
# x=6: bioimp
# x=7: skinfold
# x=8: bloods


library(dplyr)
library(tidyr)
library(labelled)

output_dir <- "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/school_onboarding/data/"

sch_consent <- readRDS("U:/Born in Bradford - AOW Raw Data/redcap/school_onboarding/data/school_consent_records.rds")
sch_info <- readRDS("U:/Born in Bradford - AOW Raw Data/redcap/school_onboarding/data/school_background_records.rds")
sch_contact <- readRDS("U:/Born in Bradford - AOW Raw Data/redcap/school_onboarding/data/school_contact_records.rds")
ht_meeting <- readRDS("U:/Born in Bradford - AOW Raw Data/redcap/school_onboarding/data/ht_meeting_records.rds")


# prepare school consent info
sch_consent <- sch_consent |> select(id = school_consent_r2,
                                     year = year_consent,
                                     ht_agreed = headcon_consent,
                                     dsa_optout = dsa_out_consent,
                                     dsa_optin = dsa_in_consent,
                                     starts_with("msr_withdraw_consent")) |>
  mutate(dsa_optout = ifelse(is.na(dsa_optout), 0, dsa_optout),
         dsa_optin = ifelse(is.na(dsa_optin), 0, dsa_optin),
         
         dsa = ifelse(dsa_optout == 0 & dsa_optin == 0, 0, 1), .after = dsa_optin,
         ht_agreed = ifelse(is.na(ht_agreed), 0, ht_agreed)) |>
  filter(!is.na(id) & !is.na(year)) |> 
  unique() 

# deal with dupes, keep "most consented" row per school+year
sch_consent <- sch_consent |>
  group_by(id, year) |>
  filter(n() == 1 | n() > 1 & ht_agreed + dsa_optout + dsa_optin == max(ht_agreed + dsa_optout + dsa_optin)) |>
  ungroup()
  

# some checks on which schools appear in each table
table(!unique(sch_consent$id) %in% unique(sch_contact$school_fcon_r2))
table(!unique(sch_contact$school_fcon_r2) %in% unique(sch_consent$id))
table(!unique(sch_contact$school_fcon_r2) %in% unique(sch_info$school_name_r3))
table(!unique(sch_consent$id) %in% unique(sch_info$school_name_r3))
which(!unique(sch_contact$school_fcon_r2) %in% unique(sch_info$school_name_r3))
unique(sch_contact$school_fcon_r2)[which(!unique(sch_contact$school_fcon_r2) %in% unique(sch_info$school_name_r3))]

# sch_info has all schools in sch_contact except 7030 about which we have no info in any other table
# given the lack of useful info in sch_contact, ignore this table
# use sch_info combined with sch_consent to get school recruitment denominator
# check ht_meeting for additional info

table(!unique(sch_info$school_name_r3) %in% unique(sch_consent$id))
unique(sch_info$school_name_r3)[which(!unique(sch_info$school_name_r3) %in% unique(sch_consent$id))]
table(!unique(sch_consent$id) %in% unique(sch_info$school_name_r3))
unique(sch_consent$id)[which(!unique(sch_consent$id) %in% unique(sch_info$school_name_r3))]
table(!unique(ht_meeting$school_meethead_r2) %in% unique(sch_info$school_name_r3))
unique(ht_meeting$school_meethead_r2)[which(!unique(ht_meeting$school_meethead_r2) %in% unique(sch_info$school_name_r3))]

# sch_info can be the denominator for schools, other tables adding little apart
# from sch_consent, which is necessary for tracking final agreements
# there may be a slightly larger group of "all eligible schools" some of which
# were tacitly ruled out due to structural reasons e.g. time or logistics of 
# getting there, especially those with smaller numbers of BiB kids
# if we want to work out success at recruiting BiB kids we should take this 
# into account somehow
# look at known BiB schools in new education data for study years? and limit
# geographically?


# can the year variables be used for matching?
str(sch_info$school_year)
str(sch_consent$year)
table(sch_info$school_year, useNA = "always")
table(sch_consent$year, useNA = "always")
labelled::val_labels(sch_consent$year)

# there are a couple of consents for 26-27 so exclude these for now


# try inner join of consent onto school info based on school id and year

# first tidy

sch_info <- sch_info |> select(id = school_name_r3,
                               year = school_year,
                               n_y8 = std_n_y8,
                               n_y9 = std_n_y9,
                               n_y10 = std_n_y10) |>
  filter(!is.na(id) & id != 8004 & !is.na(year))  |> # remove blank school ids, years and Bradford College
  filter(!(is.na(n_y8) & is.na(n_y9) & is.na(n_y10))) |> # remove blank roll numbers
  unique()

# duplicates?
sch_info_d <- sch_info |> 
  group_by(id, year) |>
  mutate(n = n()) |> 
  ungroup()

any(sch_info_d$n > 1)

# join
sch_rec <- sch_info |> left_join(sch_consent, by = c("id", "year"))

# replace all NAs with zeroes
sch_rec[is.na(sch_rec)] <- 0


# we've got nearly a complete record for every school in every year
table(sch_rec$id, sch_rec$year)

# recode the year to its labels
sch_rec <- sch_rec |> mutate(year = case_match(year,
                                               2 ~ "2022-23",
                                               3 ~ "2023-24",
                                               4 ~ "2024-25",
                                               5 ~ "2025-26"))

# attach data availability indicator
denom <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/denom/data/denom_identifiable.rds")

sch_data <- denom |> transmute(id = school_establishment_no,
                               year_group = year_group,
                               year = recruitment_era,
                               received = 1,
                               received_new = ifelse(received == 1 & rep_received == 1, 1, 0),
                               data = has_data,
                               data_new = ifelse(has_data == 1 & rep_has_data == 1, 1, 0)) |>
  filter(year != "2021-22") |> # ignore data collected in pilot year
  group_by(id, year_group, year) |>
  summarise(received = sum(received), 
            received_new = sum(received_new),
            data = sum(data),
            data_new = sum(data_new)) |>
  ungroup() |>
  mutate(id = as.numeric(id)) |>
  pivot_wider(names_from = "year_group", 
              names_prefix = c("y"), 
              values_from = c("received", "received_new", "data", "data_new"), 
              values_fill = 0) |>
  mutate(err_received_new_y8 = ifelse(received_new_y8 > received_y8, TRUE, FALSE),
         err_received_new_y9 = ifelse(received_new_y9 > received_y9, TRUE, FALSE),
         err_received_new_y10 = ifelse(received_new_y10 > received_y10, TRUE, FALSE),
         err_data_new_y8 = ifelse(data_new_y8 > data_y8, TRUE, FALSE),
         err_data_new_y9 = ifelse(data_new_y9 > data_y9, TRUE, FALSE),
         err_data_new_y10 = ifelse(data_new_y10 > data_y10, TRUE, FALSE)
  )

# qc check
if(any(sch_data$err_received_new_y8,
       sch_data$err_received_new_y9,
       sch_data$err_received_new_y10,
       sch_data$err_data_new_y8,
       sch_data$err_data_new_y9,
       sch_data$err_data_new_y10
)) {
  stop("Some new entrant counts are higher than total received counts.")
}

sch_data <- sch_data |> select(-any_of(starts_with("err_")))

sch_rec <- sch_rec |> full_join(sch_data, by = c("id", "year")) |>
  relocate(starts_with(c("received", "data")), .after = "dsa")

# replace all NAs with zeroes again
#sch_rec[is.na(sch_rec)] <- 0
# changed to leave roll counts untouched so it's clear when zero is unknown
sch_rec <- sch_rec |>
  mutate(across(-all_of(starts_with("n_y")), ~ replace(.x, is.na(.x), 0)))


sch_rec <- sch_rec |> arrange(id, year)

# drop columns we're not currently using
sch_rec <- sch_rec |> select(-starts_with("dsa_"), -starts_with("msr_"))

saveRDS(sch_rec, paste0(output_dir, "aow_school_process.rds"))
