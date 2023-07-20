
library(tidyverse)
library(haven)
library(dplyr)
library(lubridate)
library(readr)
library(labelled)
library(purrr)

# whether to export files
options(aow_export_denom = TRUE)

source("tools/aow_tools.R")

input_path <- "U:/Born in Bradford - AOW Raw Data/sql/denominator/data/"
output_path <- "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/denom/data/"


consent <- read_dta(file.path(input_path, "AOW_Consent.dta"))
schoolrec <- read_dta(file.path(input_path, "AOW_School_RecruitmentList.dta"))
cohort <- read_dta(file.path(input_path, "BiB_Cohort.dta"))

# check duplicates
consent$AoWRecruitmentID[duplicated(consent$AoWRecruitmentID) |> which()]
schoolrec$AoWRecruitmentID[duplicated(schoolrec$AoWRecruitmentID) |> which()]

# can they be joined one-to-one
nrow(consent)
nrow(schoolrec)
denom_all <- consent |> inner_join(schoolrec, by = "AoWRecruitmentID", suffix = c("_con", "_rec"))
nrow(denom_all)


# remove withdrawals
# these are withdrawn by school or parents AFTER data passed to BiB
# so not processed further
denom_all <- denom_all |> filter(is.na(Withdrawn)) |>
  select(-Withdrawn, -WithdrawnDate)


# check conflicts between school recruitment and consent tables
conflicts <- function(x, y, which = FALSE) {
  con <- which(x != y)
  output <- paste0(x[con], " : ", y[con])
  if(which) {
    return(con)
  } else {
    return(output)
  }
}

conflicts(denom_all$EstablishmentNumber_con, denom_all$EstablishmentNumber_rec) |> head()
conflicts(denom_all$School_con, denom_all$School_rec) |> head()
conflicts(denom_all$YearGroup_con, denom_all$YearGroup_rec) |> head()
conflicts(denom_all$FormTutor_con, denom_all$FormTutor_rec) |> head()
conflicts(denom_all$CreatedDateTime_con, denom_all$CreatedDateTime_rec) |> head()
conflicts(denom_all$ModifiedDateTime_con, denom_all$ModifiedDateTime_rec) |> head()


# create BiB lookup
bib_lkup <- cohort |> select(BiBPersonID = BIBPersonID, upn = UPN) |>
  filter(!is.na(upn) & nchar(upn) > 0)

# link BiB ID
denom_all <- denom_all |> left_join(bib_lkup, by = c("UPN_con" = "upn"))

# start sorting fields to keep and renaming, add pseudo columns

denom <- denom_all |>
  transmute(aow_person_id = map_chr(UPN_con, aow_pseudo),
            BiBPersonID,
            is_bib = case_when(!is.na(BiBPersonID) ~ 1, TRUE ~ 0),
            upn = UPN_con,
            aow_recruitment_id = gsub("[^aowAOW0-9]", "", AoWRecruitmentID) |> tolower(),
            birth_date = as.Date(DateOfBirth),
            birth_year = year(birth_date),
            birth_month = month(birth_date),
            postcode = Postcode,
            recruitment_era = Era,
            recruitment_date = as.Date(CreatedDateTime_rec),
            recruitment_year = year(recruitment_date),
            recruitment_month = month(recruitment_date),
            age_recruitment_y = (birth_date %--% recruitment_date) %/% years(1),
            age_recruitment_m = (birth_date %--% recruitment_date) %/% months(1),
            school_establishment_no = EstablishmentNumber_con,
            school = School_con,
            school_id = map_chr(EstablishmentNumber_con, aow_pseudo),
            year_group = YearGroup_rec,
            form_tutor = FormTutor_rec,
            form_tutor_id = map_chr(FormTutor_rec, aow_pseudo),
            gender = Gender,
            ethnicity = ethnicity_ons2,
            fsm = FSM,
            sen = SEN,
            consent_form = Consent_Form,
            consent_form_type = FormType,
            consent_scenario = ConScenario,
            consent_parental = ParConsent,
            consent_survey = ConQuest,
            consent_cogmot = ConMotCog,
            consent_hgtwgt = ConHgtWgt,
            consent_bioimp = ConBioImp,
            consent_sknthk = ConSkinThick,
            consent_bp = ConBP,
            consent_bloods = ConBlood,
            consent_bldst1 = ConBloodStor1,
            consent_bldst2 = ConBloodStor2)

# recoding and value labelling
denom <- denom |> 
  
  # gender
  mutate(gender = gender |> tolower() |> trimws(),
         gender = case_when(gender %in% c("m", "male") ~ 2L,
                            gender %in% c("f", "female") ~ 1L,
                            TRUE ~ as.integer(NA))) |>
  set_value_labels(gender = c(Female = 1, Male = 2)) |>
  
  # free shcool meals
  mutate(fsm = fsm |> tolower() |> trimws(),
         fsm = case_when(fsm %in% c("n", "no") ~ 0L,
                         fsm %in% c("y", "yes") ~ 1L,
                         TRUE ~ as.integer(NA))) |>
  set_value_labels(fsm = c(No = 0, Yes = 1)) |>
  
  # special educational needs
  mutate(sen = sen |> tolower() |> trimws(),
         sen = case_when(sen %in% c("n") ~ 0L,
                         sen %in% c("k") ~ 1L,
                         sen %in% c("e") ~ 2L,
                         TRUE ~ as.integer(NA))) |>
  # NB some schools have sent SEN type (e.g. "Severe Learning Difficulty")
  #    instead of provision as requested. Nullifying these while we find out
  #    whether there's any way to infer provision from type
  set_value_labels(sen = c("No special educational need" = 0, 
                           "Special educational need support" = 1,
                           "Education, Health and Care Plan" = 2)) |>
  
  # ethnicity
  mutate(ethnicity = case_when(ethnicity == "asian or asian british" ~ 1L,
                               ethnicity == "black or african or caribbean or black british" ~ 2L,
                               ethnicity == "mixed multiple ethnic groups" ~ 3L,
                               ethnicity %in% c("white", "roma ethnic group") ~ 4L,
                               ethnicity == "not stated" ~ 99L,
                               ethnicity == "other ethnic group" ~ 5L)) |>
  set_value_labels(ethnicity = c("Asian or Asian British" = 1,
                                 "Black, Black British, Caribbean or African" = 2,
                                 "Mixed or multiple ethnic groups" = 3,
                                 "White" = 4,
                                 "Other ethnic group" = 5,
                                 "Not stated" = 99))

  
  

# labelling variables
denom <- denom |> 
  set_variable_labels(aow_person_id = "Age of Wonder person ID",
                      BiBPersonID = "BiB cohort person ID",
                      is_bib = "Participant is in original BiB cohort",
                      upn = "Unique Pupil Number",
                      aow_recruitment_id = "Age of Wonder year group recruitment ID",
                      birth_date = "Date of birth",
                      birth_year = "Year of birth",
                      birth_month = "Month of birth",
                      postcode = "Home postcode",
                      recruitment_era = "Recruitment era (academic year)",
                      recruitment_date = "Recruitment date (import of class list)",
                      recruitment_year = "Recruitment year",
                      recruitment_month = "Recruitment month",
                      age_recruitment_y = "Age at recruitment in years",
                      age_recruitment_m = "Age at recruitment in months",
                      school_establishment_no = "School local authority establishment number",
                      school = "School name",
                      school_id = "Pseudo school ID",
                      year_group = "Year group at recruitment",
                      form_tutor = "Form tutor at recruitment",
                      form_tutor_id = "Pseudo recruitment form tutor ID",
                      gender = "Gender reported by school",
                      ethnicity = "Ethnicity reported by school",
                      fsm = "Free school meals",
                      sen = "Special educational needs provision",
                      consent_form = "Consent form type I",
                      consent_form_type = "Consent form type II",
                      consent_scenario = "Consent scenario",
                      consent_parental = "Consent: parental consent given",
                      consent_survey = "Consent: for survey",
                      consent_cogmot = "Consent: for cognitive and motor testing",
                      consent_hgtwgt = "Consent: for height and weight measurement",
                      consent_bioimp = "Consent: for bioimpedance measurement",
                      consent_sknthk = "Consent: for skin thickness measurement",
                      consent_bp = "Consent: for blood pressure measurement",
                      consent_bloods = "Consent: for blood sample for testing",
                      consent_bldst1 = "Consent: for blood sample for storage - non-genetic",
                      consent_bldst2 = "Consent: for blood sample for storage - genetic")

# create pseudo only version
denom_pseudo <- denom |> select(-upn,
                                -birth_date,
                                -postcode,
                                -school,
                                -school_establishment_no,
                                -form_tutor)

# create ID lookup
lkup <- denom |> select(aow_person_id, aow_recruitment_id, BiBPersonID, is_bib) |> unique()

# separate consent cols
consent_cols <- c("consent_form",
                  "consent_form_type",
                  "consent_scenario",
                  "consent_parental",
                  "consent_survey",
                  "consent_cogmot",
                  "consent_hgtwgt",
                  "consent_bioimp",
                  "consent_sknthk",
                  "consent_bp",
                  "consent_bloods",
                  "consent_bldst1",
                  "consent_bldst2"
)

denom_consent <- denom |> select(aow_person_id, aow_recruitment_id, BiBPersonID, is_bib,
                                 all_of(consent_cols))
denom <- denom |> select(-all_of(consent_cols))
denom_pseudo <- denom_pseudo |> select(-all_of(consent_cols))



# export
if(getOption("aow_export_denom")) {
  
  saveRDS(denom, file.path(output_path, "denom_identifiable.rds"))
  write_dta(denom, file.path(output_path, "denom_identifiable.dta"))
  write_csv(denom, file.path(output_path, "denom_identifiable.csv"), na = "")

  saveRDS(denom_pseudo, file.path(output_path, "denom_pseudo.rds"))
  write_dta(denom_pseudo, file.path(output_path, "denom_pseudo.dta"))
  write_csv(denom_pseudo, file.path(output_path, "denom_pseudo.csv"), na = "")

  saveRDS(denom_consent, file.path(output_path, "aow_consent.rds"))
  write_dta(denom_consent, file.path(output_path, "aow_consent.dta"))
  write_csv(denom_consent, file.path(output_path, "aow_consent.csv"), na = "")

  saveRDS(lkup, file.path(output_path, "id_lookup.rds"))
  write_dta(lkup, file.path(output_path, "id_lookup.dta"))
  write_csv(lkup, file.path(output_path, "id_lookup.csv"), na = "")

}
