
library(haven)
library(dplyr)
library(lubridate)
library(readr)

# whether to export files and summary reports
options(aow_export_denom = TRUE)

source("tools/aow_tools.R")

input_path <- "U:/Born in Bradford - AOW Raw Data/sql/denominator/data/"
output_path <- "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/denom/data/"


consent <- read_dta(file.path(input_path, "AOW_Consent.dta"))
schoolrec <- read_dta(file.path(input_path, "AOW_School_RecruitmentList.dta"))

# check duplicates
consent$AoWRecruitmentID[duplicated(consent$AoWRecruitmentID) |> which()]
schoolrec$AoWRecruitmentID[duplicated(schoolrec$AoWRecruitmentID) |> which()]

# can they be joined one-to-one
nrow(consent)
nrow(schoolrec)
denom_all <- consent |> inner_join(schoolrec, by = "AoWRecruitmentID", suffix = c("_con", "_rec"))
nrow(denom_all)

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


# start sorting fields to keep and renaming, add pseudo columns

denom <- denom_all |>
  transmute(aow_person_id = map_chr(UPN_con, aow_pseudo),
            upn = UPN_con,
            aow_recruitment_id = gsub("[^aowAOW0-9]", "", AoWRecruitmentID) |> tolower(),
            birth_date = DateOfBirth,
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
            ethnicity = Ethnicity,
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
            consent_bldst2 = ConBloodStor2,
            withdrawn = Withdrawn,
            withdrawal_date = WithdrawnDate)

# create pseudo only version
denom_pseudo <- denom |> select(-upn,
                                -birth_date,
                                -postcode,
                                -school,
                                -school_establishment_no,
                                -form_tutor)

# create ID lookup
lkup <- denom |> select(aow_person_id, aow_recruitment_id) |> unique()



# export
if(getOption("aow_export_denom")) {
  
  saveRDS(denom, file.path(output_path, "denom_identifiable.rds"))
  write_dta(denom, file.path(output_path, "denom_identifiable.dta"))
  write_csv(denom, file.path(output_path, "denom_identifiable.csv"), na = "")
  # html summary
  aow_df_summary(file.path(output_path, "denom_identifiable.rds"),
                 "Denominator with identifiers")
  
  saveRDS(denom_pseudo, file.path(output_path, "denom_pseudo.rds"))
  write_dta(denom_pseudo, file.path(output_path, "denom_pseudo.dta"))
  write_csv(denom_pseudo, file.path(output_path, "denom_pseudo.csv"), na = "")
  # html summary
  aow_df_summary(file.path(output_path, "denom_pseudo.rds"),
                 "Denominator without identifiers")
  
  saveRDS(lkup, file.path(output_path, "id_lookup.rds"))
  write_dta(lkup, file.path(output_path, "id_lookup.dta"))
  write_csv(lkup, file.path(output_path, "id_lookup.csv"), na = "")
  # html summary
  aow_df_summary(file.path(output_path, "id_lookup.rds"),
                 "Denominator lookup")
  
}
