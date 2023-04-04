
library(haven)
library(dplyr)

input_path <- "U:/Born in Bradford - AOW Raw Data/sql/denominator/data/"

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
conflicts <- function(x, y) {
  con <- which(x != y)
  output <- paste0(x[con], " : ", y[con])
  return(output)
}

conflicts(denom_all$EstablishmentNumber_con, denom_all$EstablishmentNumber_rec) |> head()
conflicts(denom_all$School_con, denom_all$School_rec) |> head()
conflicts(denom_all$YearGroup_con, denom_all$YearGroup_rec) |> head()
conflicts(denom_all$FormTutor_con, denom_all$FormTutor_rec) |> head()
conflicts(denom_all$CreatedDateTime_con, denom_all$CreatedDateTime_rec) |> head()
conflicts(denom_all$ModifiedDateTime_con, denom_all$ModifiedDateTime_rec) |> head()



