
library(haven)

input_path <- "U:/Born in Bradford - AOW Raw Data/sql/denominator/data/"

consent <- read_dta(file.path(input_path, "AOW_Consent.dta"))
schoolrec <- read_dta(file.path(input_path, "AOW_School_RecruitmentList.dta"))

