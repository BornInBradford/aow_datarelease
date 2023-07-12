# check all surveys for length of variable names

srv1 <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module1_labelled.rds")
srv2 <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module2_labelled.rds")
srv3 <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module3_labelled.rds")
srv4 <- readRDS("U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module4_labelled.rds")

varnames <- c(names(srv2), names(srv2), names(srv3), names(srv4))

varlengths <- data.frame(name = varnames, length = nchar(varnames))

table(varlengths$length)

