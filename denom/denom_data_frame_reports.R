
source("tools/aow_tools.R")


input_path <- "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/denom/data/"

aow_df_summary(file.path(input_path, "denom_identifiable.rds"),
               "Denominator with identifiers")

aow_df_summary(file.path(input_path, "denom_pseudo.rds"),
               "Denominator without identifiers")

aow_df_summary(file.path(input_path, "aow_consent.rds"),
               "Age of Wonder consent")

aow_df_summary(file.path(input_path, "id_lookup.rds"),
               "Denominator lookup")

