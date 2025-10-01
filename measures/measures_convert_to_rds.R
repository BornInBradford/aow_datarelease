library(haven)

data_path <- "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/measures/data"
bloods_path <- "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/bloods/data"

bio <- read_dta(file.path(data_path, "aow_bioimpedance.dta"))
bp <- read_dta(file.path(data_path, "aow_bp.dta"))
htwt <- read_dta(file.path(data_path, "aow_heightweight.dta"))
sk <- read_dta(file.path(data_path, "aow_sk.dta"))
bld <- read_dta(file.path(bloods_path, "Bloods.dta"))

saveRDS(bio, file.path(data_path, "aow_bioimpedance.rds"))
saveRDS(bp, file.path(data_path, "aow_bp.rds"))
saveRDS(htwt, file.path(data_path, "aow_heightweight.rds"))
saveRDS(sk, file.path(data_path, "aow_sk.rds"))
saveRDS(bld, file.path(bloods_path, "aow_bloods.rds"))
