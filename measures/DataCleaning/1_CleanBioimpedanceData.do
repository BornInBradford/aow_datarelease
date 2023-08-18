/*
	_________________________________________________________________________
	
	Programmer 		   			: 	Gillian Santorelli		

	Purpose Of Program 			: 	To clean the AoW year 9 bioimpedance data

	Date created				: 	17th July 2023
	
	Stata version				: 	17.0

	__________________________________________________________________________
*/

version 17

clear all

use "U:\Born in Bradford - AOW Raw Data\sql\bioimpedance\data\AOW_Person_Bioimpedance.dta", clear

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id1 = lower(AoWRecruitmentID)

* Remove trailing blanks
gen aow_recruitment_id = strrtrim(aow_recruitment_id1)

drop AoWRecruitmentID aow_recruitment_id1
order aow_recruitment_id

* Merge with denominator 
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keepusing(gender birth_date aow_person_id  BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month year_group year_group) keep(master matched) 
drop SEX FIRSTNAME LASTNAME BIRTHDATE PATNR FLAG

* Generate a date variable from date/time 
gen date_measurement = dofc(DATETIME)
format date_measurement %td
order date_measurement, after(aow_recruitment_id)
drop if date_measurement==.
drop DATETIME

* Change year-group from string to numeric
gen tmpyear_group=real(year_group)
drop year_group
rename tmpyear_group year_group

* Generate recruitment month and year vars
gen tmprecruitment_year=substr(recruitment_era, 1, 4)
gen recruitment_y=real(tmprecruitment_year)
gen tmprecruitment_month=substr(recruitment_era, 6, 2)
gen recruitment_m=real(tmprecruitment_month)
drop recruitment_era tmprecruitment_year tmprecruitment_month 
lab var recruitment_m "Month of recruitment"
lab var recruitment_y "Year of recruitment"

* Label / rename variables
rename MODEL model
lab var model "Bioimpedance scale model"
rename AGE age_y
rename HEIGHT height
lab var height "Height (cm)"
rename WEIGHT weight
lab var weight "Weight (kg)"
rename CLOTHES clothes
lab var clothes "Estimated weight of clothes (kg)"
rename BMI bmi
lab var bmi "BMI (kg/m2)"
rename FATP fatp
lab var fatp "Total fat %"
rename FATM fatm
lab var fatm "Fat mass (kg)"
rename FFM ffm
lab var ffm "Fat free mass"
rename PMM pmm
lab var pmm "Predicted muscle mass"
rename TBW tbw
lab var tbw "Total body water (kg)"
rename IMP imp
lab var imp "Impedence"
lab var aow_recruitment_id "AoW recruitment ID"
lab var year_group "Academic year"

* Generate age variables
gen age_m = (date_measurement - birth_date) / 30.4375
replace age_m = floor(age_m)
rename age_y age_y
lab var date_measurement "Date of measurement"
lab var age_m "Age (months) at measurement"
lab var age_y "Age (years) at measurement"

* Tidy dataset
order aow_person_id BiBPersonID is_bib aow_recruitment_id recruitment_m recruitment_y age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month year_group year_group date_measurement age_m age_y model
drop birth_date

* Check measurements	
sum height weight bmi fatp fatm pmm ffm tbw imp, det

* Save a dataset of not linked
preserve
keep if _merge==1
keep aow_recruitment_id date_measurement age_y height - imp
export delimited using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bioimpedance_notlinked.csv", replace
restore

* Save a dataset excluding those not linked
keep if _merge==3
drop _merge
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bioimpedance.dta", replace




*------------------------------------------------------------------------------*
* Blood pressure
*------------------------------------------------------------------------------*

use "U:\Born in Bradford - AOW Raw Data\redcap\measures\data\AoWYear9SchoolVisit.dta", clear

* Drop variables not required
keep date_time_collection bp_aow_id bp_sys_1 bp_dia_1 bp_sys_2 bp_dia_2

* Remove * from aow id
gen id=substr(bp_aow_id, 2, 10)

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id = lower(id)
drop bp_aow_id id

* Merge with denominator
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keepusing(gender birth_date aow_person_id  BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month year_group year_group) nogen keep(matched)

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "DMY")
format date_measurement %td
drop date_time_collection strdate

* Change year-group from string to numeric
gen tmpyear_group=real(year_group)
drop year_group
rename tmpyear_group year_group

* Generate recruitment month and year vars
gen tmprecruitment_year=substr(recruitment_era, 1, 4)
gen recruitment_y=real(tmprecruitment_year)
gen tmprecruitment_month=substr(recruitment_era, 6, 2)
gen recruitment_m=real(tmprecruitment_month)
drop recruitment_era tmprecruitment_year tmprecruitment_month 
lab var recruitment_m "Month of recruitment"
lab var recruitment_y "Year of recruitment"

* Generate age variables
gen age_m = (date_measurement - birth_date) / 30.4375
replace age_m = floor(age_m)
gen age_y = (date_measurement - birth_date) / 365.25
replace age_y = floor(age_y)

* Drop if no bp measurements
drop if bp_sys_1==. & bp_dia_1==.

* Order variables
order aow_person_id BiBPersonID is_bib aow_recruitment_id recruitment_m recruitment_y age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month year_group year_group date_measurement age_m age_y 

* Label
rename gender sex
lab var date_measurement "Date of measurement"
lab var age_m "Age (months) at measurement"
lab var age_y "Age (years) at measurement"
lab var aow_recruitment_id "AoW recruitment ID"

* Summary stats
sum bp*, det	/* all look plausible */

drop birth_date

* Save
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bp.dta", replace



*------------------------------------------------------------------------------*
* Skin folds
*------------------------------------------------------------------------------*

use "U:\Born in Bradford - AOW Raw Data\redcap\measures\data\AoWYear9SchoolVisit.dta", clear

* Drop variables not required
keep date_time_collection sk_aow_id sk_tricep sk_subscap

* Remove * from aow id
gen id=substr(sk_aow_id, 2, 10)

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id = lower(id)
drop sk_aow_id id

* Merge with denominator
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keepusing(gender birth_date aow_person_id  BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month year_group year_group) nogen keep(matched)

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "DMY")
format date_measurement %td
drop date_time_collection strdate

* Generate age variables
gen age_m = (date_measurement - birth_date) / 30.4375
replace age_m = floor(age_m)
gen age_y = (date_measurement - birth_date) / 365.25
replace age_y = floor(age_y)

* Change year-group from string to numeric
gen tmpyear_group=real(year_group)
drop year_group
rename tmpyear_group year_group

* Generate recruitment month and year vars
gen tmprecruitment_year=substr(recruitment_era, 1, 4)
gen recruitment_y=real(tmprecruitment_year)
gen tmprecruitment_month=substr(recruitment_era, 6, 2)
gen recruitment_m=real(tmprecruitment_month)
drop recruitment_era tmprecruitment_year tmprecruitment_month 
lab var recruitment_m "Month of recruitment"
lab var recruitment_y "Year of recruitment"

* Drop if no skin fold measurements
drop if sk_tricep==. & sk_subscap==.

* Order variables
order aow_person_id BiBPersonID is_bib aow_recruitment_id recruitment_m recruitment_y age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month year_group year_group date_measurement age_m age_y 
drop birth_date

* Label
rename gender sex
lab var date_measurement "Date of measurement"
lab var age_m "Age (months) at measurement"
lab var age_y "Age (years) at measurement"
lab var aow_recruitment_id "AoW recruitment ID"

* Summary stats
sum sk*, det	/* all look plausible */

* Save
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_sk.dta", replace







