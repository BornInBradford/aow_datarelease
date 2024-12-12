/*
	_________________________________________________________________________
	
	Programmer 		   			: 	Gillian Santorelli		

	Purpose Of Program 			: 	To clean the AoW year 9 bioimpedance data

	Date created				: 	17th July 2023
	
	Date amended				:	11th  July 2024
	
	Reason for amendment		:	To include data from academic year 2023/24
	
	Stata version				: 	17.0

	__________________________________________________________________________
*/

version 17

clear all

use "U:\Born in Bradford - AOW Raw Data\sql\bioimpedance\data\AOW_Person_Bioimpedance.dta", clear
count/* n=3,739 */

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id1 = lower(AoWRecruitmentID)

* Remove trailing blanks
gen aow_recruitment_id = strrtrim(aow_recruitment_id1)

drop AoWRecruitmentID aow_recruitment_id1
order aow_recruitment_id

* Check for recruitment id duplicates
bysort aow_recruitment_id: gen total = _N
tab total	/* all are unique */
drop total

* Merge with denominator 
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keepusing(gender birth_date aow_person_id BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id) keep(master matched) 
drop SEX FIRSTNAME LASTNAME BIRTHDATE PATNR FLAG

* Save the unmerged records
preserve
keep if _merge==1
export delimited using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bioimpedance_notlinked.csv", replace
restore

drop if _merge==1

* Generate a date variable from date/time 
gen date_measurement = dofc(DATETIME)
format date_measurement %td
order date_measurement, after(aow_recruitment_id)
drop if date_measurement==.
drop DATETIME

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
lab var ffm "Fat free mass (kg)"
rename PMM pmm
lab var pmm "Predicted muscle mass (kg)"
rename TBW tbw
lab var tbw "Total body water (%)"
rename IMP imp
lab var imp "Impedance (ohms)"
lab var aow_recruitment_id "Age of Wonder recruitment ID"

* Generate age variables
gen age_m = (date_measurement - birth_date) / 30.4375
replace age_m = floor(age_m)
rename age_y age_y
lab var date_measurement "Date of measurement"
lab var age_m "Age (months) at measurement"
lab var age_y "Age (years) at measurement"

* Tidy dataset
order aow_person_id BiBPersonID is_bib aow_recruitment_id recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id date_measurement age_m age_y model
drop birth_date

* Check measurements	
sum height weight bmi fatp fatm pmm ffm tbw imp, det

scatter height weight
list aow_recruitment_id height weight bmi fatp fatm pmm ffm if height<150 & (weight>85 & weight<.)

scatter bmi fatp
scatter bmi fatp, mlabel(aow_recruitment_id)

/* three outliers:
aow1128552
aow1053511
aow1149384
 */
list aow_recruitment_id height weight bmi fatp fatm pmm ffm if aow_recruitment_id=="aow1128552" | aow_recruitment_id=="aow1053511" | aow_recruitment_id=="aow1149384" 

/* none of these look right; the fatp/patm/ppm/ffm are disproportionate to the BMI so I'm going to drop them */
drop if aow_recruitment_id=="aow1128552" | aow_recruitment_id=="aow1053511" | aow_recruitment_id=="aow1149384" 

drop _merge
compress

* Final check
graph matrix height weight bmi fatp fatm pmm ffm tbw imp

scatter fatm pmm, mlabel(aow_recruitment_id)
edit aow_recruitment_id height weight bmi fatp fatm pmm ffm tbw imp if aow_recruitment_id=="aow1038470"
edit aow_recruitment_id height weight bmi fatp fatm pmm ffm tbw imp if pmm>80

drop if aow_recruitment_id=="aow1149954" | aow_recruitment_id=="aow1038470"

order aow_recruitment_id aow_person_id BiBPersonID is_bib recruitment_era gender ethnicity_1 ethnicity_2 birth_year birth_month date_measurement - imp
drop age_recruitment_y - form_tutor_id

save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bioimpedance_20241212.dta", replace





