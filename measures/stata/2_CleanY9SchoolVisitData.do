/*
	_________________________________________________________________________
	
	Programmer 		   			: 	Gillian Santorelli		

	Purpose Of Program 			: 	To clean the AoW year 9 measurements data

	Date created				: 	13th July 2023
	
	Stata version				: 	17.0

	__________________________________________________________________________
*/

version 17

clear all

*------------------------------------------------------------------------------*
* Height and weight
*------------------------------------------------------------------------------*

use "U:\Born in Bradford - AOW Raw Data\redcap\measures\data\AoWYear9SchoolVisit.dta", clear

* Drop variables not required
keep date_time_collection hw_aow_id hw_height hw_weight

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id = lower(hw_aow_id)
drop hw_aow_id

/* Save a dataset of not linked
preserve
keep if _merge==1
keep aow_recruitment_id aow_person_id BiBPersonID date_time_collection hw_height hw_weight
count	/* n=78 */
export delimited using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_y9schoolvisit_notlinked.csv", replace
restore
keep if _merge==3
drop _merge
*/

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "YMD")
format date_measurement %td
drop date_time_collection strdate

* Drop if no height and weight measurements
drop if hw_height==. & hw_weight==.	/* n=1,014 */

* Check to see whether bioimpedance heights/weights are in this dataset

merge m:1 aow_recruitment_id date_measurement using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bioimpedance_20241121.dta", keepusing(height weight) nogen

* For matched variables, replace any measurements missing from Y9 measurements with those from bioimpedance
replace hw_height = height if height!=. & hw_height==.
replace hw_weight = weight if weight!=. & hw_weight==.

* Check values 
sum hw_height hw_weight, det
scatter hw_height hw_weight, mlabel(aow_recruitment_id)
list aow_recruitment_id if hw_weight>800 & hw_weight<.
edit aow_recruitment_id hw_height hw_weight height weight if aow_recruitment_id=="aow1203082"
replace hw_weight=51.2 if aow_recruitment_id=="aow1203082"
edit aow_recruitment_id hw_height hw_weight height weight if aow_recruitment_id=="aow1186105"
edit aow_recruitment_id hw_height hw_weight height weight if aow_recruitment_id=="aow1212273"
replace hw_height=. if aow_recruitment_id=="aow1212273"

* Some implausible values here. Check them against bioimpedance data

* Identify height differences between the datasets
gen heightdiff = abs(hw_height - height)
sum heightdiff
edit aow_recruitment_id date_measurement hw_height height hw_weight weight heightdiff if heightdiff>1 & heightdiff<.
/* Either height is feasible. For consistency, I will prioritise the bioimpedance measurements */ 
replace hw_height = height if heightdiff>1 & heightdiff<.
replace hw_weight = weight if heightdiff>1 & heightdiff<.

* Identify weight differences between the datasets
gen weightdiff = abs(hw_weight - weight)
sum weightdiff
edit aow_recruitment_id date_measurement hw_height height hw_weight weight weightdiff if weightdiff>1 & weightdiff<.
/* Prioritise the bioimpedance measurements */ 
replace hw_weight = weight if weightdiff>1 & weightdiff<.

* Drop bioimpedance data
drop height weight heightdiff weightdiff

* Generate and format BMI
gen bmi = hw_weight/hw_height^2 * 10000
replace bmi = round(bmi, 0.1)

* Rename variables 
rename hw_height height
rename hw_weight weight

* Re-check measurements	
sum height weight bmi, det

scatter height weight
* Remove outlier
drop if height<120

graph matrix height weight bmi

* Merge with denominator 
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keepusing(gender birth_date aow_person_id  BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id) keep(3)

* Generate age variables
gen age_m = (date_measurement - birth_date) / 30.4375
replace age_m = floor(age_m)
gen age_y = (date_measurement - birth_date) / 365.25
replace age_y = floor(age_y)
drop birth_date

* Label
lab var date_measurement "Date of measurement"
lab var age_m "Age (months) at measurement"
lab var age_y "Age (years) at measurement"
lab var bmi "BMI (kg/m2)"
lab var aow_recruitment_id "Age of Wonder recruitment ID"

* Order variables
order aow_person_id BiBPersonID is_bib aow_recruitment_id recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id date_measurement age_m age_y 

drop _merge

save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_heightweight_20241121.dta", replace




*------------------------------------------------------------------------------*
* Blood pressure
*------------------------------------------------------------------------------*

use "U:\Born in Bradford - AOW Raw Data\redcap\measures\data\AoWYear9SchoolVisit.dta", clear

* Drop variables not required
keep date_time_collection hw_aow_id bp_sys_1 bp_dia_1 bp_sys_2 bp_dia_2

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id = lower(hw_aow_id)
drop hw_aow_id

* Merge with denominator to get sex and dob
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keepusing(gender birth_date aow_person_id BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id) nogen keep(matched)

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "YMD")
format date_measurement %td
drop date_time_collection strdate

* Generate age variables
gen age_m = (date_measurement - birth_date) / 30.4375
replace age_m = floor(age_m)
gen age_y = (date_measurement - birth_date) / 365.25
replace age_y = floor(age_y)
drop birth_date

* Drop if no bp measurements
drop if bp_sys_1==. & bp_dia_1==.
count	/* n=5,148 */

* Order variables
order aow_person_id BiBPersonID is_bib aow_recruitment_id recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id date_measurement age_m age_y 

* Label
rename gender sex
lab var date_measurement "Date of measurement"
lab var age_m "Age (months) at measurement"
lab var age_y "Age (years) at measurement"
lab var aow_recruitment_id "Age of Wonder recruitment ID"


**** Identify implausible values and export data for checking
*preserve
*keep aow_recruitment_id date_measurement age_y - bp_dia_2

*** Summary stats systolic
sum bp_sys_1 bp_sys_2, det	
scatter bp_sys_1 bp_sys_2

* There are still some huge differences between the two readings here, I will set them to missing if diff>50
gen sbpdiff = abs(bp_sys_1 - bp_sys_2)
replace bp_sys_1=. if sbpdiff>50 & sbpdiff<.
replace bp_sys_2=. if sbpdiff>50 & sbpdiff<.
scatter bp_sys_1 bp_sys_2

*** Summary stats diastolic
sum bp_dia_1 bp_dia_2, det	

scatter bp_dia_1 bp_dia_2

* There are still some huge differences between the two readings here, I will set them to missing if diff>50
gen dbpdiff = abs(bp_dia_1 - bp_dia_2)
replace bp_dia_1=. if dbpdiff>50 & dbpdiff<.
replace bp_dia_2=. if dbpdiff>50 & dbpdiff<.

/* Generate a flag if <1st or >99th percentile
gen sbp_lowhi_percentile_flag = .
replace sbp_lowhi_percentile=1 if (bp_sys_1<93 | bp_sys_2<93) | ((bp_sys_1>163 & bp_sys_1<.) | (bp_sys_2>163 & bp_sys_2<.))

gen dbp_lowhi_percentile_flag = .
replace dbp_lowhi_percentile=1 if (bp_dia_1<54 | bp_dia_2<54) | ((bp_dia_1>119  & bp_dia_1<.) | (bp_dia_2>119 & bp_dia_2<.))

order bp_sys_1 bp_sys_2 sbp_lowhi_percentile sbpdiff_abs bp_dia_1 bp_dia_2 dbp_lowhi_percentile dbpdiff_abs, after(age_y)

* Flag if difference between readings is >20
gen sbpdiff_flag=.
replace sbpdiff_flag=1 if sbpdiff_abs>20 & sbpdiff_abs<.

gen dbpdiff_flag=.
replace dbpdiff_flag=1 if dbpdiff_abs>20 &  dbpdiff_abs<.

* keep flagged 
keep if sbp_lowhi_percentile_flag==1 | dbp_lowhi_percentile_flag==1 | sbpdiff_flag==1 | dbpdiff_flag==1 
count	/* n=566. This is a lot! */

/* Drop high / low readings as long as the difference is <20 */
drop if (sbp_lowhi_percentile_flag==1 & sbpdiff_abs<20) & (dbp_lowhi_percentile_flag==1 & dbpdiff_abs<20)
count	/* n=2 */

/* Drop readings where the difference is <20 */
drop if sbpdiff_abs<20 & dbpdiff_abs<20
count	/* n=482 */

export delimited using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\bp_checks.csv", replace
restore
*/

* SBP and DBP
scatter bp_sys_1 bp_dia_1
scatter bp_sys_2 bp_dia_2
drop *diff

* Save
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bp_20241121.dta", replace



*------------------------------------------------------------------------------*
* Skin folds
*------------------------------------------------------------------------------*

use "U:\Born in Bradford - AOW Raw Data\redcap\measures\data\AoWYear9SchoolVisit.dta", clear

* Drop variables not required
keep date_time_collection hw_aow_id sk_tricep sk_subscap

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id = lower(hw_aow_id)
drop hw_aow_id

* Merge with denominator to get sex and dob
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keepusing(gender birth_date aow_person_id BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id) nogen keep(matched)

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "YMD")
format date_measurement %td
drop date_time_collection strdate

* Generate age variables
gen age_m = (date_measurement - birth_date) / 30.4375
replace age_m = floor(age_m)
gen age_y = (date_measurement - birth_date) / 365.25
replace age_y = floor(age_y)
drop birth_date

* Drop if no skin fold measurements
drop if sk_tricep==. & sk_subscap==.

* Order variables
order aow_person_id BiBPersonID is_bib aow_recruitment_id recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id date_measurement age_m age_y 

* Label
rename gender sex
lab var date_measurement "Date of measurement"
lab var age_m "Age (months) at measurement"
lab var age_y "Age (years) at measurement"
lab var aow_recruitment_id "Age of Wonder recruitment ID"
lab var sk_tricep "Triceps skinfold (mm)"
lab var sk_subscap "Subscapular skinfold (mm)"

* Summary stats
sum sk*, det	
scatter sk_tricep sk_subscap
/* all look plausible */

* Save
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_sk_20241121.dta", replace







